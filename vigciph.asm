data segment
;Argument storage format:
;storage		:byte[STO_SIZE]			- NULL-terminated arguments
;argc			:byte					- number of parsed arguments
;argptrs		:word[ARGNUM]			- offset of every argument relative to segment
;arglens		:byte[ARGNUM]			- table of arguments' lengths, excluding terminator
;
STO_SIZE = 255d
ARGNUM = STO_SIZE / 2d

storage				db STO_SIZE dup(?)
argc				db 0d
argptrs				dw ARGNUM dup(0d)
arglens				db ARGNUM dup(0d)


str_usage			db 'vigciph [-d] INPUT OUTPUT KEY',13d,10d
					db 'INPUT = valid DOS filename',13d,10d
					db 'OUTPUT = valid DOS filename',13d,10d
					db 'KEY = [0-9A-Za-z]*',13d,10d,'$'


err_overflow		db 'error: arguments too long',13d,10d,13d,10d,'$'
err_arg_count		db 'error: invalid number of arguments',13d,10d,13d,10d,'$'
err_unknown_option	db 'error: invalid option',13d,10d,13d,10d,'$'
err_key_value		db 'error: invalid key value',13d,10d,13d,10d,'$'
err_file			db 'error: invalid file operation',13d,10d,13d,10d,'$'


decode				db 0d
input_name			db 128d dup(0d)
output_name			db 128d dup(0d)
key					db 128d dup(0d)


input_handle		dw ?
output_handle		dw ?


BUFFER_SIZE			= 1024d
buffer				db BUFFER_SIZE dup(?)

data ends


;Storage API;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;LD_STO_SEG	(SEG_REG)
;LD_STO (SEG_REG, REG)
;
;IS_STO_FULL (REG)
;
;ARGCX
;
;ARGP (INDEX, REG)
;ARG (INDEX, REG)
;ARGV (INDEX, REG)
;
;ARGLENP (INDEX, REG)
;ARGLEN (INDEX, REG)
;
;COPY_ARG (SEG_SRC, INDEX, SEG_DEST, OFFSET_DEST)
;

LD_STO_SEG macro SEG_REG				;moves storage segment to SEG_REG
	push ax

	mov ax,seg storage
	mov SEG_REG,ax

	pop ax
endm

LD_STO macro SEG_REG, REG				;moves storage segment to SEG_REG and offset to REG
	LD_STO_SEG SEG_REG
	mov REG,offset storage
endm

IS_STO_FULL macro REG					;checks whether pointer REG is after the last byte of storage data; use: jae <=> true / jb <=> false
	cmp REG,offset argc
endm

ARGCX macro								;moves number of arguments in storage to cx
	push ds

	LD_STO_SEG ds

	mov cl,ds:[argc]
	xor ch,ch

	pop ds
endm

ARGP macro INDEX, REG					;returns in REG pointer to near pointer to argument with index INDEX, indices begin from 0
	xor REG,REG

	mov REG,INDEX						;skip pointers (2 bytes) of args before INDEX
	sal REG,1d

	add REG,offset argc + 1d
endm

ARG macro INDEX, REG					;returns in REG near pointer to argument with index INDEX, indices begin from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGP INDEX, REG

	mov bx,REG
	mov REG,ds:[bx]

	pop ds
	pop bx
endm

ARGV macro INDEX, REG					;returns in REG first word of argument with index INDEX, indices begin from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGP INDEX, REG

	mov bx,REG
	mov bx,ds:[bx]						;get pointer to argument

	mov bx,ds:[bx]						;get first word of argument
	mov REG,bx

	pop ds
	pop bx
endm

ARGLENP macro INDEX, REG				;returns in REG pointer to size of argument INDEX, indices start from 0
	xor REG,REG
	mov REG,INDEX
	add REG,offset arglens
endm

ARGLEN macro INDEX, REG					;returns in REG size of argument INDEX, indices start from 0
	push bx								;REG cannot be bx
	push ds

	LD_STO_SEG ds

	ARGLENP INDEX, REG

	mov bx,REG
	mov REG,ds:[bx]
	and REG,00ffh

	pop ds
	pop bx
endm

;COPY_ARG
;Copies argument with index INDEX from storage to SEG_DEST:OFFSET_DEST
;
COPY_ARG macro INDEX, SEG_DEST, OFFSET_DEST
	push ax
	push si
	push di
	push ds
	push es

	LD_STO_SEG ds
	ARG INDEX, si

	mov ax,SEG_DEST
	mov es,ax
	mov di,OFFSET_DEST

	ARGLEN INDEX, cx

	rep movsb
	
	pop es
	pop ds
	pop di
	pop si
	pop ax
endm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


code segment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LSTR
;Load address of string NAME to ds:dx
;
;params:	NAME		- name of the string to load
;
LSTR macro NAME
	push ax

	mov ax,seg NAME
	mov ds,ax
	mov dx,offset NAME

	pop ax
endm


;error_exit
;If bx isn't 0d, prints $-terminated error string at ds:[dx].
;Exits program with return code set in al.
;
;params:	al			- return code
;			bx			- print error string flag
;			ds:[dx]		- error string
;
ERROR_EXIT macro RET
	mov al,RET
	mov bx,0d
	call $error_exit
endm

ERROR_EXIT_STR macro RET, STR
	mov al,RET
	mov bx,1d
	LSTR STR

	call $error_exit
endm

$error_exit proc
	push ax						;to preserve al return code

	test bx,bx
	jz print_str_usage

	mov ah,09h					;print string at ds:[dx]
	int 21h

print_str_usage:
	LSTR str_usage
	mov ah,09h
	int 21h

	pop ax
	mov ah,4ch
	int 21h
$error_exit endp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;eat_whitespace
;Returns position of the first non-whitespace char in the string
;at ds:[si] and sets ah to 1. If enter (13d) found, sets ah to 0.
;
;params:	ds:[si]		- string to search in
;
;returns:	ds:[si]		- first non-whitespace char found in the string
;			ah = 0d		- enter found
;			ah = 1d		- non-whitespace found
;
eat_whitespace proc
	push ax

get_char:
	lodsb

	cmp al,0dh					;0dh = cr = enter
	je enter_handler

	cmp al,21h					;21h = first non-whitespace ascii char
	jb get_char

	cmp al,7fh					;check delete = 7fh
	je get_char

	dec si						;non-whitespace found
	pop ax
	mov ah,1d
	ret

enter_handler:
	pop ax
	mov ah,0d
	ret
eat_whitespace endp


;copy_arg_to_storage
;Copies string at ds:[si] to storage, beginning at es:[di].
;Updates argptr table and argc. Modifies si, di.
;
;params:	ds:[si]		- source
;			es:[di]		- destination inside a storage
;
;returns:	ds:[si]		- character in the string after the first found whitespace
;			ah = 0d		- enter found
;			ah = 1d		- whitespace found
;
copy_arg_to_storage proc
	push bx
	push dx
	push ax

	IS_STO_FULL di				;check free space
	jae error_overflow

	ARGP word ptr es:[argc], bx	;get pointer to pointer to argument with index argc (=counter of args in storage) to bx
	mov es:[bx],di				;save argument ptr

	movsb						;move first char from source to storage
	mov dx,1d					;dx stores length of current argument

get_char:
	lodsb						;load char from source

	cmp al,0dh					;0dh = cr = enter
	je enter_handler

	cmp al,21h					;21h = first non-whitespace ascii char
	jb whitespace_handler

	cmp al,7fh					;check delete = 7fh
	je whitespace_handler

	IS_STO_FULL di				;check free space
	jae error_overflow

	stosb						;move char from al to storage
	inc dx						;increment length

	jmp get_char

enter_handler:
	pop ax						;pop ax now to preserve return value in ah later
	mov ah,0d
	jmp end_arg

whitespace_handler:
	pop ax						;pop ax now to preserve return value in ah later
	mov ah,1d

end_arg:
	IS_STO_FULL di				;check free space
	jae error_overflow

	mov byte ptr es:[di],0d		;terminate argument
	inc di

	ARGLENP word ptr es:[argc], bx	;get pointer to length of argument with index argc
	mov es:[bx],dx				;save argument length

	inc es:[argc]				;increment number of arguments in storage

	pop dx
	pop bx
	ret

error_overflow:
	ERROR_EXIT_STR -1d, err_overflow	;print error and exit if overflow
copy_arg_to_storage endp


;parse_args
;Parses DOS cmdline arguments and saves them in storage.
;
parse_args proc
	push si
	push ds

	mov ah,51h					;get argument segment address from DOS to bx
	int 21h

	mov ds,bx					;DOS segment in bx
	mov si,80h					;80h -> DOS cmdline length

	LD_STO es, di				;init es:di with seg storage:offset storage

	cmp byte ptr ds:[si],0d		;check if there are any arguments
	je warn_no_arguments

	add si,2d					;move si to point at first possible non-whitespace

parse_argument:
	call eat_whitespace

	test ah,ah					;check end of input
	jz return

	call copy_arg_to_storage

	test ah,ah					;check end of input
	jnz parse_argument

return:
	pop ds
	pop si
	ret

warn_no_arguments:				;just print usage info and exit
	ERROR_EXIT 1d
parse_args endp


verify_decode proc
	push ax

	ARGLEN dx, ax				;get length of argument with index dx to ax

	cmp ax,2d
	jne error_unknown_option

	ARGV dx, ax					;get first word of argument with index dx to ax

	cmp	ah,'d'					;check if option is "d"
	je set_decode

error_unknown_option:
	ERROR_EXIT_STR -3d, err_unknown_option	;else: unknown option

set_decode:
	mov ds:[decode],1d
		
	pop ax
	ret
verify_decode endp


verify_key proc
	push ax
	push si

	ARGLEN dx, si				;check size of argument with index dx
	test si,si					;if it's 0 (argument isn't provided)
	jz cleanup					;leave default, jump to cleanup

	ARG dx, si					;point si at an argument with index dx

get_char:
	lodsb
	cmp al,0d
	je key_ok

	cmp al,'0'					;check below 0
	jb error_key_value

	cmp al,'9'					;check above 9
	ja not_digit

	jmp get_char	

not_digit:
	cmp al,'A'					;check below A
	jb error_key_value

	cmp al,'Z'					;check above Z
	ja not_capital

	jmp get_char

not_capital:
	cmp al,'a'					;check below a
	jb error_key_value

	cmp al,'z'					;check above z
	ja error_key_value

	jmp get_char

key_ok:
	COPY_ARG dx, seg key, offset key	;copy key from parser storage to dedicated key variable

cleanup:
	pop si
	pop ax
	ret

error_key_value:
	ERROR_EXIT_STR -4d, err_key_value
verify_key endp


;verify_args
;
;returns:	exits with negative value if cmdline arguments are not valid
;
verify_args proc
	push ax
	push dx
	push ds

	LD_STO_SEG ds				;load ds with storage segment
	
	mov al,ds:[argc]			;get number of arguments to al
	xor dx,dx					;dx is index of currently verified argument

	cmp al,2d					;two args?
	je copy_input				;first must be input

	cmp al,3d					;three args?
	je is_option				;verify flag first

	cmp al,4d
	je check_decode

	ERROR_EXIT_STR -2d, err_arg_count	;else: invalid number of arguments

is_option:
	ARGV dx, ax
	cmp al,'-'
	jne copy_input

check_decode:
	call verify_decode
	inc dx

copy_input:
	COPY_ARG dx, seg input_name, offset input_name
	inc dx

	COPY_ARG dx, seg output_name, offset output_name
	inc dx

check_key:
	call verify_key

	pop ds
	pop dx
	pop ax
	ret
verify_args endp


check_file_error proc
	jnc no_error
	ERROR_EXIT_STR -5d, err_file
no_error:
	ret
check_file_error endp


open_files proc
	push ax
	push cx
	push dx
	push ds
	
	LD_STO_SEG ds

	mov ax,3d00h				;open input file for reading
	mov dx,offset input_name
	int 21h
	call check_file_error

	mov ds:[input_handle],ax

	mov ah,3ch					;create tmp output file
	mov dx,offset output_name
	xor cx,cx
	int 21h
	call check_file_error

	mov ds:[output_handle],ax

	pop ds
	pop dx
	pop cx
	pop ax
	ret
open_files endp


close_files proc
	push ax
	push bx
	push ds

	LD_STO_SEG ds

	mov ah,3eh					;close input file
	mov bx,ds:[input_handle]
	int 21h
	call check_file_error

	mov bx,ds:[output_handle]	;close output file
	int 21h
	call check_file_error

	pop ds
	pop bx
	pop ax
	ret
close_files endp


;load_buffer
;Returns number of bytes read in cx.
;
load_buffer proc
	push ax
	push bx
	push dx
	
	mov ah,3fh
	mov bx,ds:[input_handle]
	mov cx,BUFFER_SIZE
	mov dx,offset buffer
	int 21h
	call check_file_error
	mov cx,ax

	pop dx
	pop bx
	pop ax
	ret
load_buffer endp


save_buffer proc				;numbers of bytes to write in cx
	push ax
	push bx
	push dx
	
	mov ah,40h
	mov bx,ds:[output_handle]
	mov dx,offset buffer
	int 21h
	call check_file_error
	cmp cx,ax
	je no_error
	ERROR_EXIT_STR -5d, err_file

no_error:
	pop dx
	pop bx
	pop ax
	ret
save_buffer endp


encrypt proc
	push ax
	push bx
	push cx
	push dx
	push si
	push ds
	
	LD_STO_SEG ds

	cmp ds:[decode],1d
	jne init_encryption

	mov bx,offset key
invert_char:
	neg byte ptr ds:[bx]		;ds:[bx] = 256 - ds:[bx]; two's complement negation
	inc bx
	cmp byte ptr ds:[bx],0d
	jne invert_char

init_encryption:
	mov bx,offset key

ld_buffer:
	call load_buffer
	mov si,offset buffer
	mov dx,cx					;save number of bytes read

encrypt_char:
	mov al,ds:[si]
	add al,byte ptr ds:[bx]

	inc bx
	cmp byte ptr ds:[bx],0d
	jne key_not_reset
	mov bx,offset key

key_not_reset:
	mov ds:[si],al
	inc si
	loop encrypt_char

	mov cx,dx
	call save_buffer

	cmp cx,BUFFER_SIZE
	je ld_buffer

	pop ds
	pop si
	pop dx
	pop cx
	pop bx
	pop ax
	ret
encrypt endp


main:
	mov ax,stack				;init stack segment
	mov ss,ax
	mov sp,offset top

	call parse_args

	call verify_args

	call open_files
	
	call encrypt

	call close_files

	mov ax,4c00h
	int 21h
code ends


stack segment stack

	db 1024 dup(?)
top	db ?

stack ends

end main
