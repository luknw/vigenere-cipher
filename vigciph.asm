data segment

fname			db 'tmp.txt',0d

str_usage		db 'vigciph [-d] INPUT OUTPUT KEY',13d,10d
				db 'INPUT = valid DOS filename',13d,10d
				db 'OUTPUT = valid DOS filename, encrypted INPUT',13d,10d
				db 'KEY = [0-9A-Za-z]*',13d,10d,'$'

err_file		db 'error: file operation failed',13d,10d,13d,10d,'$'

data ends


code segment

debug macro CHAR
	push ax
	push dx

	mov ah,02h
	mov dl,CHAR
	int 21h

	mov ah,00h
	int 16h

	pop dx
	pop ax
endm

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


;ENDL
;Prints cr lf.
ENDL macro
	push ax
	push dx

	mov ah,02h
	mov dl,0dh
	int 21h
	mov dl,0ah
	int 21h

	pop dx
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



main:
	mov ax,stack
	mov ss,ax
	mov sp,offset top

	mov ax,data
	mov ds,ax

	mov ax,3d02h				;open file
	mov dx,offset fname
	int 21h

	jc file_error
	debug 'a'

	mov dx,ax

	mov ah,3eh					;close file
	mov bx,dx
	int 21h

	jc file_error
	debug 'b'

	mov ax,4c00h
	int 21h

file_error:
	ERROR_EXIT_STR -1, err_file
code ends

stack segment stack

	db 1024 dup(?)
top	db ?

stack ends

end main
