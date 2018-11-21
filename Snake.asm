[org 0x100]

	jmp main

;-------------------------
;	Utility Functions

clrscr:
	push bp
	mov bp, sp

	push ax
	push cx
	push di
	push es

	mov di, 0

	mov ax, 0xb800
	mov es, ax

	mov ax, 0x0720
	mov cx, 2000

	repe stosw

	pop es
	pop di
	pop cx
	pop ax

	pop bp
	ret
;----------------------------

getKey:
	push bp
	mov bp, sp

	mov ah, 0x01                      
	int   0x16                           
	jz    __nodata                     

	mov ah, 0                
	int 0x16
	jmp __checkExit
__nodata:
	mov ax, 0
__checkExit:
	pop bp
	ret
;----------------------------

drawBoundry:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es

	mov cx, 1

__loop:
	push 0x3820
	push 2
	push cx
	call drawChar

	push 0x3820
	push 23
	push cx
	call drawChar

	inc cx
	cmp cx, 78
	jnz __loop

	mov cx, 2

__loop1:
	push 0x3820
	push cx
	push 1
	call drawChar

	push 0x3820
	push cx
	push 78
	call drawChar

	inc cx
	cmp cx, 24
	jnz __loop1

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax

	pop bp
	ret
;----------------------------

drawChar:
	push bp
	mov bp, sp

	push ax
	push bx
	push di
	push es

	mov ax, 0xb800
	mov es, ax

	mov ax, [bp + 6] ; col
	mov bx, 160
	mul bx

	mov bx, [bp + 4] ; row
	shl bx, 1
	add ax, bx

	mov di, ax
	mov ax, [bp + 8] ; color + char
	mov [es:di], ax

	pop es
	pop di
	pop bx
	pop ax

	pop bp
	ret 6
;----------------------------

drawSnake:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov cx, [snakeSize]
	mov bx, snake

__loop2:
	mov dx, [bx]
	mov ax, dx

	mov ah, 0
	mov dl, dh
	mov dh, 0

	push 0x6820
	push dx
	push ax
	call drawChar

	add bx, 2

	dec cx
	cmp cx, 0
	jnz __loop2

	pop dx
	pop cx
	pop bx
	pop ax

	pop bp
	ret
;----------------------------

getPosition:
	push bp
	mov bp, sp

	push ax

	;mov ax, 0
	;int 0x16

	call getKey 

	cmp ah, 0x48
	jne __checkLeft

	mov word [UP], 	  1
	mov word [DOWN],  0
	mov word [LEFT],  0
	mov word [RIGHT], 0

__checkLeft:
	cmp ah, 0x4B
	jne __checkRight

	mov word [UP],    0
	mov word [DOWN],  0
	mov word [LEFT],  1
	mov word [RIGHT], 0

__checkRight:
	cmp ah, 0x4D
	jne __checkDown

	mov word [UP],    0
	mov word [DOWN],  0
	mov word [LEFT],  0
	mov word [RIGHT], 1

__checkDown:
	cmp ah, 0x50
	jne __skip

	mov word [UP],    0
	mov word [DOWN],  1
	mov word [LEFT],  0
	mov word [RIGHT], 0

__skip:
	
	pop ax

	pop bp
	ret
;----------------------------

moveLeft:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx

	mov bx, snake
	mov cx, [snakeSize]
	dec cx

_loopLeft:
	mov ax, [bx + 2]
	mov [bx], ax

	add bx, 2

	dec cx
	cmp cx, 0
	jnz _loopLeft

	pop cx
	pop bx
	pop ax

	pop bp
	ret
;----------------------------

;----------------------------
;	Game Functions

update:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	
	mov bx, snake

	mov dx, [bx]
	mov ax, dx

	mov ah, 0
	mov dl, dh
	mov dh, 0

	push 0x0720
	push dx
	push ax
	call drawChar

	call moveLeft
	call getPosition

	mov ax, [snakeSize]	

	dec ax
	shl ax, 1
	add bx, ax

	mov ax, [bx]

	cmp word [UP], 1
	jne __down

	sub ah, [speed]

__down:
	cmp word [DOWN], 1
	jne __left

	add ah, [speed]

__left:
	cmp word [LEFT], 1
	jne __right

	sub al, [speed]

__right:
	cmp word [RIGHT], 1
	jne __doNothing

	add al, [speed]

__doNothing:
	mov [bx], ax

	pop dx
	pop cx
	pop bx
	pop ax

	pop bp
	ret

;----------------------------
;	Main Function

main:
	mov bx, snake
	mov cx, [snakeSize]
	mov ah, 12
	mov al, 28
l1:	
	mov [bx], ax
	inc al

	add bx, 2

	dec cx
	cmp cx, 0
	jnz l1
	
	call clrscr
	call drawBoundry
	call drawSnake

infinite:

	call drawSnake
	call update

	mov ah, 0x86
	mov cx, 1
	int 0x15

	jmp infinite



	mov ax, 0x4c00
	int 0x21

;----------------------------
;	Defines	

snake:		times 256 dw 0 ; AH = Rows, AL = Columns
snakeSize:	dw 20

speed:		dw 1

UP:		dw 0
DOWN:		dw 0
LEFT:		dw 0
RIGHT:		dw 0
