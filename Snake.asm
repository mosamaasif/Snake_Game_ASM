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

getAsyncKey:
	push bp
	mov bp, sp

	mov ah, 0x01                      
	int 0x16                           
	jz __nodata                     

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
	sub sp, 8

	push ax
	push bx
	push cx
	push dx
	push si
	push di
	push es

	mov dx, [boundryX1Y1] ; DH = Row, DL = Column

	mov cx, dx
	mov dl, dh
	mov dh, 0
	mov ch, 0


	mov [bp - 2], dx ; Row Y1 
	mov [bp - 4], cx ; Col X1

	mov dx, [boundryX2Y2] ; DH = Row, DL = Column

	mov cx, dx
	mov dl, dh
	mov dh, 0
	mov ch, 0

	mov [bp - 6], dx ; Row Y2
	mov [bp - 8], cx ; Col X2

	mov cx, [bp - 4] ; Col X1
	mov si, [bp - 2] ; Row Y1
	mov di, [bp - 8] ; Col X2
	mov ax, [bp - 6] ; Row Y2

__loop:
	push 0x3820
	push si
	push cx
	call drawPixel

	push 0x3820
	push ax
	push cx
	call drawPixel

	inc cx
	cmp cx, di
	jnz __loop

	mov cx, si
	mov si, [bp - 4]
	inc ax
__loop1:
	push 0x3820
	push cx
	push si
	call drawPixel

	push 0x3820
	push cx
	push di
	call drawPixel

	inc cx
	cmp cx, ax
	jnz __loop1

	pop es
	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax

	mov sp, bp
	pop bp
	ret
;----------------------------

drawPixel:
	push bp
	mov bp, sp

	push ax
	push bx
	push di
	push es

	mov ax, 0xb800
	mov es, ax

	mov ax, [bp + 6] ; row
	mov bx, 160
	mul bx

	mov bx, [bp + 4] ; col
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
	call drawPixel

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

	call getAsyncKey 

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
checkCollision:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si
	push di

; checking collision with boundary

	mov ax, [snakeSize]	
	mov bx, snake

	dec ax
	shl ax, 1
	add bx, ax

	mov ax, [bx]
	mov cx, [boundryX1Y1]
	inc cl
	inc ch

	cmp al, cl
	jb exit

	cmp ah, ch
	jb exit

	mov cx, [boundryX2Y2]
	dec cl
	dec ch

	cmp al, cl
	ja exit

	cmp ah, ch
	ja exit

; checking collision with snake's body

	mov cx, [snakeSize]
	mov bx, snake
	dec cx
	mov si, 0

__collision:
	cmp ax, [bx + si]
	je exit

	add si, 2
	dec cx
	cmp cx, 0
	jnz __collision

	pop di
	pop si
	pop dx
	pop cx
	pop bx
	pop ax

	pop bp
	ret
;----------------------------
;	Game Functions

update:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si

	mov si, [speed]

__speedLoop:

	mov bx, snake

	mov dx, [bx]
	mov ax, dx

	mov ah, 0
	mov dl, dh
	mov dh, 0

	push 0x0720
	push dx
	push ax
	call drawPixel

	call moveLeft
	call getPosition

	mov ax, [snakeSize]	

	dec ax
	shl ax, 1
	add bx, ax

	mov ax, [bx]

	cmp word [UP], 1
	jne __down

	sub ah, 1

__down:
	cmp word [DOWN], 1
	jne __left

	add ah, 1

__left:
	cmp word [LEFT], 1
	jne __right

	sub al, 1

__right:
	cmp word [RIGHT], 1
	jne __doNothing

	add al, 1

__doNothing:
	mov [bx], ax
	
	dec si
	cmp si, 0
	jnz __speedLoop

	pop si
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

	mov word [RIGHT], 1
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
	
	call update
	call checkCollision
	call drawSnake
	
	

	mov ah, 0x86
	mov dx, 0xFFFF
	int 0x15

	jmp infinite
exit:
	mov ax, 0x4c00
	int 0x21

;----------------------------
;	Defines	

snake:		times 240 dw 0 ; AH = Rows, AL = Columns
snakeSize:	dw 20
maxSize:	dw 240

boundryX1Y1: dw 0x0201 ; AH = Row, AL = Columns
boundryX2Y2: dw 0x174E ; AH = Row, AL = Columns

speed:		dw 1

UP:			dw 0
DOWN:		dw 0
LEFT:		dw 0
RIGHT:		dw 0
