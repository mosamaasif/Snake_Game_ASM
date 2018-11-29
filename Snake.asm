[org 0x100]

	jmp main

;------------------------------
note:
	push bp
	mov bp, sp
	pusha

	mov al, 182
	out 43h, al
	mov ax, [bp + 4]

	out 42h, al
	mov al, ah
	out 42h, al
	in al, 61h

	or al, 3
	out 61h, al

	popa
	pop bp
	ret 2
;------------------------------
;	BaoJi's stuff

printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di

	mov ax, 0xb800
	mov es, ax 
	mov ax, [bp+4] 
	mov bx, 10 
	mov cx, 0 

nextdigit:
	mov dx, 0 
	div bx 
	add dl, 0x30 
	push dx 
	inc cx 
	cmp ax, 0 
	jnz nextdigit 
	mov di, 0 

nextpos:
	pop dx
	mov dh, 0x07 
	mov [es:di], dx 
	add di, 2 
	loop nextpos

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2

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

clock:
	pusha

	mov ax, [second]
	push ax
	call printnum

	mov al, [note_flag + 2]
	cmp byte[note_flag + 1], al
	jb __secCheck

	mov byte[note_flag], 0
	mov byte[note_flag + 1], 0
	mov byte[note_flag + 2], 0
	in al, 61h
	and al, 11111100b
	out 61h, al

	__secCheck:
		cmp byte[note_flag], 1
		jne __nextC
		inc byte[note_flag + 1]

		__nextC:
			add word [milliSecond], 55
			cmp word [milliSecond], 1000 ; one second
			jb __endClock

	mov word [milliSecond], 0
	add word [second], 1		

	cmp word [second], 60 ; one minute
	jb __endClock

	mov word [second], 0
	add word [minute], 1

	__endClock:
		popa
		iret

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

getPixel:
	push bp
	mov bp, sp

	push bx
	push cx
	push di
	push es

	mov cx, 0xb800
	mov es, cx

	mov cx, [bp + 6] ; row
	mov bx, 160
	mul bx

	mov bx, [bp + 4] ; col
	shl bx, 1
	add cx, bx

	mov di, cx
	mov ax, [es:di]

	pop es
	pop di
	pop cx
	pop bx

	pop bp
	ret 4



;----------------------------
	
rand:
	push bp
	mov bp, sp 

	push bx
	push cx
	push dx

    mov ax, 0  
	int 0x1A      

	mov  ax, dx
	mov  dx, 0
 	mov  cx, [bp + 4]    
	div  cx 

	mov ax, dx

	pop dx
	pop cx
	pop bx 

	pop bp
	ret 2

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

	cmp word [DOWN], 1
	je __skip

	mov word [UP], 	  1
	mov word [DOWN],  0
	mov word [LEFT],  0
	mov word [RIGHT], 0

__checkLeft:
	cmp ah, 0x4B
	jne __checkRight

	cmp word [RIGHT], 1
	je __skip

	mov word [UP],    0
	mov word [DOWN],  0
	mov word [LEFT],  1
	mov word [RIGHT], 0

__checkRight:
	cmp ah, 0x4D
	jne __checkDown

	cmp word [LEFT], 1
	je __skip

	mov word [UP],    0
	mov word [DOWN],  0
	mov word [LEFT],  0
	mov word [RIGHT], 1

__checkDown:
	cmp ah, 0x50
	jne __skip

	cmp word [UP], 1
	je __skip

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
	
fruitUpdate:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si

	mov dx, [boundryX1Y1]
	mov bx, [boundryX2Y2]

	mov cx, [fruitPos]
	mov ax, cx

	mov cl, ch
	mov ch, 0
	mov ah, 0

	push 0x0720
	push cx
	push ax
	call drawPixel

__findFruitPos:

	mov cx, 0
	mov cl, bh
	sub cl, dh 
	sub cl, 3 ; just to be sure fruit doesn't get outside the boundary

	mov ax, 0
	push cx ; row
	call rand ; returns row between 0 to X2 - X1

	add al, dh
	add al, 3 ; just to be sure fruit doesn't get outside the boundary
	mov si, ax

	mov cl, bl
	sub cl, dl
	sub cl, 2 ; just to be sure fruit doesn't get outside the boundary

	mov ax, 0

	push cx
	call rand

	add al, dl
	add al, 2 ; just to be sure fruit doesn't get outside the boundary
	mov cx, si
	mov ah, cl ; packs Row into AH and Col into AL

	mov si, 0
	mov cx, [snakeSize]

__checkFruit:
	cmp word [snake + si], ax
	je __findFruitPos

	add si, 2
	dec cx
	cmp cx, 0
	jnz __checkFruit

	mov bx, [boundryX1Y1]
	mov cx, [boundryX2Y2]

	mov [fruitPos], ax

	pop si 
	pop dx 
	pop cx
	pop bx 
	pop ax

	pop bp
	ret

;----------------------------

drawFruit:
	push bp
	mov bp, sp 

	push ax
	push bx

	mov ax, [fruitPos]

	mov bx, 0
	mov bl, ah
	mov ah, 0

	push 0x2020
	push bx
	push ax
	call drawPixel

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


	mov ax, [snakeSize]	
	mov bx, snake

	dec ax
	shl ax, 1
	add bx, ax
	mov ax, [fruitPos]

	cmp ax, [bx] ; snake's head
	jne __end

	add word [interpolate], 4

	mov byte[note_flag], 1
	mov byte[note_flag + 1], 0
	mov byte[note_flag + 2], 6

	push 4560	;middle C
	call note
	call fruitUpdate

__end:
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
	
	push es
	push 0
	pop es

	mov word [es:0x1C * 0x4], clock
	mov word [es:0x1C * 0x4 + 0x2], cs 

	pop es

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
	call fruitUpdate
	
	push bx
	mov bx, 3
	__outerLoop:
		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 6

		push 1140	;middle C
		call note
		__innerLoop:
			cmp byte[note_flag], 1
			je __innerLoop
			mov ecx, 1000000000
			__delay:
				loop __delay
			dec bx
			jnz __outerLoop


infinite:
	
	cmp word[interpolate], 0
	je __noInterp

	mov ax, [snakeSize]	
	mov bx, snake

	dec ax
	shl ax, 1
	add bx, ax

	mov ax, [bx]
	mov [bx + 2], ax
	add word [snakeSize], 1
	sub word [interpolate], 1
__noInterp:

	call update
	call checkCollision
	call drawFruit
	call drawSnake
	

	mov ah, 0x86
	mov cx, 1
	mov dx, 0xFFFF
	int 0x15

	jmp infinite
exit:
	mov byte[note_flag], 1
	mov byte[note_flag + 1], 0
	mov byte[note_flag + 2], 4

	push 9121	;middle C
	call note
	__deathLoop:
		cmp byte[note_flag], 1
		je __deathLoop

	mov ax, 0x4c00
	int 0x21

;----------------------------
;	Defines	

boundryX1Y1: dw 0x0201 ; AH = Row, AL = Column
boundryX2Y2: dw 0x174E ; AH = Row, AL = Column

fruitPos: 	dw 0 ; AH = Row, AL = Column

milliSecond: dw 0
second: dw 0
minute: dw 0

interpolate: dw 0

speed:		dw 1

UP:			dw 0
DOWN:		dw 0
LEFT:		dw 0
RIGHT:		dw 0

snakeSize:	dw 20
maxSize:	dw 240
snake:		times 240 dw 0 ; AH = Rows, AL = Columns

note_flag: times 3 db 0
