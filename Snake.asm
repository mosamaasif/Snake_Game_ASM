[org 0x100]

	jmp main

;////////////////////////////////////////////
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
;////////////////////////////////////////////

;////////////////////////////////////////////
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
	mov ax, [bp + 4]	;number
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
		mov ax, [bp + 8]	;row
		mov bx, 160
		mul bx
		mov dx, [bp + 6]	;col
		shl dx, 1
		add ax, dx
		mov di, ax

	nextpos:
		pop dx
		mov ax, [bp + 10]
		mov dh, al 
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
	ret 8
;////////////////////////////////////////////

time_upd:
	pusha
	push es

	cmp byte[curr_time], 0
	je __tnext

	cmp word[curr_time + 1], 0
	je __tskip

	__tupd:
		dec word[curr_time + 1]

		push 0xB800
		pop es

		mov word[es:294], 0x0720
		mov word[es:296], 0x073A
		mov word[es:298], 0x0720
		mov word[es:300], 0x0720

		mov ax, 0x0007
		push ax
		push 1
		push 67
		mov ah, 0
		mov al, [curr_time]
		push ax
		call printnum

		cmp word[curr_time + 1], 10
		jb _tupd1
		
		mov ax, 0x0007
		push ax
		push 1
		push 69
		push word[curr_time + 1]
		call printnum
		jmp __tend

		_tupd1:
			mov word[es:298], 0x0730
			mov ax, 0x0007
			push ax
			push 1
			push 70
			push word[curr_time + 1]
			call printnum
			jmp __tend

	__tskip:
		mov word[curr_time + 1], 60
		dec byte[curr_time]
		jmp __tupd

	__tnext:
		cmp word[curr_time + 1], 0
		jne __tupd

		mov byte[curr_time], 4
		mov word[curr_time + 1], 0
		dec byte[life]
		cmp byte[life], 0
		je __tend
		call updateStat

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 4

		push 9121	;middle C
		call note
		call resetGame

	__tend:
		pop es
		popa
		ret
;////////////////////////////////////////////
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
;////////////////////////////////////////////

;////////////////////////////////////////////
clock:
	pusha

	mov ax, [second]

	cmp byte[poison], 1
	jne __rest1

	cmp byte[poison + 1], 90
	jb __rest

	mov byte[poison], 0
	mov byte[poison + 1], 0
	call fruitUpdate
	jmp __rest1

	__rest:
		inc byte[poison + 1]

	__rest1:
		mov al, [note_flag + 2]
		cmp byte[note_flag + 1], al
		jb __secCheck

		mov byte[note_flag], 0
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 0
		in al, 61h
		and al, 0xFC
		out 61h, al

	__secCheck:
		cmp byte[note_flag], 1
		jne __nextC
		inc byte[note_flag + 1]

		__nextC:
			add word [milliSecond], 55
			cmp word [milliSecond], 1000 ; one second
			jb __endClock

	call time_upd
	cmp word [life], 0
	je __endClock
	mov word [milliSecond], 0
	add word [second], 1		

	cmp word [second], 60 ; one minute
	jb __endClock

	mov word [second], 0
	add word [minute], 1

	__endClock:
		popa
		cmp word[life], 0
		je __final
		iret

	__final:
		pop ax
		push exit
		iret
;////////////////////////////////////////////

;////////////////////////////////////////////
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
;////////////////////////////////////////////

;////////////////////////////////////////////
printStatTags:
	pusha
	push es

	push 0xB800
	pop es

	mov si, 120
	mov di, 0
	__pl1:
		mov al, [str1 + di]
		mov ah, 0x07
		mov word[es:si], ax
		add si, 2
		inc di
		cmp byte[str1 + di], 0
		jne __pl1

	mov si, 280
	mov di, 0
	__pl2:
		mov al, [str2 + di]
		mov ah, 0x07
		mov word[es:si], ax
		add si, 2
		inc di
		cmp byte[str2 + di], 0
		jne __pl2

	mov si, 440
	mov di, 0
	__pl3:
		mov al, [str3 + di]
		mov ah, 0x07
		mov word[es:si], ax
		add si, 2
		inc di
		cmp byte[str3 + di], 0
		jne __pl3

	mov si, 600
	mov di, 0
	__pl4:
		mov al, [str4 + di]
		mov ah, 0x07
		mov word[es:si], ax
		add si, 2
		inc di
		cmp byte[str4 + di], 0
		jne __pl4

	pop es
	popa
	ret
;////////////////////////////////////////////

;////////////////////////////////////////////
updateStat:
	pusha
	push es

	push 0xB800
	pop es

	mov cl, [life]
	mov ch, 0
	cmp cx, 0
	je end

	inc cx
	mov si, 136
	__ul1:
		mov word[es:si], 0x0720
		add si, 2
		loop __ul1

	mov cl, [life]
	mov ch, 0

	mov si, 136
	__ul2:
		mov word[es:si], 0x0403
		add si, 2
		loop __ul2

	mov ax, 0x0007
	push ax
	push 2	;row
	push 67	;col
	mov al, [cs:score]
	mov ah, 0
	push ax
	call printnum

	mov ax, 0x0007
	push ax
	push 3	;row
	push 67	;col
	mov al, [cs:currlvl]
	mov ah, 0
	push ax
	call printnum

	end:
	pop es
	popa
	ret
;////////////////////////////////////////////

;////////////////////////////////////////////
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
;////////////////////////////////////////////

;////////////////////////////////////////////
drawPixel:
	push bp
	mov bp, sp

	push ax
	push bx
	push dx
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
	pop dx
	pop bx
	pop ax

	pop bp
	ret 6
;////////////////////////////////////////////

;////////////////////////////////////////////
getPixel:
	push bp
	mov bp, sp

	push bx
	push cx
	push dx
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
	pop dx
	pop cx
	pop bx

	pop bp
	ret 4
;////////////////////////////////////////////

;////////////////////////////////////////////	
rand:
	push bp
	mov bp, sp 

	push bx
	push cx
	push dx

    mov ax, 0  
	int 0x1A      

	; using Xn+1 = (aXn + c) mod M

	mov  ax, dx
	mov  dx, 0

	mov bx, 23
	mul bx

	add ax, dx
	mov dx, 0
 	mov  cx, [bp + 4]
	div  cx 

	mov ax, dx

	pop dx
	pop cx
	pop bx 

	pop bp
	ret 2
;////////////////////////////////////////////

;////////////////////////////////////////////
drawSnake:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx

	mov bx, snake
	mov cx, [snakeSize]
	dec cx
	jz __endSnakeDraw

	__loop2:
		mov dx, [bx]
		mov ax, dx

		mov ah, 0
		mov dl, dh
		mov dh, 0

		push 0x682A
		push dx
		push ax
		call drawPixel

		add bx, 2

		dec cx
		cmp cx, 0
		jnz __loop2

		mov dx, [bx]
		mov ax, dx

		mov ah, 0
		mov dl, dh
		mov dh, 0

		push 0x4040
		push dx
		push ax
		call drawPixel

	__endSnakeDraw:
		pop dx
		pop cx
		pop bx
		pop ax

		pop bp
		ret
;////////////////////////////////////////////

;////////////////////////////////////////////
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
;////////////////////////////////////////////

;////////////////////////////////////////////
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
;////////////////////////////////////////////
	
;////////////////////////////////////////////
fruitUpdate:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si

	__findFruitPos: 

		push 10
		call rand

		cmp ax, 3
		jne __nextComp

		mov byte[poison], 1
		mov byte[poison + 1], 0
		jmp __upd

		__nextComp:
			cmp ax, 5
			jne __nextComp1

			mov byte[poison], 1
			mov byte[poison + 1], 0
			jmp __upd

		__nextComp1:
			cmp ax, 8
			jne __upd

			mov byte[poison], 1
			mov byte[poison + 1], 0

		__upd:
			mov dx, ax
			shl dx, 1
			mov byte[fruitType], dl

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

;////////////////////////////////////////////
;	fruit level collision

	cmp word [currentLevel], 0
	jne __checkFruit2

		mov bx, level1
		mov cx, [level1Size]
		jmp __fruitLevelCollision

	__checkFruit2:
	cmp word [currentLevel], 1
	jne __checkFruit3

		mov bx, level2
		mov cx, [level2Size]
		jmp __fruitLevelCollision

	__checkFruit3:

		mov bx, level3
		mov cx, [level3Size]

	__fruitLevelCollision:

	mov si, 0

		__checkFruitLevel:
			cmp ax, [bx + si]
			je __findFruitPos

			add si, 2
			dec cx
			cmp cx, 0
			jnz __checkFruitLevel

		mov [fruitPos], ax

	pop si 
	pop dx 
	pop cx
	pop bx 
	pop ax

	pop bp
	ret
;////////////////////////////////////////////

;////////////////////////////////////////////
drawFruit:
	push bp
	mov bp, sp 

	push ax
	push bx
	push si

	mov bl, [fruitType]
	mov bh, 0
	push word[fruitArr + bx]

	mov ax, [fruitPos]

	mov bx, 0
	mov bl, ah
	mov ah, 0

	push bx
	push ax
	call drawPixel

	pop si
	pop bx
	pop ax

	pop bp
	ret
;////////////////////////////////////////////

genLevel1:
	
	pusha

	mov ax, 0x0604
	mov bx, level1
	mov cx, [level1Size]
	shr cx, 1

	gen_loop1:

		mov [bx], ax

		add bx, 2
		inc ax
		dec cx
		cmp cx, 0
		jnz gen_loop1

	mov ax, 0x1604
	mov cx, [level1Size]
	shr cx, 1

	gen_loop2:

		mov [bx], ax
		add bx, 2
		inc ax
		dec cx
		cmp cx, 0
		jnz gen_loop2

	popa
	ret	
;/////////////////////////////////////////////

genLevel2:
	
	pusha

	mov ax, 0x0804
	mov bx, level2
	mov cx, [level2Size]
	shr cx, 1

	gen_loop3:

		mov [bx], ax

		add bx, 2
		inc ah
		dec cx
		cmp cx, 0
		jnz gen_loop3

	mov ax, 0x084B
	mov cx, [level2Size]
	shr cx, 1

	gen_loop4:

		mov [bx], ax
		add bx, 2
		inc ah
		dec cx
		cmp cx, 0
		jnz gen_loop4

	popa
	ret	
;/////////////////////////////////////////////

genLevel3:
	pusha

	mov bx, level3
	mov cx, [level1Size]
	mov si, 0

	gen_loop5:
		mov ax, [level1 + si]
		mov [bx], ax

		add bx, 2
		add si, 2
		dec cx
		cmp cx, 0
		jnz gen_loop5


	mov cx, [level2Size]
	mov si, 0

	gen_loop6:
		mov ax, [level2 + si]
		mov [bx], ax

		add bx, 2
		add si, 2
		dec cx
		cmp cx, 0
		jnz gen_loop6


	popa
	ret



drawLevel:
	pusha

	cmp word [currentLevel], 0
	je d_level1

	cmp word [currentLevel], 1
	je d_level2

		mov bx, level3
		mov cx, [level3Size]

		d_loop3:
			mov ax, [bx]

			mov dx, ax
			mov dh, 0
			mov al, ah
			mov ah, 0

			push 0x3820
			push ax
			push dx
			call drawPixel

			add bx, 2
			dec cx
			cmp cx, 0
			jnz d_loop3
		jmp d_end

	d_level1:

		mov bx, level1
		mov cx, [level1Size]

		d_loop1:
			mov ax, [bx]

			mov dx, ax
			mov dh, 0
			mov al, ah
			mov ah, 0

			push 0x3820
			push ax
			push dx
			call drawPixel

			add bx, 2
			dec cx
			cmp cx, 0
			jnz d_loop1
		jmp d_end

	d_level2:

		mov bx, level2
		mov cx, [level2Size]

		d_loop2:
			mov ax, [bx]

			mov dx, ax
			mov dh, 0
			mov al, ah
			mov ah, 0

			push 0x3820
			push ax
			push dx
			call drawPixel

			add bx, 2
			dec cx
			cmp cx, 0
			jnz d_loop2
		jmp d_end


	d_end:
	popa
	ret

;////////////////////////////////////////////
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

	;////////////////////////////////
	;	check collision with level walls

	mov ax, [snakeSize]	
	mov bx, snake

	dec ax
	shl ax, 1
	add bx, ax
	mov dx, [bx]

	cmp word [currentLevel], 0
	jne __checkLevel2

		mov bx, level1
		mov cx, [level1Size]
		jmp __checkLevelCollision

	__checkLevel2:
	cmp word [currentLevel], 1
	jne __checkLevel3

		mov bx, level2
		mov cx, [level2Size]
		jmp __checkLevelCollision

	__checkLevel3:

		mov bx, level3
		mov cx, [level3Size]

	__checkLevelCollision:
	mov si, 0

	__levelCollision:
		cmp dx, [bx + si]
		jne __levelCol

		dec byte[life]
		call updateStat

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 4

		push 9121	;middle C
		call note
		call resetGame
		jmp __end

	__levelCol:
		add si, 2
		dec cx
		cmp cx, 0
		jnz __levelCollision


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
	jae __nxtC

	dec byte[life]
	call updateStat

	mov byte[note_flag], 1
	mov byte[note_flag + 1], 0
	mov byte[note_flag + 2], 4

	push 9121	;middle C
	call note
	call resetGame
	jmp __end

	__nxtC:
		cmp ah, ch
		jae __nxtC1

		dec byte[life]
		call updateStat

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 4

		push 9121	;middle C
		call note
		call resetGame
		jmp __end

	__nxtC1:
		mov cx, [boundryX2Y2]
		dec cl
		dec ch

		cmp al, cl
		jbe __nxtC2

		dec byte[life]
		call updateStat

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 4

		push 9121	;middle C
		call note
		call resetGame
		jmp __end

	__nxtC2:
		cmp ah, ch
		jbe __nxtC3

		dec byte[life]
		call updateStat

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 4

		push 9121	;middle C
		call note
		call resetGame
		jmp __end

; checking collision with snake's body
	__nxtC3:
		mov cx, [snakeSize]
		mov bx, snake
		dec cx
		mov si, 0

	__collision:
		cmp ax, [bx + si]
		jne __colS

		dec byte[life]
		call updateStat

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 4

		push 9121	;middle C
		call note
		call resetGame
		jmp __end

	__colS:
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

	cmp byte[poison], 1
	jne __skipC

	mov word[poison], 0
	dec byte[life]
	call updateStat

	mov byte[note_flag], 1
	mov byte[note_flag + 1], 0
	mov byte[note_flag + 2], 4

	push 9121	;middle C
	call note
	call resetGame

	jmp __end

	__skipC:
		add word [interpolate], 4

		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 6

		push 4560	;middle C
		call note
		call fruitUpdate
		inc byte[cs:score]
		call updateStat

	__end:
		cmp byte[life], 0
		je exit

		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax

		pop bp
		ret
;////////////////////////////////////////////

;////////////////////////////////////////////
interpolateSnake:
	pusha

	cmp word [interpInterval], 10 ; after 10 frames snake's size increases by 1, making the snake grow smoothly
	jne __noInterp

	mov word [interpInterval], 0

	cmp word [interpolate], 0
	je __noInterp

	mov ax, [snakeSize]	
	mov bx, snake

	dec ax
	shl ax, 1
	add bx, ax

	mov ax, [bx]
	mov [bx + 2], ax
	add word [snakeSize], 1
	cmp word[snakeSize], 240
	jae _noCheck1
	sub word [interpolate], 1
	__noInterp:
	
		add word [interpInterval], 1

		popa
		ret
;////////////////////////////////////////////

;////////////////////////////////////////////
;	Game Functions

resetGame:
	pusha

	mov word [snakeSize], 20
	inc word [currentLevel]
	inc byte[currlvl]
	mov word[poison], 0

	mov bx, snake
	mov cx, [snakeSize]
	mov ah, 12
	mov al, 28

	mov word [UP], 	  0
	mov word [DOWN],  0
	mov word [LEFT],  0
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
		call drawLevel
		call fruitUpdate
		call updateStat
		call printStatTags

		call drawSnake
	
	popa
	ret
;////////////////////////////////////////////

;////////////////////////////////////////////

speedControl:
	pusha

	mov ax, [second]

	cmp ax, 0
	je __noIncrease

	mov dx, 0
	mov bx, 20
	div bx

	cmp dx, 0
	jne __noIncrease

	mov ax, [speed + 2]
	cmp ax, 0x00FE
	jbe __noIncrease

	sub ah, 0x11
	mov [speed + 2], ax

	__noIncrease:
	mov ax, 0
	mov cx, [speed]
	mov dx, [speed + 2]
	mov ah, 0x86
	int 0x15

	popa
	ret
;////////////////////////////////////////////

;///////////////////////////////////////////

startSound:
	pusha

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

			push cx
			push dx

			mov ah, 0x86
			mov cx, 2
			mov dx, 0
			mov al, 0
			int 0x15

			pop dx
			pop cx

			dec bx
			jnz __outerLoop

	popa
	ret
;////////////////////////////////////////////

;////////////////////////////////////////////
update:
	push bp
	mov bp, sp

	push ax
	push bx
	push cx
	push dx
	push si

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

	pop si
	pop dx
	pop cx
	pop bx
	pop ax

	pop bp
	ret
;////////////////////////////////////////////

;////////////////////////////////////////////
	printName:
		pusha

        push 0xB800
        pop es

        mov cx, 83
        mov si, name

        p_l1:
            mov bx, [si]
            mov word[es:bx + 36], 0x4020
            add si, 2
            mov dx, 1000
            __delay1:
                mov ax, 100
                __delay2:
                    dec ax
                    jnz __delay2
                dec dx
                jnz __delay1
            dec cx
            jnz p_l1
        
        popa
        ret
;////////////////////////////////////////////

;////////////////////////////////////////////
	menu:
		pusha

	    push 0xB800
	    pop es

	    cmp byte[curr], 1
	    jne m_cmp2

	    mov ah, 0x47
	    jmp m_p1
	    m_cmp2:
	        mov ah, 0x47
	        jmp m_p2

	    m_p1:
	    	mov si, 1994
	    	mov di, 0
			
	    m_l1:
	        mov al, [m1 + di]
		    mov word[es:si], ax
		    add si, 2
		    inc di
		    cmp byte[m1 + di], 0
		    jne m_l1

	    cmp byte[curr], 2
	    je m_exit

	    mov ah, 0x07
	    m_p2:
	    	mov si, 2314
	   	 	mov di, 0
	    m_l2:
	        mov al, [m2 + di]
		    mov word[es:si], ax
		    add si, 2
		    inc di
		    cmp byte[m2 + di], 0
		    jne m_l2

	    mov ah, 0x07
	    cmp byte[curr], 2
	    je m_p1

	    m_exit:
	    	popa
	    	ret
;////////////////////////////////////////////

;////////////////////////////////////////////
	endDetails:
		pusha
		push es

		push 0xB800
		pop es

		mov si, 1994
		mov di, 0

		jmp n
		wonmsg:
			mov di, end_msg3
			jmp e_l1

		n:
			cmp word[snakeSize], 240
			jae wonmsg

		mov di, end_msg1
		e_l1:
			mov ah, 0x47
			mov al, [di]
			mov word[es:si], ax
			add si, 2
			inc di
			cmp byte[di], 0
			jne e_l1

		mov si, 2314
		mov di, 0

		e_l2:
			mov ah, 0x07
			mov al, [end_msg2 + di]
			mov word[es:si], ax
			add si, 2
			inc di
			cmp byte[end_msg2 + di], 0
			jne e_l2

		mov ax, 0x0004
		push ax
		push 14
		push 50
		mov ah, 0
		mov al, [score]
		push ax
		call printnum

		pop es
		popa
		ret
;////////////////////////////////////////////

;////////////////////////////////////////////
;	Main Function

main:
	
	call clrscr
	;call printName
	start_loop:
        call menu
        mov ah, 0x00
        int 0x16
        cmp ah, 0x48    ;up

        jne l_nextcmp
        mov byte[curr], 1
        jmp l_skip

        l_nextcmp:
            cmp ah, 0x50    ;down
            jne l_nextcmp1

        mov byte[curr], 2
        jmp l_skip
		
       	l_nextcmp1:
            cmp ah, 0x1C
            jne l_skip

        jmp l_next
        l_skip:
            jmp start_loop

	l_next:
		cmp byte[curr], 2
		je quit
	
	push 0
	pop es

	mov ax, [es:0x1C * 0x4]
	mov word[timerold], ax
	mov ax, [es:0x1C * 0x4 + 0x2]
	mov word[timerold + 2], ax
	

	mov word [es:0x1C * 0x4], clock
	mov word [es:0x1C * 0x4 + 0x2], cs

	mov word [speed], 0x0001
	mov word [speed +  2], 0xFFFE


	call genLevel1
	call genLevel2
	call genLevel3

	call resetGame
	call fruitUpdate
	call startSound

	infinite:

		call interpolateSnake
		call update
		call checkCollision
		call drawLevel
		call drawFruit
		call drawSnake
		call speedControl

		jmp infinite

	exit:
		mov byte[note_flag], 1
		mov byte[note_flag + 1], 0
		mov byte[note_flag + 2], 8

		push 9121	;middle C
		call note

		cmp byte [score], 0
		jne _noCheck

		in al, 61h
		and al, 0xFC
		out 61h, al
_noCheck:

		cmp byte [score], 0
		je _noCheck1

		deathloop:
			cmp byte[note_flag], 1
			je deathloop
_noCheck1:

		mov ax, [timerold]
		mov bx, [timerold + 2]

		push 0
		pop es
		mov word [es:0x1C * 0x4], ax
		mov word [es:0x1C * 0x4 + 0x2], bx

		call clrscr
		call endDetails

	quit:
		mov ax, 0x4c00
		int 0x21
;////////////////////////////////////////////

;////////////////////////////////////////////
;	Defines	

boundryX1Y1: 		dw 0x0400 ; H = Row, L = Column
boundryX2Y2: 		dw 0x184F ; H = Row, L = Column

fruitPos: 			dw 0 ; H = Row, L = Column
fruitArr:			dw 0x010F, 0x0514, 0x0315, 0x8824, 0x0E25, 0x8D0E, 0x0606, 0x0613, 0x831E, 0x0417 
fruitType:			db 0
poison:				dw 0

milliSecond: 		dw 0
second: 			dw 0
minute: 			dw 0

timerold:			dd 0
speed:				dd 0

interpolate: 		dw 0
interpInterval: 	dw 0

UP:					dw 0
DOWN:				dw 0
LEFT:				dw 0
RIGHT:				dw 0

note_flag: times 3 	db 0

life:				db 3
score:				db 0
curr_time:			db 4, 0, 0

snakeSize:			dw 20
maxSize:			dw 240
snake: times 240 	dw 0 ; AH = Rows, AL = Columns

currentLevel:		dw 0xFFFF
level1:	times 144	dw 0			
level1Size:			dw 144
level2:	times 28	dw 0
level2Size:			dw 28
level3: times 172	dw 0
level3Size:			dw 172

name: dw 342, 340, 338, 336, 496, 656, 816, 818, 820, 822, 982, 1142, 1302, 1300, 1298, 1296, 
      dw 346, 506, 666, 826, 986, 1146, 1306, 508, 670, 832, 994, 1156, 1318, 1158, 998, 838, 678, 518, 358, 
      dw 362, 522, 682, 842, 1002, 1162, 1322, 364, 366, 368, 528, 688, 848, 1008, 1168, 1328, 844, 846, 848,
      dw 372, 532, 692, 852, 1012, 1172, 1332, 694, 536, 378, 1014, 1176, 1338,
      dw 382, 542, 702, 862, 1022, 1182, 1342, 384, 386, 388, 864, 866, 868, 1344, 1346, 1348

str1: 				db 'Health:', 0
str2:				db 'Time:', 0
str3:				db 'Score:', 0
str4:				db 'Level:', 0
currlvl:			db 0

end_msg1:			db 'GAME OVER', 0
end_msg2:			db 'Your Score:', 0
end_msg3:           db 'YOU WON', 0

curr: db 1
m1:	  db 'START', 0
m2:   db 'QUIT', 0
;////////////////////////////////////////////
