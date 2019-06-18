;MORE FPU
INCLUDE Irvine32.inc

SCREEN_W equ 119   ;출력 화면 넓이
SCREEN_H equ 30     ;출력 화면 높이
BLOCK_SIZE equ 100  ;블럭 사이즈
WORLD_X equ 10      ;월드 x 크기
WORLD_Y equ 10     ;월드 y 크기

vector STRUCT       ;벡터
	x DWORD 0
	y DWORD 0
vector ENDS

point STRUCT	;골 좌표
	x BYTE 0
	y BYTE 0
	off DWORD 0	;ebp - esp (시작 지점에서 가장 멀리있는 좌표를 골로 지정하기 위함)
point ENDS

.data
    wall_t BYTE "###]]/||;:......                                ",0      ;벽 텍스쳐
    floor_t BYTE "    .^",0      ;바닥 텍스쳐
    pixels BYTE SCREEN_H-1 DUP(SCREEN_W DUP(?), 0dh, 0ah), SCREEN_W DUP(?),0    ;픽셀 버퍼
	zero REAL8 0.0	;fpu 연산에 사용되는 0
	den REAL8 0.0
	tval REAL8 0.0
	rad REAL8 180.0
	stage BYTE 0	;상태 (1: 게임 클리어, 2: 메뉴로 돌아가기)
	world BYTE WORLD_X * WORLD_Y DUP(0)


	;미리 계산한 각도별 방향 벡터 & 단위벡터 (좌표계가 정수 & 속도 개선을 위함)
	walk	vector <5,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>	;cos x * 5, sin x * 5
			vector <4,0>,<4,0>,<4,1>,<4,1>,<4,1>,<4,1>,<4,1>,<4,1>,<4,1>,<4,1>
			vector <4,1>,<4,1>,<4,1>,<4,1>,<4,2>,<4,2>,<4,2>,<4,2>,<4,2>,<4,2>
			vector <4,2>,<4,2>,<4,2>,<4,2>,<4,2>,<4,2>,<4,2>,<3,3>,<3,3>,<3,3>
			vector <3,3>,<3,3>,<3,3>,<3,3>,<3,3>,<3,3>,<3,3>,<3,3>,<3,3>,<3,3>
			vector <3,3>,<3,3>,<3,3>,<3,3>,<2,4>,<2,4>,<2,4>,<2,4>,<2,4>,<2,4>
			vector <2,4>,<2,4>,<2,4>,<2,4>,<2,4>,<2,4>,<2,4>,<1,4>,<1,4>,<1,4>
			vector <1,4>,<1,4>,<1,4>,<1,4>,<1,4>,<1,4>,<1,4>,<1,4>,<1,4>,<0,4>
			vector <0,4>,<0,4>,<0,4>,<0,4>,<0,4>,<0,4>,<0,4>,<0,4>,<0,4>,<0,4>
			vector <-1,5>,<-1,4>,<-1,4>,<-1,4>,<-1,4>,<-1,4>,<-1,4>,<-1,4>,<-1,4>,<-1,4>
			vector <-1,4>,<-1,4>,<-2,4>,<-2,4>,<-2,4>,<-2,4>,<-2,4>,<-2,4>,<-2,4>,<-2,4>
			vector <-2,4>,<-2,4>,<-2,4>,<-2,4>,<-3,4>,<-3,4>,<-3,4>,<-3,4>,<-3,4>,<-3,4>
			vector <-3,4>,<-3,4>,<-3,4>,<-3,4>,<-3,4>,<-3,4>,<-3,4>,<-4,3>,<-4,3>,<-4,3>
			vector <-4,3>,<-4,3>,<-4,3>,<-4,3>,<-4,3>,<-4,3>,<-4,3>,<-4,3>,<-4,3>,<-4,3>
			vector <-4,3>,<-4,3>,<-4,3>,<-4,3>,<-5,2>,<-5,2>,<-5,2>,<-5,2>,<-5,2>,<-5,2>
			vector <-5,2>,<-5,2>,<-5,2>,<-5,2>,<-5,2>,<-5,2>,<-5,2>,<-5,1>,<-5,1>,<-5,1>
			vector <-5,1>,<-5,1>,<-5,1>,<-5,1>,<-5,1>,<-5,1>,<-5,1>,<-5,1>,<-5,1>,<-5,0>
			vector <-5,0>,<-5,0>,<-5,0>,<-5,0>,<-5,0>,<-5,0>,<-5,0>,<-5,0>,<-5,0>,<-5,0>
			vector <-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>,<-5,-1>
			vector <-5,-1>,<-5,-1>,<-5,-2>,<-5,-2>,<-5,-2>,<-5,-2>,<-5,-2>,<-5,-2>,<-5,-2>,<-5,-2>
			vector <-5,-2>,<-5,-2>,<-5,-2>,<-5,-2>,<-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>
			vector <-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>,<-5,-3>,<-4,-4>,<-4,-4>,<-4,-4>
			vector <-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>
			vector <-4,-4>,<-4,-4>,<-4,-4>,<-4,-4>,<-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>
			vector <-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>,<-3,-5>,<-2,-5>,<-2,-5>,<-2,-5>
			vector <-2,-5>,<-2,-5>,<-2,-5>,<-2,-5>,<-2,-5>,<-2,-5>,<-2,-5>,<-2,-5>,<-2,-5>,<-1,-5>
			vector <-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>,<-1,-5>
			vector <0,-5>,<0,-5>,<0,-5>,<0,-5>,<0,-5>,<0,-5>,<0,-5>,<0,-5>,<0,-5>,<0,-5>
			vector <0,-5>,<0,-5>,<1,-5>,<1,-5>,<1,-5>,<1,-5>,<1,-5>,<1,-5>,<1,-5>,<1,-5>
			vector <1,-5>,<1,-5>,<1,-5>,<1,-5>,<2,-5>,<2,-5>,<2,-5>,<2,-5>,<2,-5>,<2,-5>
			vector <2,-5>,<2,-5>,<2,-5>,<2,-5>,<2,-5>,<2,-5>,<2,-5>,<3,-4>,<3,-4>,<3,-4>
			vector <3,-4>,<3,-4>,<3,-4>,<3,-4>,<3,-4>,<3,-4>,<3,-4>,<3,-4>,<3,-4>,<3,-4>
			vector <3,-4>,<3,-4>,<3,-4>,<3,-4>,<4,-3>,<4,-3>,<4,-3>,<4,-3>,<4,-3>,<4,-3>
			vector <4,-3>,<4,-3>,<4,-3>,<4,-3>,<4,-3>,<4,-3>,<4,-3>,<4,-2>,<4,-2>,<4,-2>
			vector <4,-2>,<4,-2>,<4,-2>,<4,-2>,<4,-2>,<4,-2>,<4,-2>,<4,-2>,<4,-2>,<4,-1>
			vector <4,-1>,<4,-1>,<4,-1>,<4,-1>,<4,-1>,<4,-1>,<4,-1>,<4,-1>,<4,-1>,<4,-1>
    
	camp vector <150,150>	;카메라 위치
	camd DWORD 0			;카메라 각도

    tmp vector 10 DUP(<>)     ;벡터 계산을 위한 저장소
	goal point <>	;골지점 벡터

	;출력에 사용되는 문자열들
	clear_msg BYTE SCREEN_W DUP('='),0dh,0ah, SCREEN_W DUP(' '),0dh,0ah, 54 DUP(' '), "stage clear!", SCREEN_W-64 DUP (' '), 0dh, 0ah,  SCREEN_W DUP(' '), 0dh, 0ah, SCREEN_W DUP('='), 0
	title_msg BYTE 13 DUP (SCREEN_W DUP ('.'), 0dh, 0ah), SCREEN_W DUP (' '), 0dh, 0ah, 56 DUP (' '), "3D MAZE", SCREEN_W-63 DUP (' '), 0dh, 0ah, SCREEN_W DUP (' '), 0dh, 0ah, 13 DUP (SCREEN_W DUP ('.'), 0dh, 0ah), SCREEN_W DUP ('.'), 0
	help_msg BYTE "INSTRUCTIONS:", 0dh, 0ah, 0ah, "W,A,S,D : move", 0dh, 0ah, "<,> : change camera angle", 0dh, 0ah
			BYTE "M : view minimap", 0dh, 0ah, "Q : exit to menu", 0dh, 0ah, "R : reset current position", 0dh, 0ah
			BYTE "H : show this message", 0dh, 0ah, "  Map symbols:", 0dh, 0ah, "  O : current position", 0dh, 0ah
			BYTE "  G : goal", 0dh, 0ah, "  # : wall", 0dh, 0ah, 0
	minimap_msg BYTE "Generated map :  ", 0dh, 0ah, 0
	half_line BYTE 60 DUP (' '), 0dh, 0ah, 0
	
	aa REAL8 -2.0

.code

;--------------------------------------------
main PROC
; main procedure
; Input: nothing
; Output: nothing
;--------------------------------------------
	mov tmp[0].x, 100
	mov tmp[0].y, 200
	mov tmp[1*TYPE vector].x, 200
	mov tmp[1*TYPE vector].y, 200
	mov esi, 90
	mov camp.x, 150
	mov camp.y, 150
	call intersect

menu_loop:
	call menusplash	;press any key to start
game_init:
	call gameinit
game_loop:
	call getinput
	call makefloor
	call makewall
	call campus
	call render
	cmp stage, 1	;1: game clear
		je game_clear
	cmp stage, 2	;2: return to menu
		je menu_loop
	jmp game_loop
game_clear:
	call gameclear
	jmp game_init
	exit
main ENDP

;--------------------------------------------
gamemap PROC uses eax ecx edx
; prints game map on screen
; Input: nothing
; Output: nothing
;--------------------------------------------
	call getwc
	mov ecx, edx
	mov edx, 0
	call gotoxy
	mov edx, offset minimap_msg
	call writestring
	mov dl, 0
	mov dh, WORLD_Y-1
map_loop:
	cmp dh, 0
	jl map_loop_done
		mov dl, 0
		.WHILE dl < WORLD_X
			.IF dx == cx
				mov eax, 'O'
				call writechar
				mov eax, ' '
				call writechar
				jmp continue
			.ENDIF
			call wctoi
			mov al, world[eax]
			.IF al == 0
				mov eax, ' '
			.ELSEIF al == 1
				mov eax, '#'
			.ELSEIF al == 2
				mov eax, 'G'
			.ELSEIF al == 3
				mov eax, '-'
			.ENDIF
			call writechar
			mov eax, ' ' 
			call writechar
		continue:
			inc dl
		.ENDW
		dec dh
		call crlf
		jmp map_loop
	map_loop_done:
	ret
gamemap ENDP

;--------------------------------------------
campus PROC uses eax ecx
; set campus on output buffer (pixels)
; Input: nothing
; Output: nothing
;--------------------------------------------
	mov eax, camd
	sub eax, SCREEN_W/2
	cmp eax, 0
	jge campus_p
		add eax, 360
campus_p:
	mov ecx, SCREEN_W-1
campus_loop:
	cmp ecx, 0
	jl campus_loop_done
		.IF eax >= 360
			sub eax, 360
		.ENDIF
		.IF eax == 0
			mov pixels[ecx], 'E'
		.ELSEIF eax == 45
			mov pixels[ecx], 'N'
			mov pixels[ecx+1], 'E'
		.ELSEIF eax == 90
			mov pixels[ecx], 'N'
		.ELSEIF eax == 135
			mov pixels[ecx], 'N'
			mov pixels[ecx+1], 'W'
		.ELSEIF eax == 180
			mov pixels[ecx], 'W'
		.ELSEIF eax == 225
			mov pixels[ecx], 'S'
			mov pixels[ecx+1], 'W'
		.ELSEIF eax == 270
			mov pixels[ecx], 'S'
		.ELSEIF eax == 315
			mov pixels[ecx], 'S'
			mov pixels[ecx+1], 'E'
		.ENDIF
		inc eax
		dec ecx
	jmp campus_loop
campus_loop_done:
	ret
campus ENDP

;--------------------------------------------
menusplash PROC uses edx
; shows menu screen
; Input: nothing
; Output: nothing
;--------------------------------------------
	mov edx, 0 
	call gotoxy
	mov edx, offset title_msg
	call writestring
	mov edx, 0f30h
	call gotoxy
	call waitmsg
	ret
menusplash ENDP

;--------------------------------------------
gameinit PROC uses eax ecx edx
; initialized variables for gameplay
; Input: nothing
; Output: nothing
;--------------------------------------------
	call clrscr
	mov goal.x, 0
	mov goal.y, 0
	mov goal.off, 0
	call generatemap
	mov camp.x, 150
	mov camp.y, 150
	mov camd, 0
	mov stage, 0
	;print instructions and minimap
	call gamemap
	call crlf
	mov edx, offset help_msg
	call writestring
	call waitmsg
	ret
gameinit ENDP

;--------------------------------------------
generatemap PROC uses eax ebx ecx edx esi
; generate random maze(map) using DFS
; Input: nothing
; Output: nothing
;--------------------------------------------
	push ebp
	mov ebp, esp
	;clean world
	mov ecx, WORLD_X*WORLD_Y
map_init:
	mov world[ecx], 3
	loop map_init

	mov esi, 0 ;dirty counter
	;make default walls
	mov ecx, 0
	mov edx, 0
	.WHILE ecx < WORLD_X
		mov dl, cl	;y=0
		call wctoi
		mov world[eax], 1
		inc esi
		inc ecx
	.ENDW
	mov ecx, 0
	mov edx, 0
	.WHILE ecx < WORLD_Y
		mov dh, cl	;x=0
		call wctoi
		mov world[eax], 1
		inc esi
		inc ecx
	.ENDW
	mov ecx, 0
	mov edx, 0
	.WHILE ecx < WORLD_X
		mov dh, WORLD_Y-1
		mov dl, cl	;y=WORLD_Y-1
		call wctoi
		mov world[eax], 1
		inc esi
		inc ecx
	.ENDW
	mov ecx, 0
	mov edx, 0
	.WHILE ecx < WORLD_Y
		mov dl, WORLD_X-1
		mov dh, cl	;x=WORLD_X-1
		call wctoi
		mov world[eax], 1
		inc ecx
		inc esi
	.ENDW
	sub esi, 4	;remove duplicates

	;recursive backtracker
	call randomize	;generate seed
	mov dx, -1
	push dx	;mark starting point in stack
	mov edx, 0101h	;starts from 1,1
	.WHILE esi < WORLD_X * WORLD_Y
		call wctoi	;mark current cell
		.IF world[eax] == 3
			inc esi
			mov world[eax], 0
			;update goal
			;(goal is the furthest point)
			mov eax, ebp
			sub eax, esp
			cmp eax, goal.off
			jbe no_update_goal
			mov goal.x, dl
			mov goal.y, dh
			mov goal.off, eax
	no_update_goal:
		.ENDIF
		mov bl, 0	;path counter
		;check x+1, y
		inc dl
		call checkcell
		.IF al == 0
			push dx
			inc bl
		.ENDIF
		;check x-1, y
		sub dl, 2
		call checkcell
		.IF al == 0
			push dx
			inc bl
		.ENDIF
		;check x, y+1
		inc dl
		inc dh
		call checkcell
		.IF al == 0
			push dx
			inc bl
		.ENDIF
		;check x, y-1
		sub dh, 2
		call checkcell
		.IF al == 0
			push dx
			inc bl
		.ENDIF
		inc dh	;restore coor
		.IF bl == 0
			;no more path
			pop dx
			.IF dx == -1 ;cannot find any more path
				jmp forcestop
			.ENDIF
		.ELSE
			;path exists
			;select random
			.IF bl > 1
				movzx eax, bl
				call randomrange	;eax = random val (0 ~ eax)
				push edx	;2 * eax
				mov edx, 0
				mov ecx, 2
				mul ecx
				pop edx
			.ELSE
				mov eax, 0
			.ENDIF
			add eax, esp
			movzx ecx, bl	;loop = dirty counter
			mov bx,WORD PTR [eax]	;get choosen value
			popper:	;pop available paths
				pop ax
				loop popper
			;mov to next cell, mark & push previous cell (dx)
			push dx
			mov dx, bx
		.ENDIF
	.ENDW
	jmp normalstop
forcestop:
	;loop through all cells and find isolated uninitialized cell
	mov edx, 0
	.WHILE dh < WORLD_Y
		mov dl, 0
		.WHILE dl < WORLD_X
			call wctoi
			.IF world[eax] == 3
				mov world[eax], 1
			.ENDIF
			inc dl
		.ENDW
		inc dh
	.ENDW
normalstop:
	;set goal
	mov dl, goal.x
	mov dh, goal.y
	call wctoi
	mov world[eax], 2
	leave
	ret
generatemap ENDP

;--------------------------------------------
checkcell PROC uses ebx edx
; check if cell is available
; Input: DL=X coor, DH=Y coor 
; Output: AL (0:available, else:unavailable)
;--------------------------------------------
	call wctoi	;get index
	movzx eax, world[eax]
	.IF al == 3	;has to be 3(unused)
		mov bl, 0	;dirty counter
		;x+1, y
		inc dl
		.IF dl<WORLD_X	;check oob
			call wctoi
			mov al, world[eax]
			.IF al == 0	;is a path
				inc bl
			.ENDIF
		.ENDIF
		;x-1, y
		sub dl, 2
		cmp dl, 0
		jl x_negative
		call wctoi
		mov al, world[eax]
		.IF al == 0	;is a path
			inc bl
		.ENDIF
		inc dl
	x_negative:
		;x, y+1
		inc dh
		.IF dh<WORLD_Y
			call wctoi
			mov al, world[eax]
			.IF al == 0	;is a path
				inc bl
			.ENDIF
		.ENDIF
		;x, y-1
		sub dh, 2
		cmp dh, 0
		jl y_negative
		call wctoi
		mov al, world[eax]
		.IF al == 0	;is a path
			inc bl
		.ENDIF
	y_negative:
		inc dh	;restore coor
		.IF bl > 1
			call wctoi	;has to be a wall
			mov world[eax], 1
			inc esi
			mov eax, 1
		.ELSE
			mov eax, 0
		.ENDIF
	.ELSE
		mov eax, 1
	.ENDIF
	ret
checkcell ENDP

;--------------------------------------------
gameclear PROC uses edx
; outputs stage clear message
; Input: Nothing 
; Output: Nothing
;--------------------------------------------
	mov edx, 0d00h
	call gotoxy
	mov edx, offset clear_msg
	call writestring
	mov edx, 1030h
	call gotoxy
	mov eax, 3000
	call delay
	call waitmsg
	ret
gameclear ENDP

;--------------------------------------------
getinput PROC uses eax ebx
; read and process user input
; Input: Nothing 
; Output: Nothing
;--------------------------------------------
	call readKey
	mov tmp[0].x, 0
	mov tmp[0].y, 0
	.IF al == 'w'
		mov ebx, camd
		mov eax, walk[ebx*TYPE vector].x
		add tmp[0].x, eax
		mov eax, walk[ebx*TYPE vector].y
		add tmp[0].y, eax
		call move
	.ELSEIF al == 's'
		mov ebx, camd
		mov eax, walk[ebx*TYPE vector].x
		sub tmp[0].x, eax
		mov eax, walk[ebx*TYPE vector].y
		sub tmp[0].y, eax
		call move
	.ELSEIF al == 'a'
		mov ebx, camd
		add ebx, 90
		.IF ebx>=360
			sub ebx, 360
		.ENDIF
		mov eax, walk[ebx*TYPE vector].x
		add tmp[0].x, eax
		mov eax, walk[ebx*TYPE vector].y
		add tmp[0].y, eax
		call move
	.ELSEIF al == 'd'
		mov ebx, camd
		add ebx, 90
		.IF ebx>=360
			sub ebx, 360
		.ENDIF
		mov eax, walk[ebx*TYPE vector].x
		sub tmp[0].x, eax
		mov eax, walk[ebx*TYPE vector].y
		sub tmp[0].y, eax
		call move
	.ELSEIF al == ','
		add camd, 5
		.IF camd >= 360
			sub camd, 360
		.ENDIF
	.ELSEIF al == '.'
		sub camd, 5
		cmp camd, 0
		jl camd_n
		ret
camd_n:
		add camd, 360
	.ELSEIF al == 'm'
		call gamemap
		call waitmsg
	.ELSEIF al == 'h'
		call helpmsg
		call waitmsg
	.ELSEIF al == 'q'
		mov stage, 2
	.ELSEIF al == 'r'
		mov camp.x, 150
		mov camp.y, 150
		mov camd, 0
	.ENDIF
	ret
getinput ENDP

;--------------------------------------------
helpmsg PROC uses ecx edx
; outputs help message
; Input: Nothing 
; Output: Nothing
;--------------------------------------------
	mov edx, 0
	call gotoxy
	mov edx, offset half_line
	mov ecx, 14
help_clear:
	call writestring
	loop help_clear
	mov edx, 0
	call gotoxy
	mov edx, offset help_msg
	call writestring
	ret
helpmsg ENDP

;--------------------------------------------
move PROC uses eax ebx ecx edx
; move camera position
; Input: camp=current position, tmp[0]=vector to add to current position
; Output: Nothing
;--------------------------------------------
	mov eax, camp.x
	add eax, tmp[0].x
	add eax, tmp[0].x
	mov ebx, BLOCK_SIZE
	mov edx, 0
	div ebx
	mov ecx, eax
	mov eax, camp.y
	add eax, tmp[0].y
	add eax, tmp[0].y
	mov edx, 0
	div ebx
	;x = eax, y = ecx
	mov dl, cl
	mov dh, al
	call wctoi
	mov al, world[eax]
	.IF al == 0
		;can move
		mov eax, tmp[0].x
		add camp.x, eax
		mov eax, tmp[0].y
		add camp.y, eax
	.ELSEIF al == 2
		;goal
		mov stage, 1
	.ENDIF
	ret
move ENDP

;--------------------------------------------
render PROC uses ecx edx
; outputs screen buffer(pixels)
; Input: Nothing 
; Output: Nothing
;--------------------------------------------
	mov edx, 0
	mov pixels[SIZEOF pixels -2], dl
	call gotoxy
	mov edx, offset pixels
	call writestring
	ret
render ENDP

;--------------------------------------------
makefloor PROC uses eax ebx ecx edx
; create floor in screen buffer
; Input: Nothing 
; Output: Nothing
;--------------------------------------------
	mov dh, 0	;y=0
floor_h_loop:
		movzx eax, dh
		push edx
		mov edx, 0
		mov ecx, 5
		div ecx
		mov ecx, 6
		sub ecx, edx
		pop edx
		mov bl, floor_t[eax]	;get texture
		mov dl, 0	;x=0
		call ctoi	;save pixel index in eax
	floor_w_loop:
			movzx ecx, dl
			add ecx, eax
			mov pixels[ecx], bl	;set pixel
			inc dl
			cmp dl, SCREEN_W
			jl floor_w_loop
		inc dh
		cmp dh, SCREEN_H
		jl floor_h_loop
	ret
makefloor ENDP

;--------------------------------------------
makewall PROC uses eax ebx ecx edx esi edi
; create wall in screen buffer (use ray-casting)
; Input: Nothing 
; Output: Nothing
;--------------------------------------------
	mov esi, camd
	sub esi, SCREEN_W/2
	cmp esi, 0
	jl w_negative
	jmp w_positive
w_negative:
	add esi, 360
w_positive:
	mov dl, SCREEN_W-1
w_loop:
	cmp dl, 0
	jl w_loop_done	;(counter)loop through fov angles and shoot rays
		.IF esi >= 360
			sub esi, 360
		.ENDIF
		mov eax, esi	;save angle to eax
		call shootray	;shoot ray to current direction vector
		mov eax, ebx	;save distance to eax
		cmp ebx, 0
		jl no_intersect
			push edx
			mov edx, 0
			mov ebx, 20
			div ebx
			pop edx
			mov dh, al	;save y
			push edx
			mov edx, 0
			mov ebx, 1
			div ebx
			mov bl, wall_t[eax]	;get texture
			pop edx
			mov eax, SCREEN_H
			sub al, dh
			.IF dh < 15
				.WHILE dh < al
					push eax
					call ctoi
					mov pixels[eax], bl
					inc dh
					pop eax
				.ENDW
			.ENDIF
no_intersect:
		inc esi
		dec dl
	jmp w_loop
w_loop_done:
	ret
makewall ENDP

;--------------------------------------------
shootray PROC uses eax ecx edx esi
; cast ray to selected angle 
; Input: eax=angle
; Output: ebx=max length
;--------------------------------------------
	mov esi, eax
	mov ebx, -1
	;loop through all blocks
	mov edx, 0
	.WHILE dh < WORLD_Y ;loop y
		.WHILE dl < WORLD_X ;loop x
			mov eax, edx
			call wctoi	;index is in eax
			movzx eax, world[eax]
			.IF eax == 1	; is a block
				call wctorc
				;south
				mov eax, tmp[0*TYPE vector].x
				mov tmp[1*TYPE vector].x, eax
				mov eax, tmp[0*TYPE vector].y
				mov tmp[1*TYPE vector].y, eax
				mov eax, tmp[1*TYPE vector].x
				add eax, BLOCK_SIZE
				mov tmp[1*TYPE vector].x, eax
				call intersect
				.IF eax < ebx || ebx == -1
					mov ebx, eax
				.ENDIF
				;east
				add tmp[0*TYPE vector].x, BLOCK_SIZE
				add tmp[1*TYPE vector].y, BLOCK_SIZE
				call intersect
				.IF eax < ebx || ebx == -1
					mov ebx, eax
				.ENDIF
				;south
				add tmp[0*TYPE vector].y, BLOCK_SIZE
				sub tmp[1*TYPE vector].x, BLOCK_SIZE
				call intersect
				.IF eax < ebx || ebx == -1
					mov ebx, eax
				.ENDIF
				;west
				sub tmp[0*TYPE vector].x, BLOCK_SIZE
				sub tmp[1*TYPE vector].y, BLOCK_SIZE
				call intersect
				.IF eax < ebx || ebx == -1
					mov ebx, eax
				.ENDIF
			.ENDIF
			inc dl
		.ENDW
		mov dl, 0
		inc dh
	.ENDW
	ret
shootray ENDP

;--------------------------------------------
intersect PROC uses ebx ecx edx edi esi
; check intersection of camera+angle and line
; Input: tmp[0]&tmp[1]=start&end point of line to check intersection, camp=camera position, ESI=camera angle 
; Output: EAX=distance
;--------------------------------------------
	;if intersects, return distance (-1 = no intersection)
	mov ebx, -1
	;den = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
	finit
	fild tmp[0*TYPE vector].x	;x1
	fild tmp[1*TYPE vector].x	
	fsub						;-x2
	fild camp.y	;y3
	fild camp.y
	push esi
	fild dword ptr[esp]
	pop esi
	fld rad
	fdiv
	fldpi
	fmul
	fsin
	fadd
	fsub						;-y4
	fmul						;*
	fild tmp[0*TYPE vector].y	;y1
	fild tmp[1*TYPE vector].y	;-y2
	fsub
	fild camp.x	;x3
	fild camp.x
	push esi
	fild DWORD PTR [esp]
	pop esi
	fld rad
	fdiv
	fldpi
	fmul
	fcos
	fadd
	fsub;-x4
	fmul;*
	fsub	;ST(0) = denominator
	fst den
	fld zero
	fcomip ST(0), ST(1)	;if den == 0
	je intersect_ret
	;t = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4) / den
	fild tmp[0*TYPE vector].x	;x1
	fild camp.x	;-x3
	fsub
	fild camp.y	;y3
	fild camp.y
	push esi
	fild DWORD PTR [esp]
	pop esi
	fld rad
	fdiv
	fldpi
	fmul
	fsin
	fadd
	fsub;-y4
	fmul
	fild tmp[0*TYPE vector].y	;y1
	fild camp.y	;-y3
	fsub
	fild camp.x	;x3
	fild camp.x
	push esi
	fild DWORD PTR [esp]
	pop esi
	fld rad
	fdiv
	fldpi
	fmul
	fcos
	fadd
	fsub	;-x4
	fmul
	fsub
	fld den
	fdiv
	fst tval
	fld zero	;0 < t
	fcomip ST(0), ST(1)
	jae intersect_ret
	mov eax, 1
	push eax
	fild DWORD PTR [esp]	;1 > t
	pop eax
	fcomip ST(0), ST(1)
	jbe intersect_ret
	;u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / den;
	fild tmp[0*TYPE vector].x	;x1
	fild tmp[1*TYPE vector].x	
	fsub;-x2
	fild tmp[0*TYPE vector].y	;y1
	fild camp.y	;-y3
	fsub
	fmul
	fild tmp[0*TYPE vector].y	;y1
	fild tmp[1*TYPE vector].y	
	fsub	;-y2
	fild tmp[0*TYPE vector].x	;x1
	fild camp.x	
	fsub	;-x3
	fmul
	fsub
	fchs
	fld den
	fdiv
	fild zero	;0 < u
	fcomip ST(0), ST(1)
	jae intersect_ret
	;make vector and get length
	;x = x1 + t * (x2 - x1) - camx
	mov eax, tmp[0*TYPE vector].x
	mov tmp[3*TYPE vector].x, eax	; x1
	fild tmp[1*TYPE vector].x	;x2
	fild tmp[0*TYPE vector].x	
	fsub	;-x1
	fld tval
	fmul
	fild camp.x	;-camx
	fsub
	push eax
	fisttp DWORD PTR [esp]	;save float to stack as int
	pop eax
	add tmp[3*TYPE vector].x, eax	;+ t*(x2-x1)
	;y = y1 + t * (y2 - y1) - camy
	mov eax, tmp[0*TYPE vector].y
	mov tmp[3*TYPE vector].y, eax	; y1
	fild tmp[1*TYPE vector].y
	fild tmp[0*TYPE vector].y
	fsub
	fld tval 
	fmul
	fild camp.y
	fsub
	push eax
	fisttp DWORD PTR [esp]	;save float to stack as int
	pop eax
	add tmp[3*TYPE vector].y, eax	;+ t*(y2-y1)
	mov eax, tmp[3*TYPE vector].y
	mov eax, tmp[3*TYPE vector].x
	mov edi, 3
	call getlength
	mov ebx, eax
intersect_ret:
	mov eax, ebx
	ret
intersect ENDP

;--------------------------------------------
ctoi PROC uses ebx ecx edx
; convert screnn buffer coordinates to index 
; Input: DL=X coor, DH=Y coor
; Output: EAX=index for screen buffer (pixels)
;--------------------------------------------
	mov eax, SCREEN_W+2
	movzx ebx, dh
	movzx ecx, dl
	mul ebx
	add eax, ecx
	ret
ctoi ENDP

;--------------------------------------------
wctoi PROC uses ebx ecx edx
; convert world coordinates to index
; Input: DL=X coor, DH=Y coor
; Output: EAX=index for world
;--------------------------------------------
	mov eax, WORLD_X
	movzx ebx, dh
	movzx ecx, dl
	mul ebx
	add eax, ecx
	ret
wctoi ENDP

;--------------------------------------------
wctorc PROC uses eax ebx ecx edx
; convert world coordinates to real coordinates
; Input: DL=X coor, DH=Y coor
; Output: tmp[0]=position vector
;--------------------------------------------
	mov ecx, edx
	mov ebx, BLOCK_SIZE
	movzx eax, cl
	mov edx, 0
	mul ebx
	mov tmp[0].x, eax
	movzx eax, ch
	mov edx, 0
	mul ebx
	mov tmp[0].y, eax
	ret
wctorc ENDP

;--------------------------------------------
getwc PROC uses eax ebx ecx
; get current world coordinates 
; Input: camp=camera position
; Output: DL=X coor, DH=Y coor
;--------------------------------------------
	mov eax, camp.x
	mov edx, 0
	mov ecx, BLOCK_SIZE
	div ecx
	mov ebx, eax
	mov eax, camp.y
	mov edx, 0
	div ecx
	mov dh, al
	mov dl, bl
	ret
getwc ENDP

;--------------------------------------------
sqrt PROC uses ebx ecx edx esi
; calculate floored square root
; Input: EAX
; Output: EAX
;--------------------------------------------
	mov ebx, eax
	shr eax, 1
	mov esi, 1
sqrtL:
	push eax
	mov eax, esi
	mov edx, 0
	mul esi
	mov ecx, eax
	pop eax
	cmp	ecx, ebx	;if n*n>=target : return
	jae sqrtB
	inc esi
	cmp esi, eax
	jbe sqrtL
sqrtB:
	mov eax, esi
	ret
sqrt ENDP

;--------------------------------------------
getlength PROC uses ebx ecx edx edi
; get length of a vector(tmp[edi])
; Input: EDI=index of target vector in tmp
; Output: EAX
;--------------------------------------------
	mov eax, tmp[edi*TYPE vector].x
	mov ecx, eax
	mul eax
	mov ebx, eax
	mov eax, tmp[edi*TYPE vector].y
	mov ecx, eax
	mul eax
	add eax, ebx
	call sqrt	;result in eax
	ret
getlength ENDP

END main