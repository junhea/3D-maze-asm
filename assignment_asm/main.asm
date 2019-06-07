INCLUDE Irvine32.inc

SCREEN_W equ 120    ;출력 화면 넓이
SCREEN_H equ 30     ;출력 화면 높이
BLOCK_SIZE equ 100  ;블럭 사이즈
WORLD_X equ 10      ;월드 x 크기
WORLD_Y equ 10     ;월드 y 크기
VD equ 300		    ;가시 거리
FOV equ 120			;가시 각도
SCREEN_D equ 30     ;카메라 포인트에서 화면 까지의 수직 거리
V_SIZE equ 8

vector STRUCT       ;벡터
	x DWORD 0
	y DWORD 0
vector ENDS

point STRUCT
	x BYTE 0
	y BYTE 0
	off DWORD 0
point ENDS



.data
    wall_t BYTE "###]]/||;:......                                ",0      ;벽 텍스쳐
    floor_t BYTE "    .^",0      ;바닥 텍스쳐
    pixels BYTE SCREEN_W*SCREEN_H DUP(' '),0    ;픽셀 버퍼
	zero REAL8 0.0
	stage BYTE 0

	world BYTE WORLD_X * WORLD_Y DUP(0)


	;미리 계산한 각도별 방향 벡터 & 단위벡터
	angles	vector <100,0>,<99,1>,<99,3>,<99,5>,<99,6>,<99,8>,<99,10>,<99,12>,<99,13>,<98,15>	;cos x * 100
			vector <98,17>,<98,19>,<97,20>,<97,22>,<97,24>,<96,25>,<96,27>,<95,29>,<95,30>,<94,32>
			vector <93,34>,<93,35>,<92,37>,<92,39>,<91,40>,<90,42>,<89,43>,<89,45>,<88,46>,<87,48>
			vector <86,50>,<85,51>,<84,52>,<83,54>,<82,55>,<81,57>,<80,58>,<79,60>,<78,61>,<77,62>
			vector <76,64>,<75,65>,<74,66>,<73,68>,<71,69>,<70,70>,<69,71>,<68,73>,<66,74>,<65,75>
			vector <64,76>,<62,77>,<61,78>,<60,79>,<58,80>,<57,81>,<55,82>,<54,83>,<52,84>,<51,85>
			vector <49,86>,<48,87>,<46,88>,<45,89>,<43,89>,<42,90>,<40,91>,<39,92>,<37,92>,<35,93>
			vector <34,93>,<32,94>,<30,95>,<29,95>,<27,96>,<25,96>,<24,97>,<22,97>,<20,97>,<19,98>
			vector <17,98>,<15,98>,<13,99>,<12,99>,<10,99>,<8,99>,<6,99>,<5,99>,<3,99>,<1,99>
			vector <-1,100>,<-2,99>,<-4,99>,<-6,99>,<-7,99>,<-9,99>,<-11,99>,<-13,99>,<-14,99>,<-16,98>
			vector <-18,98>,<-20,98>,<-21,97>,<-23,97>,<-25,97>,<-26,96>,<-28,96>,<-30,95>,<-31,95>,<-33,94>
			vector <-35,93>,<-36,93>,<-38,92>,<-40,92>,<-41,91>,<-43,90>,<-44,89>,<-46,89>,<-47,88>,<-49,87>
			vector <-51,86>,<-52,85>,<-53,84>,<-55,83>,<-56,82>,<-58,81>,<-59,80>,<-61,79>,<-62,78>,<-63,77>
			vector <-65,76>,<-66,75>,<-67,74>,<-69,73>,<-70,71>,<-71,70>,<-72,69>,<-74,68>,<-75,66>,<-76,65>
			vector <-77,64>,<-78,62>,<-79,61>,<-80,60>,<-81,58>,<-82,57>,<-83,55>,<-84,54>,<-85,52>,<-86,51>
			vector <-87,50>,<-88,48>,<-89,46>,<-90,45>,<-90,43>,<-91,42>,<-92,40>,<-93,39>,<-93,37>,<-94,35>
			vector <-94,34>,<-95,32>,<-96,30>,<-96,29>,<-97,27>,<-97,25>,<-98,24>,<-98,22>,<-98,20>,<-99,19>
			vector <-99,17>,<-99,15>,<-100,13>,<-100,12>,<-100,10>,<-100,8>,<-100,6>,<-100,5>,<-100,3>,<-100,1>
			vector <-100,-1>,<-100,-2>,<-100,-4>,<-100,-6>,<-100,-7>,<-100,-9>,<-100,-11>,<-100,-13>,<-100,-14>,<-99,-16>
			vector <-99,-18>,<-99,-20>,<-98,-21>,<-98,-23>,<-98,-25>,<-97,-26>,<-97,-28>,<-96,-30>,<-96,-31>,<-95,-33>
			vector <-94,-35>,<-94,-36>,<-93,-38>,<-93,-40>,<-92,-41>,<-91,-43>,<-90,-44>,<-90,-46>,<-89,-47>,<-88,-49>
			vector <-87,-50>,<-86,-52>,<-85,-53>,<-84,-55>,<-83,-56>,<-82,-58>,<-81,-59>,<-80,-61>,<-79,-62>,<-78,-63>
			vector <-77,-65>,<-76,-66>,<-75,-67>,<-74,-69>,<-72,-70>,<-71,-71>,<-70,-72>,<-69,-74>,<-67,-75>,<-66,-76>
			vector <-65,-77>,<-63,-78>,<-62,-79>,<-61,-80>,<-59,-81>,<-58,-82>,<-56,-83>,<-55,-84>,<-53,-85>,<-52,-86>
			vector <-50,-87>,<-49,-88>,<-47,-89>,<-46,-90>,<-44,-90>,<-43,-91>,<-41,-92>,<-40,-93>,<-38,-93>,<-36,-94>
			vector <-35,-94>,<-33,-95>,<-31,-96>,<-30,-96>,<-28,-97>,<-26,-97>,<-25,-98>,<-23,-98>,<-21,-98>,<-20,-99>
			vector <-18,-99>,<-16,-99>,<-14,-100>,<-13,-100>,<-11,-100>,<-9,-100>,<-7,-100>,<-6,-100>,<-4,-100>,<-2,-100>
			vector <0,-100>,<1,-100>,<3,-100>,<5,-100>,<6,-100>,<8,-100>,<10,-100>,<12,-100>,<13,-100>,<15,-99>
			vector <17,-99>,<19,-99>,<20,-98>,<22,-98>,<24,-98>,<25,-97>,<27,-97>,<29,-96>,<30,-96>,<32,-95>
			vector <34,-94>,<35,-94>,<37,-93>,<39,-93>,<40,-92>,<42,-91>,<43,-90>,<45,-90>,<46,-89>,<48,-88>
			vector <49,-87>,<51,-86>,<52,-85>,<54,-84>,<55,-83>,<57,-82>,<58,-81>,<60,-80>,<61,-79>,<62,-78>
			vector <64,-77>,<65,-76>,<66,-75>,<68,-74>,<69,-72>,<70,-71>,<71,-70>,<73,-69>,<74,-67>,<75,-66>
			vector <76,-65>,<77,-63>,<78,-62>,<79,-61>,<80,-59>,<81,-58>,<82,-56>,<83,-55>,<84,-53>,<85,-52>
			vector <86,-51>,<87,-49>,<88,-47>,<89,-46>,<89,-44>,<90,-43>,<91,-41>,<92,-40>,<92,-38>,<93,-36>
			vector <93,-35>,<94,-33>,<95,-31>,<95,-30>,<96,-28>,<96,-26>,<97,-25>,<97,-23>,<97,-21>,<98,-20>
			vector <98,-18>,<98,-16>,<99,-14>,<99,-13>,<99,-11>,<99,-9>,<99,-7>,<99,-6>,<99,-4>,<99,-2>

	walk	vector <5,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>,<4,0>	;cos x * 5
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
    
	camp vector <150,150>
	camd DWORD 0	;카메라 방향

    tmp vector 10 DUP(<>)     ;벡터 계산을 위한 저장소
	clear_msg BYTE 120 DUP('='), 120 DUP(' '), 54 DUP(' '), "stage clear!", 54 DUP(' '),120 DUP(' '), 120 DUP('='), 0
	title_msg BYTE 120*13 DUP ('.'), 120 DUP (' '), 56 DUP (' '), "3D MAZE", 57 DUP (' '), 120 DUP (' '), 120*14 -1 DUP ('.'), 0
	help_msg BYTE "INSTRUCTIONS:", 0dh, 0ah, 0ah, "W,A,S,D : move", 0dh, 0ah, "<,> : change camera angle", 0dh, 0ah
			BYTE "M : view minimap", 0dh, 0ah, "Q : exit to menu", 0dh, 0ah, "R : reset current position", 0dh, 0ah
			BYTE "H : show this message", 0dh, 0ah, "  Map symbols:", 0dh, 0ah, "  O : current position", 0dh, 0ah
			BYTE "  G : goal", 0dh, 0ah, "  # : wall", 0dh, 0ah, 0
	minimap_msg BYTE "Generated map :  ", 0dh, 0ah, 0
	half_line BYTE 60 DUP (' '), 0dh, 0ah, 0
	goal point <>

.code

main PROC
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

gamemap PROC uses eax ecx edx
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

campus PROC uses eax ecx
	mov eax, camd
	sub eax, 60
	cmp eax, 0
	jge campus_p
		add eax, 360
campus_p:
	mov ecx, FOV-1
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
		.ENDIF
		inc eax
		dec ecx
	jmp campus_loop
campus_loop_done:
	ret
campus ENDP

menusplash PROC uses edx
	mov edx, 0 
	call gotoxy
	mov edx, offset title_msg
	call writestring
	mov edx, 0f30h
	call gotoxy
	call waitmsg
	ret
menusplash ENDP

gameinit PROC uses eax ecx edx
	call clrscr
	mov camp.x, 150
	mov camp.y, 150
	mov camd, 0
	mov stage, 0
	mov goal.x, 0
	mov goal.y, 0
	mov goal.off, 0
	call generatemap
	;print instructions and minimap
	call gamemap
	call crlf
	mov edx, offset help_msg
	call writestring
	call waitmsg
	ret
gameinit ENDP


generatemap PROC uses eax ebx ecx edx esi
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
		;call gamemap
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
			;mov to next cell, mark & push previous cell	(dx)
			push dx
			mov dx, bx
		.ENDIF
		;call gamemap
		;call dumpregs
		;call waitmsg
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
;input edx:coor
;output al: 0=available, else=unavailable
checkcell PROC uses ebx edx
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


gameclear PROC uses edx
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

debug PROC uses eax edx
	mov edx, 0
	call gotoxy
	mov eax, camp.x
	call writeint
	call crlf
	mov eax, camp.y
	call writeint
	call crlf
	mov eax, camd
	call writeint
	ret
debug ENDP

getinput PROC uses eax ebx
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

helpmsg PROC uses ecx edx
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

move PROC uses eax ebx ecx edx
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

render PROC uses ecx edx
	mov edx, 0
	mov pixels[SCREEN_W*SCREEN_H-1],dl
	call gotoxy
	mov edx, offset pixels
	call writestring
	ret
render ENDP

;MakeFloor
;not much of a calculation here
makefloor PROC uses eax ebx ecx edx
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

;MakeWall
makewall PROC uses eax ebx ecx edx esi edi
	mov esi, camd
	sub esi, FOV/2
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


;input: eax = angle vector index 
;output: ebx = max length
shootray PROC uses eax ecx edx esi
	mov esi, eax
	mov ebx, -1
	;loop through all blocks
	mov edx, 0
	.WHILE dh < WORLD_Y	;loop y
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

;check intersection for tmp[0]->tmp[1] & camp+angles[esi]
;output: eax = distance
intersect PROC uses ebx ecx edx edi esi
	;if intersects, return distance (-1 = no intersection)
	mov ebx, -1
	mov eax, angles[esi*TYPE vector].x

    mov tmp[2*TYPE vector].x, eax
	mov eax, angles[esi*TYPE vector].y
	mov tmp[2*TYPE vector].y, eax

	mov eax, camp.x
    add tmp[2*TYPE vector].x, eax
	mov eax, camp.y
    add tmp[2*TYPE vector].y, eax

	;den = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);
	mov eax, tmp[0*TYPE vector].x	;x1
	sub eax, tmp[1*TYPE vector].x	;-x2
	mov ecx, camp.y	;y3
	sub ecx, tmp[2*TYPE vector].y	;-y4
	mov edx, 0
	mul ecx
	push eax

	mov eax, tmp[0*TYPE vector].y	;y1
	sub eax, tmp[1*TYPE vector].y	;-y2
	mov ecx, camp.x	;x3
	sub ecx, tmp[2*TYPE vector].x	;-x4
	mov edx, 0
	mul ecx

	pop ecx
	sub ecx, eax

	.IF ecx == 0	;if den == 0
		jmp intersect_ret
	.ENDIF
	
	push ecx	;den
	finit
	fild DWORD PTR [esp]
	fwait
	pop ecx

	;t = (x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4) / den
	mov eax, tmp[0*TYPE vector].x	;x1
	sub eax, camp.x	;-x3
	mov ecx, camp.y	;y3
	sub ecx, tmp[2*TYPE vector].y	;-y4
	mov edx, 0
	mul ecx
	push eax

	mov eax, tmp[0*TYPE vector].y	;y1
	sub eax, camp.y	;-y3
	mov ecx, camp.x	;x3
	sub ecx, tmp[2*TYPE vector].x	;-x4
	mov edx, 0
	mul ecx

	pop ecx
	sub ecx, eax

	push ecx
	fild DWORD PTR [esp]
	fwait
	pop ecx

	fdiv ST(0), ST(1)	;ST(0) = t
	fild zero	;0 < t
	fwait
	test al, al
	fcomip ST(0), ST(1)
	jae intersect_ret

	mov eax, 1
	push eax
	fild DWORD PTR [esp]	;1 > t
	fwait
	pop eax
	test al, al
	fcomip ST(0), ST(1)
	jbe intersect_ret
	
	;u = -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / den;
	mov eax, tmp[0*TYPE vector].x	;x1
	sub eax, tmp[1*TYPE vector].x	;-x2
	mov ecx, tmp[0*TYPE vector].y	;y1
	sub ecx, camp.y	;-y3
	mov edx, 0
	mul ecx
	push eax

	mov eax, tmp[0*TYPE vector].y	;y1
	sub eax, tmp[1*TYPE vector].y	;-y2
	mov ecx, tmp[0*TYPE vector].x	;x1
	sub ecx, camp.x					;-x3
	mov edx, 0
	mul ecx

	pop ecx
	sub ecx, eax

	push ecx
	fild DWORD PTR [esp]	
	fchs
	fwait
	pop ecx

	fdiv ST(0), ST(2)	;ST(0) = u, ST(1) = t, ST(2) = den

	fild zero	;0 < u
	fwait
	test al, al
	fcomip ST(0), ST(1)
	jae intersect_ret

	;make vector and get length
	;x = x1 + t * (x2 - x1) - camx
	mov eax, tmp[0*TYPE vector].x

	mov tmp[3*TYPE vector].x, eax	; x1
	mov eax, tmp[1*TYPE vector].x
	sub eax, tmp[0*TYPE vector].x
	push eax
	fild DWORD PTR [esp]	;ST(0) = x2-x1, ST(1) = u, ST(2) = t, ST(3) = den
	fmul ST(0), ST(2)
	fisttp DWORD PTR [esp]	;save float to stack as int
	fwait
	pop eax
	add tmp[3*TYPE vector].x, eax	;+ t*(x2-x1)
	
	;y = y1 + t * (y2 - y1) - camy
	mov eax, tmp[0*TYPE vector].y
	mov tmp[3*TYPE vector].y, eax	; y1
	mov eax, tmp[1*TYPE vector].y
	sub eax, tmp[0*TYPE vector].y
	push eax
	fild DWORD PTR [esp]	;ST(0) = y2-y1, ST(1) = u, ST(2) = t, ST(3) = den
	
	fmul ST(0), ST(2)
	fisttp DWORD PTR [esp]	;save float to stack as int
	fwait
	pop eax
	add tmp[3*TYPE vector].y, eax	;+ t*(y2-y1)

	mov eax, camp.x
	sub tmp[3*TYPE vector].x, eax
	mov eax, camp.y
	sub tmp[3*TYPE vector].y, eax

	mov eax, tmp[3*TYPE vector].x
	mov eax, tmp[3*TYPE vector].y
	
	mov edi, 3
	call getlength
	mov ebx, eax
intersect_ret:
	fwait
	mov eax, ebx
	ret
intersect ENDP



;CoordinatesToIndex
;output: eax = 어레이 인덱스 for pixels
;input: dl = x
;input: dh = y
ctoi PROC uses ebx ecx edx
	mov eax, SCREEN_W
	movzx ebx, dh
	movzx ecx, dl
	mul ebx
	add eax, ecx
	ret
ctoi ENDP

;WorldCoordinatesToIndex
;output: eax = 어레이 인덱스 for world
;input: dl = x
;input: dh = y
wctoi PROC uses ebx ecx edx
	mov eax, WORLD_X
	movzx ebx, dh
	movzx ecx, dl
	mul ebx
	add eax, ecx
	ret
wctoi ENDP

;world coordinates to read coordinates
;input: dh = y, dl = x
;output: tmp[0]
wctorc PROC uses eax ebx ecx edx
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

;input: camp
;output: dl = x, dh = y
getwc PROC uses eax ebx ecx
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

;SquareRoot (floor)
;input/output = eax
sqrt PROC uses ebx ecx edx esi
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

;GetLength
;input: tmp[edi], esi(angle index)
;output: eax
getlength PROC uses ebx ecx edx edi
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
