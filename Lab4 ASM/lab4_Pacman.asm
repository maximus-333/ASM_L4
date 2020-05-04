#make_exe#
.186  ;for PUSHA/POPA

.stack 200h

;Le PacMan game.

.data
newl EQU 0Ah, 0Dh

FLD_W EQU 19 
FLD_H EQU 15
SCR_W EQU 80 ;Width of screen(80x25)

;empty playing field, only walls and corridors  
field db "ллллллллллллллллллл",
      db "л        л        л",
      db "л лл ллл л ллл лл л",
      db "л                 л",
      db "лллл л ллллл л лллл",                 
      db "л    л   л   л    л",
      db "л лл ллл   ллл лл л",                      
      db "л    л   л   л    л", 
      db "л лллл ллллл лллл л",     
      db "л                 л",
      db "лллл лл ллл лл лллл", 
      db "л        л        л",
      db "л ллл лл л лл ллл л",          
      db "л                 л",
      db "ллллллллллллллллллл"          
fieldIter dw 0

T_P_UP   EQU '+'
T_WALL   EQU 'л'
T_FLOOR  EQU ' '
E_PLAYER EQU 002  ;black face symbol
E_ENEMY  EQU 002  ;also black face symbol
E_UPG    EQU 004  ;crystal thing
E_FOOD   EQU 254  ;fat dot
DUR_UPG  EQU 20   ;active frames of an upgrade


;position - top-left symbol assumed as (0,0). Maybe will change that
;state values: 0 -good, 1 - (player)can attack. Enemy states not used
;dir values: 1XXb - stays still, 000b - up, 001b - right, 010b - down, 011b - left  
   ;alt dir values - in unitary code(0001 - up, 1000 - left)


;Structure: posy, posx, dir, state
player   db 7  ,7 ,4 ,0
enemy1   db 1  ,1 ,4 ,0
enemy2   db 13 ,13,4 ,0

upg1     db 1, 2, 0
upg2     db 3, 17, 0
upgTimer db 0

food1    db 6, 4
food2    db 6, 9     ;Can access all 3 values as array, which is handy
food3    db 9, 13
score    dw 0
strScore db "Score: ",    ;draw-ready string
   strScoreNum db "00000",'$' ;2nd pointer is for easy access to value

infoGameOver   db "GAME OVER", newl, '$'

randomByte db ?   ;Need to initialize it. System time will do

actVPage   db 0   ;Will use pages 0-1, for double buffering



.code
start:
   mov ax, @data    
   mov ds, ax
   mov es, ax
     
   
   ;Set video mode(80x25, 16 color)   
   mov ax, 0003h
   int 10h 
   
   ;Hide cursor
   mov ch, 20h
   mov ah, 1
   int 10h
   
   call drawBuffer   ;Initialize the world for navigation
    
   game_loop:
      call seedRandomByte
      call newRandomByte   
    
      ;Player control and movement
      call doControls
      lea bx, player
      call moveEntity
      
      ;Check if player is on one of upgrades, pick up if true
      lea bx, upg1
      CALL checkUpgrade
      lea bx, upg2
      CALL checkUpgrade
      
      ;Check if enemy is on player. Act according to player's upgrades
      lea bx, enemy1
      CALL checkEnemy
      lea bx, enemy2
      CALL checkEnemy
      
      ;Enemy movement
      lea bx, enemy1
      call doAI
      lea bx, enemy1
      call moveEntity
      lea bx, enemy2
      call doAI
      lea bx, enemy2
      call moveEntity 
      
      ;Additional player-enemy check to ensure collision
      lea bx, enemy1
      CALL checkEnemy
      lea bx, enemy2
      CALL checkEnemy
      
      ;Eat apples, update score
      call checkPoints         
       
      call updateTimer;counts down powerup time and removes it
      
      call drawBuffer ;Drawing of everything on screen
      
      ;Delay of 0.4 sec
      mov cx, 0006h
      mov dx, 1A80h 
      mov ah, 86h
      int 15h        ;BIOS delay function
      
      mov cx, 0000h ;for infinite cycle 
   loop game_loop   
   game_isOver:
    
   ;display game over screen
   
   ;clear the screen(by setting video mode again)
   mov ax, 0003h
   int 10h
   ;display info
   mov ah, 09h
   lea dx, infoGameOver
   int 21h
   lea dx, strScore
   int 21h
    
   ;return control to DOS. Better than INT 20h, which stops everything
   mov ah, 4Ch
   int 21h
   
   
   
    
;|-------------------------------------FUNCTIONS-------------------------------------|    
    
   ;-----Minor functions-----


;RNG - linear congruential generator
;Formula = Y=(a*X + c) mod m
;Selected values: a = 5, c = 1, m = 256. Not great, but ok
newRandomByte PROC
   pusha
   mov al, randomByte  ;load previous value
   xor ah, ah
   mov bx, 13 ;a
   mul bx
   add ax, 1 ;c
   add al, ah ;bit more random
   mov randomByte, al ;truncate to < m(8 bytes in this case)
   popa       
   ret
newRandomByte ENDP 

;Addition to previous procedure. Seeds RNG with system time, 
;to avoid repetitive behaviour. Call every cycle for good random
seedRandomByte PROC
   pusha
   mov ah, 2Ch
   int 21h     ;get time
   add DH, DL  ;add rogether minute, second, 1/100th of second to get value
   add DH, CL
   mov randomByte, dh
   popa
   ret   
seedRandomByte ENDP   


;Retrieve symbol from ACTIVE screen
;Input - DX for row-column(from 0)
;Output - raw, in AX(attribute-character)
getSymbol PROC
   ;push bh         
   mov bh, actVPage
   mov ah, 02h      
   int 10h  ;move cursor
   mov ah, 08h
   int 10h  ;read character and attribute
   ;pop bh
   ret
getSymbol ENDP   

;"Look in a direction"
;Inputs - center pos in DX(row-col), direction in AH
;Output - symbol from that direction in AL (or 0 if no direction)
lookDir PROC
   push bx
   push dx
   
   
   mov al, 00h 
   test ah, 04h ;in case no dir specified, return 0
   jnz ld_end
   
   push ax
   
   and ah, 03h
   xor bx, bx
   mov bl, ah
   shl bl, 1 ;x2 because array of words
   mov ax, cs:ld_jumpTable[bx]
   jmp ax      ;switch() through jump table
   
   ld_c0:
      dec dh
      jmp ld_switchEnd
   ld_c1:
      inc dl
      jmp ld_switchEnd
   ld_c2:
      inc dh
      jmp ld_switchEnd
   ld_c3:
      dec dl
   ld_switchEnd:
   
   mov bh, actVPage
   mov ah, 02h
   int 10h  ;set cursor
   mov ah, 08h
   int 10h  ;get symbol
   
   mov dl, al;Need both to preserve AH(direction) and update AL(with symbol in that direction)
   pop ax    ;For that, use DX. It gets reverted anyway  
   mov al, dl
   
   ld_end:
   pop dx
   pop bx
   ret
   ;jump table, for simpler switch
   ld_jumpTable dw ld_c0, ld_c1, ld_c2, ld_c3

lookDir ENDP


;Converts unitary to normal(aka digital decoder)(0100 -> 2, 0001 -> 0)
;will stop at rightmost '1' it encounters
;Input - in AL
;Output - in AH
decodeUnitary PROC
   push cx
   xor ah, ah
   mov cx, 03h
   dun_loop:
      test al, 01h
      jnz dun_loopEnd
      inc ah
      shr al, 1
   loop dun_loop
   dun_loopEnd:
   pop cx
   ret    
decodeUnitary ENDP


;(copied from Lab3, reworked for unsigned word)
;converts 2-byte UNSIGNED number into 5-char string (without '$'!)
;Input - signed int in AX, pointer to string in SI
;Output - in string at SI
;(registers don't change)
proc wordToStr
   pusha
   
   mov bx, 10000
   mov cx, 05h
   wtsLoop:
      xor dx, dx
      div bx   
      add ax, '0' 
      mov [si], al
      inc si 
      
      mov ax, dx

      push ax    
      mov ax, bx
      mov bx, 10
      xor dx, dx
      div bx
      mov bx, ax
      pop ax 
   loop wtsLoop 
   
   popa
   ret
endp wordToStr



   ;-----Major functions-----


;Control of pacman 
;Algorithm: get keyboard input(arrows only), get surrounding 4 tiles.
;If change of direction required, attempt that. If failed, keep old direction.
;Actual movement is accomplished in another function, this one just sets direction!

;ARROW KEY VALUES - AL stays 00h, AH is: 48h for Up, 50h for Down, 4Bh for Left, 4Dh for Right    
doControls PROC    
   pusha
   
   ;if not arrowkey, repeat till found or empty
   mov cx, 0000h
   dc_inpCycle:
      mov ax, 0100h
      int 16h        ;peek character
      jz dc_sameDir  ;haven't found any arrow keys. Exit procedure
         xor ax, ax 
         int 16h     ;if character exists, read it
         test al, al ;returns NULL symbol if arrows(maybe also on other special symbols)
         jz dc_inpEnd;Check if arrow keys. If not, back to reading
   loop dc_inpCycle
   dc_inpEnd:
   push ax
   mov ax, 0C00h
   int 21h  ;flushing input buffer
   pop ax
   
   ;map value from AH to direction bits
   cmp ah, 48h    ;up
   jne dc_akR
      mov ah, 00h
      jmp dc_akEnd
   dc_akR:
   cmp ah, 4Dh    ;right
   jne dc_akD
      mov ah, 01h
      jmp dc_akEnd
   dc_akD:            
   cmp ah, 50h    ;down
   jne dc_akL
      mov ah, 02h
      jmp dc_akEnd
   dc_akL:
   cmp ah, 4Bh    ;left
   jne dc_inpCycle;Found some special symbol, but it wasn't arrow key. Go back to reading
      mov ah, 03h
      jmp dc_akEnd
   dc_akNone:
   mov ah, 04h    
   jmp dc_sameDir   
   dc_akEnd:   
   
   ;AH now has direction.
   ;Check if there's no wall there
   mov dh, player[0] ;load player's position into dx
   mov dl, player[1]
   call lookDir   ;return whatever is in that direction
   
   ;If it's a wall, don't change direction
   cmp al, T_WALL         
   je dc_sameDir
      mov player[2], ah ;update direction
   dc_sameDir:
   
   
   popa 
   ret
doControls ENDP     
   

;Brain of the enemy.
;Makes decision once per a 3-4 way split(not rly)
;Will look at player's relative position and turn towards him
;Input - source entity in BX. Player entity is hardwired as target
;Output - updated direction byte for that entity
doAI PROC
   pusha
      
   ;check surroundings
   mov cx, 0004h
   mov dh, [bx]
   mov dl, [bx+1]
   mov ah, cl
   mov cs:ai_dirAmt, 00h   ;reset values
   mov cs:ai_freeDirs, 00h
   ai_loop:        ;loop through 4 directions(L-D-R-U)
      dec ah   
      call lookDir 
      
      cmp al, T_WALL
      je ai_wall
         xor cs:ai_freeDirs, 1   ;set direction bit if it's clear
         inc cs:ai_dirAmt ;increase amount of free directions
      ai_wall:
      shl cs:ai_freeDirs, 1 
   loop ai_loop
   shr cs:ai_freeDirs, 1   ;compensate
   ;got an array of paths   
   
                           
   ;Part that handles navigation in corridors and corners
   cmp cs:ai_dirAmt, 2
   jg ai_dirChoice 
   jl ai_1dir
   ;2 directions. This may be in straight corridor or on a L-turn. In latter case, take a turn
   mov cl, [bx+2]   ;get enemy's facing direction
   ;need to convert it to unitary
   mov al, 1 
   shl al, cl   ;got the facing direction as a bit
   test al, 0F0h
   jnz ai_dirChoice ;if enemy is standing still, pick direction
   test cs:ai_freeDirs, al
   jnz ai_end   ;2 directions. If forward is free, continue moving
   ;If not, enemy is in corner. Turn and continue
   ;For that, remove 'back' direction bit
   mov ah, al
   shl ah, 4
   or al, ah      ;copy low 4 bits into top 4 bits
   shr al, 2      
   and al, 0Fh    ;got inverse direction
   xor cs:ai_freeDirs, al ;get remaining direction
    ai_1dir:   ;If only one free direction, go straight to decoding
   mov al, cs:ai_freeDirs
   CALL decodeUnitary
   mov [bx+2], ah ;enemy will go in the only remaining direction
   jmp ai_end
           
   
   ;Part that handles change of directon on splits        
   ;Algorithm: determine quadrant player is in.
   ;Next, look at available paths. If two 'useful' ones are free,
   ;choose one at random. If only one is 'useful', pick it.
   ;If there's no 'useful' directions, choose one at random.
   ai_dirChoice:                                            
   
   mov ch, player[0]
   mov cl, player[1]
   sub ch, dh
   sub cl, dl  ;got vector enemy -> player
   ;Determine quadrant (here they go clockwise, from zero)
   mov al, 00h
   neg dh
   cmp ch, 00h ;sign of y
   jle ai_qa
      inc al      ;if y>0
   ai_qa:
   cmp cl, 00h ;sign of x
   jge ai_qb 
      xor al, 03h ;if x<0
   ai_qb:
   mov cs:ai_qdrt, al
   ;Quadrant known. Need to pick a direction. 
   ;make a mask of 'useful' directions from quadrant
   mov al, 03h
   mov cl, cs:ai_qdrt
   shl al, cl
   mov ah, al 
   mov cx, 4   ;this bit is for rollover on q3(supposed to give 1001 mask)
   shr ah, cl
   xor al, ah
   and al, 0Fh
   ;check if have 2(or 0) 'useful' dirs to choose from. Use Parity Flag for that
   and al, cs:ai_freeDirs
   jpe ai_evenDirs
      ;One direction. Pick it and leave. Need to decode it first
      call decodeUnitary
      mov [bx+2], ah ;set direction
      jmp ai_end
   ai_evenDirs:
      ;If zero 'useful' dirs, pick random
      call newRandomByte ;Get fresh value first
      xor cx, cx
      mov cl, randomByte
      test al, al
      jz ai_noDirs
         ;Have 2 'useful' dirs. Use RNG
         mov dx, 05h ;dir bits are next to eachother. Use mask to get one
         and cl, 01h ;get CX = 1 or 0
         shl dl, cl  ;shift or no shift at random (hope CX=0 works)
         and al, dl  ;select one of two options
         call decodeUnitary
         mov [bx+2], ah ;set direction
         
         jmp ai_end
      ai_noDirs:
      ;Pick random direction from available
      and cl, 03h ;get value 0-3
      mov ah, al
      shl ah, 4
      or al, ah   ;clone half of AL in itself
      rol al, cl  ;rotate it 0-3 places
      call decodeUnitary 
      add ah, cl  ;get random VALID direction
      mov [bx+2], ah ;set direction
   ai_end:
   
   popa
   ret
   
   ai_dirAmt   db 0
   ai_freeDirs db 0  ;4 lesser bits used for storage (0000-L D R U)
   
   ai_qdrt     db ?
   
   
doAI ENDP




;Moves an entity to a new place. That's it.
;Main reason of procedure - gets used for player and enemies.
;Input - pointer to entity data in BX. Direction must be updated earlier
;Also stops on head collision with walls
moveEntity PROC
   push ax
   
   ;check to not run into a wall
   mov dh, [bx]
   mov dl, [bx+1] ;load posy, posx, dir
   mov ah, [bx+2]
   call lookDir
   cmp al, T_WALL
   jne me_freeWay
      mov [bx+2], 04h   ;set direction to 'stop'
   me_freeWay:
   
   xor ax, ax
   mov al, [bx+2] ;load direction info
   test al, 04h
   jnz me_end     ;stay in place
   
   ;switch() with jump table
   and al, 03h
   shl al, 1
   push bx
   mov bx, ax
   mov ax, cs:me_jumpTable[bx] ;load address from jump table on index from AX
   pop bx
   jmp ax   
   
   me_c0:
      dec [bx]   
      jmp me_end
   me_c1:
      inc [bx+1]
      jmp me_end
   me_c2:
      inc [bx]
      jmp me_end
   me_c3:
      dec [bx+1]
   
   me_end:  ;plz
   
   pop ax
   ret
   ;jump table for switch
   me_jumpTable dw me_c0, me_c1, me_c2, me_c3   
moveEntity ENDP   



;Draws everything to 2nd(invisible) buffer,
;swaps it with current one. Should be easy
drawBuffer PROC

   ;Maybe draw elements in different procedures. Maybe
   
   
   xor actVPage, 01h ;swap to back buffer
   mov bh, actVPage 
   mov bl, 08h ;black background, dark gray font color
   
   ;iterate on 2 coordinates, from (0,0)
   mov fieldIter, 00h
   xor dx, dx
   
   mov cx, FLD_H ;iterate through rows
   db_rowLoop:
      push cx
      xor dl, dl  ;start from 1st column of row
      
      mov cx, FLD_W ;iterate through symbols of a row
      db_colLoop:
         mov ah, 02h
         int 10h  ;set cursor to position
         
         push cx
         ;push si
         
         mov si, fieldIter
         mov al, field[si]
         mov ah, 09h
         mov cx, 1
         int 10h  ;write character to buffer
          
         ;pop si
         pop cx
          
         inc fieldIter
         inc dl
      loop db_colLoop
      pop cx
      inc dh
   loop db_rowLoop
   
   ;Finished drawing the field. Now draw player and enemies(and upgrades, later)
   ;Definitely need to wrap these in procedures
   
   ;drawing player
   mov dh, player[0]
   mov dl, player[1]
   and bl, 0F0h;clear font color(leaving background color intact)
   ;Change player's color if he's upgraded
   cmp player[3], 0
   jnz db_plUpgd
      or bl, 0Dh ;usually player is pink
      jmp db_plDraw
   db_plUpgd:
      or bl, 0Bh ;powerup makes player light cyan
   db_plDraw:      
   mov ah, 02h
   int 10h  ;set cursor
   mov al, E_PLAYER
   mov cx, 1
   mov ah, 09h
   int 10h  ;put player symbol
   
   
   ;draw upgrades
   and bl, 0F0h;clear font color
   or bl, 03h ;set upgrade color to cyan
   mov dh, upg1[0]
   mov dl, upg1[1]
   mov ah, 02h
   int 10h  ;set cursor
   mov al, E_UPG
   mov cx, 1
   mov ah, 09h
   int 10h  ;put symbol 
   
   mov dh, upg2[0]
   mov dl, upg2[1]
   mov ah, 02h
   int 10h  ;set cursor
   mov al, E_UPG
   mov cx, 1
   mov ah, 09h
   int 10h  ;put symbol
   
   
   ;draw food
   and bl, 0F0h   ;clear foreground color
   or bl, 0Ah  ;set light green food color
   mov cx, 03  ;cycle through all 3 pieces
   mov al, E_FOOD
   lea di, food1
   db_foodLoop:
      mov dh, [di]
      inc di
      mov dl, [di]
      inc di
      mov ah, 02h
      int 10h  ;set cursor
      push cx
      mov cx, 1
      mov ah, 09h
      int 10h  ;put symbol
      pop cx
   loop db_foodLoop
   
   
   ;draw enemies
   and bl, 0F0h;clear font color
   or bl, 0Ch ;set enemy color to yellow
   ;1st one
   mov dh, enemy1[0]
   mov dl, enemy1[1]
   mov ah, 02h
   int 10h ;move cursor
   mov al, E_ENEMY
   mov cx, 1
   mov ah, 09h
   int 10h ;put symbol
   ;2nd one
   mov dh, enemy2[0]
   mov dl, enemy2[1]
   mov ah, 02h
   int 10h ;move cursor
   mov ah, 09h
   int 10h ;put symbol


   ;Draw current score
   ;Place - right below the field.
   ;Update string in memory:
   mov ax, score
   lea si, strScoreNum
   CALL wordToStr ;update 5 digits of score string
   ;set starting position
   mov dh, FLD_H  ;position - just below field, in corner
   mov dl, 00h
   ;write in cycle until '$' symbol
   mov bl, 0Fh ;set text to default(i guess)
   xor cx, cx  ;no limit on string length
   lea si, strScore
   db_scoreLoop:
      mov al, [si] ;load symbol to put in buffer
      cmp al, '$'
      je db_scoreLoopEnd
      mov ah, 02h
      int 10h        ;set cursor position
      push cx
      mov cx, 1
      mov ah, 09h
      int 10h  ;put symbol
      pop cx
      inc si ;go to next symbol in string
      inc dl
   loop db_scoreLoop
   db_scoreLoopEnd:
   
   
   ;show new video page 
   mov al, bh
   mov ah, 05h
   int 10h
   
   ret
drawBuffer ENDP   


;Move/place an upgrade to random free tile
;Algorithm: pick a point(2 coords).
;If not empty, pick another point. Simple as that   
;Input - pointer to entity in BX 
moveUpgrade PROC
   pusha
   su_getPoint:
   ;get random (x,y) into (CL,CH)
   xor ax, ax
   call newRandomByte
   mov al, randomByte
   mov dl, FLD_W - 2
   div dl
   inc ah
   mov cl, ah  
   
   xor ax, ax
   call newRandomByte
   mov al, randomByte
   mov dl, FLD_H - 2
   div dl
   inc ah
   mov ch, ah
   
   ;look at picked spot
   push bx
   mov dx, cx
   mov bh, actVPage
   mov ah, 02h
   int 10h  ;set cursor pos
   mov ah, 08h
   int 10h  ;get symbol
   pop bx
   cmp al, T_FLOOR
   jne su_getPoint   ;If not clear tile, try again
   
   ;store coordinates
   mov [bx], ch
   mov [bx+1], cl

   ;idk, should be enough
   
   popa
   ret
moveUpgrade ENDP   


;Checks if player is on provided upgrade. If yes, 
;relocate upgrade, mark in player's state
;Input - ptr to upgrade in BX
;Output - updated player state and upgrade pos
checkUpgrade PROC
   pusha
   mov ch, [bx]
   mov cl, [bx+1]
   xor ch, player[0]
   xor cl, player[1]
   test cx, cx
   jnz gu_end
      ;Coordinates match. But, don't pick up if have upgrade already
      cmp player[3], 00h
      jne gu_end
      
      mov player[3], 01h   ;set player state
      call moveUpgrade     ;reposition current upgrade
   gu_end:
   popa
   ret
checkUpgrade ENDP


;Check if player collides with provided enemy. If yes,
;check player's status. Then act accordingly
checkEnemy PROC
   pusha
   
   mov ch, [bx]
   mov cl, [bx+1]
   xor ch, player[0]
   xor cl, player[1]
   test cx, cx
   jnz ce_end
      ;Coordinates match. One eats the other
      
      mov ch, player[3] ;player's upgrade status
      test ch, ch
      jnz ce_pupg 
         ;player is defenceless. Means end of the game
         
         ;For now, go out of loop. Maybe add "you died" screen later
         jmp game_isOver
         
      ce_pupg:
      ;player 'ate' enemy. Enemy reappears in some other place
      ;Cleverly reuse a procedure meant for moving upgrades
      call moveUpgrade     ;move enemy to a new position
      add score, 5   ;add 5 score points
   ce_end:
   popa
   ret
checkEnemy ENDP   


;If player's upgrade state is set and T=0, set timer.
;When T=1, remove upgrade. Decrease T on each frame upgrde is active
updateTimer PROC
   cmp player[3], 0
   je ut_end
      cmp upgTimer, 1
      jg ut_decr
      je ut_rem
         ;Player just picked upgrade. Initiate timer
         mov upgTimer, DUR_UPG
         jmp ut_end
      ut_rem:
         ;T=1 means powerup ran out, remove status and tick to zero
         mov player[3], 00h
      ut_decr:
         ;Counter ticks down, nothing special
         dec upgTimer
   ut_end:
   ret
updateTimer ENDP   


;Handling of food items and score
;Algorithm - check each food against player's pos.
;If any gets eaten, relocate it and increase score by 1
;Only works with memory, no registers changed
checkPoints PROC
   pusha   
   
   lea bx, food1  ;load beginning of array
   mov cx, 03h    ;number of elements
   mov dh, player[0]  ;player's pos
   mov dl, player[1]
   ;check each food against player's position 
   cp_loop:
      mov ah, [bx]
      inc bx
      mov al, [bx]
      inc bx
      cmp ax, dx
      je cp_eatFood
   loop cp_loop
   popa
   ret
   cp_eatFood:
   ;player eats the food. Increase score and relocate it
   inc score
   sub bx, 2
   CALL moveUpgrade  ;pick a random free spot
   popa
   ret
checkPoints ENDP   

end start