; Copyright (c) 2025 Adrian Pilkington

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

;;; Classic shootem up game like asteroids, but more crazy
;;;
;;; https://youtube.com/@byteforever7829

;;; Known bug(s)
;;; 
;some #defines for compatibility with other assemblers
;pasmo only accepts DEFINE
CLS EQU $0A2A


;DEFINE DEBUG_PRINT_PIRATE_CYCLE
;DEFINE DEBUG_PIRATE_DIR
;DEFINE DEBUG_NO_MOVE_PIRATE  1
;DEFINE DEBUG_START_PIRATES_LOWER

KEYBOARD_READ_PORT_P_TO_Y	EQU $DF
; for start key
KEYBOARD_READ_PORT_A_TO_G	EQU $FD
; keyboard port for shift key to v
KEYBOARD_READ_PORT_SHIFT_TO_V EQU $FE
; keyboard space to b
KEYBOARD_READ_PORT_SPACE_TO_B EQU $7F
; keyboard q to t
KEYBOARD_READ_PORT_Q_TO_T EQU $FB

TOTAL_NUMBER_OF_ASTEROIDS  EQU 8

; starting port numbner for keyboard, is same as first port for shift to v
KEYBOARD_READ_PORT EQU $FE
SCREEN_WIDTH EQU 32
SCREEN_HEIGHT EQU 23   ; we can use the full screen becuase we're not using PRINT or PRINT AT ROM subroutines
MISSILE_COUNTDOWN_INIT EQU 18
PLAYER_START_POS EQU 637
PLAYER_LIVES EQU 3
ASTEROID_START_POS EQU 55
LEVEL_COUNT_DOWN_INIT EQU 4
LEV_COUNTDOWN_TO_INVOKE_BOSS EQU 2

VSYNCLOOP       EQU      2

; character set definition/helpers
__:				EQU	$00	;spacja
_QT:			EQU	$0B	;"
_PD:			EQU	$0C	;funt
_SD:			EQU	$0D	;$
_CL:			EQU	$0E	;:
_QM:			EQU	$0F	;?
_OP:			EQU	$10	;(
_CP:			EQU	$11	;)
_GT:			EQU	$12	;>
_LT:			EQU	$13	;<
_EQ:			EQU	$14	;=
_PL:			EQU	$15	;+
_MI:			EQU	$16	;-
_AS:			EQU	$17	;*
_SL:			EQU	$18	;/
_SC:			EQU	$19	;;
_CM:			EQU	$1A	;,
_DT:			EQU	$1B	;.
_NL:			EQU	$76	;NEWLINE

_BL             EQU $80; solid block

_0				EQU $1C
_1				EQU $1D
_2				EQU $1E
_3				EQU $1F
_4				EQU $20
_5				EQU $21
_6				EQU $22
_7				EQU $23
_8				EQU $24
_9				EQU $25
_A				EQU $26
_B				EQU $27
_C				EQU $28
_D				EQU $29
_E				EQU $2A
_F				EQU $2B
_G				EQU $2C
_H				EQU $2D
_I				EQU $2E
_J				EQU $2F
_K				EQU $30
_L				EQU $31
_M				EQU $32
_N				EQU $33
_O				EQU $34
_P				EQU $35
_Q				EQU $36
_R				EQU $37
_S				EQU $38
_T				EQU $39
_U				EQU $3A
_V				EQU $3B
_W				EQU $3C
_X				EQU $3D
_Y				EQU $3E
_Z				EQU $3F


;;;; this is the whole ZX81 runtime system and gets assembled and
;;;; loads as it would if we just powered/booted into basic

           ORG  $4009             ; assemble to this address

VERSN
    DB 0
E_PPC:
    DW 2
D_FILE:
    DW Display
DF_CC:
    DW Display+1                  ; First character of display
VARS:
    DW Variables
DEST:           DW 0
E_LINE:         DW BasicEnd
CH_ADD:         DW BasicEnd+4                 ; Simulate SAVE "X"
X_PTR:          DW 0
STKBOT:         DW BasicEnd+5
STKEND:         DW BasicEnd+5                 ; Empty stack
BREG:           DB 0
MEM:            DW MEMBOT
UNUSED1:        DB 0
DF_SZ:          DB 2
S_TOP:          DW $0002                      ; Top program line number
LAST_K:         DW $fdbf
DEBOUN:         DB 15
MARGIN:         DB 55
NXTLIN:         DW Line2                      ; Next line address
OLDPPC:         DW 0
FLAGX:          DB 0
STRLEN:         DW 0
T_ADDR:         DW $0c8d
SEED:           DW 0
FRAMES:         DW $f5a3
COORDS:         DW 0
PR_CC:          DB $bc
S_POSN:         DW $1821
CDFLAG:         DB $40
MEMBOT:         DB 0,0 ;  zeros
UNUNSED2:       DW 0

            ORG 16509       ;; we have to push the place in memory for this here becuase basic has
                    ;; to start at 16514 if memory was tight we could use the space between UNUSED2
                    ;; and Line1 for variables

Line1:          DB $00,$0a                    ; Line 10
                DW Line1End-Line1Text         ; Line 10 length
Line1Text:      DB $ea                        ; REM



;; in test mode comment out jp intro_title and comment in the test calls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	jp intro_title		; main entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;     call test_Missile
;    call test_randAsteroidLocation
     call test_checkCollisionMulti
;     call test_checkCollisionAtTopRow

;    call test_checkCollision_One
;    call test_initialiseAsteroids
;    call test_UpdateAsteroids
;    call test_drawAsteroids
endTest
    jr endTest
;;; end of test modes    


introWaitLoop
	ld b,64
introWaitLoop_1
    push bc
    pop bc

    ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
    inc hl
    ld a, $1f   ; we want a random seed index into the ROM which is 8Kbytes or zero to 8191 = 1f00 hex
    cp h
    jr z, resetRandSeed_1
    ld (randomSeed),hl
	djnz introWaitLoop_1
    jp read_start_key_1  
resetRandSeed_1
    ld hl, 0
    ld (randomSeed), hl

	djnz introWaitLoop_1
    jp read_start_key_1     ;; have to have 2 labels as not a call return

secondIntroWaitLoop

    ld b, 64
introWaitLoop_2
    push bc
    pop bc
    djnz introWaitLoop_2

	jp read_start_key_2

intro_title
	call CLS  ; clears screen and sets the boarder
;    ld a, (gameOverRestartFlag)
;    cp 1
;    call z, gameOverDeathScene


    xor a
    ld (goNextLevelFlag),a
    ld (restartLevelFlag),a
    ld (gameOverRestartFlag), a
    ld (last_score_mem_tens),a
    ld (last_score_mem_hund),a
    ld (sharkPosX), a
    ld (UFOValid), a
    ld (UFOBonusCountUp), a

    ;ld a, $00
    xor a
    ld (score_mem_tens),a
    ;ld a, $00
    ld (score_mem_hund),a


	ld bc,6
	ld de,title_screen_txt
	call printstring
	ld bc,6+33
	ld de,title_screen_txt
	call printstring
    ld bc,6+66
	ld de,title_screen_txt
	call printstring
	ld bc,202
	ld de,keys_screen_txt_1
	call printstring

    ld bc,235
	ld de,keys_screen_txt_2
	call printstring



	ld bc,299
	ld de,game_objective_boarder
	call printstring
	ld bc,332
	ld de,game_objective_txt
	call printstring
	ld bc,365
	ld de,game_objective_boarder
	call printstring

	ld bc,436
	ld de,high_Score_txt
	call printstring

    ld bc, 476
    ;ld de, last_score_mem_hund ; load address of hundreds
    ld de, high_score_hund
	call printNumber
	ld bc, 478			; bc is offset from start of display
	;ld de, last_score_mem_tens ; load address of  tens
	ld de, high_score_tens
	call printNumber
	ld bc,537
	ld de,credits_and_version_1
	call printstring
	ld bc,569
	ld de,credits_and_version_2
	call printstring
	ld bc,634
	ld de,credits_and_version_3
	call printstring
    ld de, 529
    ld hl, Display+1
    add hl, de
    ex de, hl
    ld hl, playerSpriteData
    ld c, 4
    ld b, 4
    call drawSprite



read_start_key_1
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, secondIntroWaitLoop
    ;; else drop into preinit then initVariables
    jr preinit

read_start_key_2
	ld a, KEYBOARD_READ_PORT_A_TO_G
	in a, (KEYBOARD_READ_PORT)					; read from io port
	bit 1, a									; check S key pressed
	jp nz, introWaitLoop
    jr preinit  ; not really necessary


preinit
;; initialise variables that are once per game load/start
    call CLS

initVariables
    ld a, 0
    ld (bossLevelFlag), a

    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring

    xor a
    ld a, (MissileInFlightFlag)
    ld (evenOddLoopFlag), a    ; used for multi rate enemies and other things
    ld (randNextAsteroidPosition), a
    ld (restartLevelFlag), a
    ld (sharkSpriteCycleCount), a
	
    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl
    call resetJollyRogerPos
	
	ld hl, UFOBonusSprite
	ld (UFOBonusSpritePointer), hl
	
	


    ld hl, 1
    ld (pirateDirUpdate),hl
    ld a, 5
    ld (pirateXPos),a

    ld a, PLAYER_LIVES
    ld (playerLives), a

    ld a, LEVEL_COUNT_DOWN_INIT
    ld (levelCountDown), a

    ld a, $01
    daa
    ld (gameLevel), a

    xor a
    ld a, (score_mem_tens)
    ld (last_score_mem_tens),a
    ld a, (score_mem_hund)
    ld (last_score_mem_hund),a

    ;call initialiseAsteroidValidAllOn
    ;call initialise_3_AsteroidValid
    call initialiseFirstAsteroidValid
    call initialiseAsteroids

    ;;ld hl, $00 
    ;;ld (randomSeed), hl     

    xor a
    ld (asteroidSpriteCycleCount), a
    ld hl, asteroidSpriteData4x4
    ld (asteroidSpritePointer), hl
    ld hl, 1
    ld (pirateDirUpdate), hl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
gameLoop    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ld b,VSYNCLOOP
waitForTVSync
	call vsync
	djnz waitForTVSync

    ld a, (levelCountDown)
    ld b, a
    ld a, (evenOddLoopCount)
    inc a
    cp b
    jr nc, resetEvenOddAndSetFlag  ; levelCountDown greater than evenOddLoopCount
    jr z, resetEvenOddAndSetFlag   ; levelCountDown equal to evenOddLoopCount
    ld (evenOddLoopCount), a
    xor a
    ld (evenOddLoopFlag), a    ; used for multi rate enemies

    ;;ld a,(evenOddLoopCount)
    ;;ld de, 760
    ;;call print_number8bits

    jr continueWithGameLoop

resetEvenOddAndSetFlag
    xor a
    ld (evenOddLoopCount), a
    ld a, 1
    ld (evenOddLoopFlag), a    ; used for multi rate enemies

    ;;ld a,(evenOddLoopFlag)
    ;;ld de, 764
    ;;call print_number8bits

continueWithGameLoop

    ld a, (restartLevelFlag)
    cp 1
    call z, executeRestartLevel
    xor a
    ld (restartLevelFlag), a

    ld a, (gameOverRestartFlag)
    cp 1
    jp z, intro_title


    ;ld a, (evenOddLoopFlag)
    ;cp 0
    ;jr z, skipUFOInGameLoop

    ;call asteroidUFOCountUp
    
    ;ld a, (evenOddLoopFlag)
    ;cp 1
    ;call z, updateAsteroidsPositions

    ;ld a, (UFOValid)
    ;cp 1
    ;call z, drawUFOBonus

skipUFOInGameLoop
    call drawAsteroids
    ld de, (currentPlayerLocation)
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite
    call printLivesAndScore

; keyboard layout for reading keys on ZX81
; BIT   left block      right block  BIT
; off                                off in <port>, when ld a, <port>
;       0  1 2 3 4     4 3 2 1 0                 <<< bit to check for each column after in a, $fe
; 3   ( 1  2 3 4 5 ) ( 6 7 8 9 0 )     4
; 2   ( Q  W E R T ) ( Y U I O P )     5
; 1   ( A  S D F G ) ( H I K L n/l)    6
; 0   (sft Z X C V ) ( B N M . spc)    7
;
; to read keys 1 2 3 4 5
; set all bits except bit 3 of register A = 1 1 1 1 0 1 1 1= f7, then execute in a, $fe  (fe is the "keyboard read port")
; now register a will contain a bit pattern to check for which key in that block was set, eg Key "1" = bit 0 of a
; ld a, $f7
; in a, $fe
; similarly for the rest, to read from block A S D F G, set a to 1 1 1 1 1 1 1 0 1 = $fd


    ;; read keys
    ld a, KEYBOARD_READ_PORT_P_TO_Y
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a                            ; O
    jp z, moveLeft


    ld a, KEYBOARD_READ_PORT_P_TO_Y
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a					        ; P
    jp z, moveRight


    ld a, KEYBOARD_READ_PORT_SPACE_TO_B
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; SPACE
    jp z, doFireMissile

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a						    ; Z
    jp z, doFireMissile
skipFireKeyDetect_1
    jp updateRestOfScreen                       ; if no key pressed continue

moveLeft
    ld a, (playerXPos)
    dec a
    cp 0      ;;; this prevents the player moving past edge, but if it's a door
              ;; trigger seperate code to move to new room
    jp z, updateRestOfScreen
    ld (playerXPos), a


    ld hl, (currentPlayerLocation)
    dec hl
    ld (currentPlayerLocation), hl


    ld a, KEYBOARD_READ_PORT_SPACE_TO_B
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; SPACE
    jp z, doFireMissile

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a						    ; Z
    jp z, doFireMissile
    jp updateRestOfScreen

moveRight
    ld a, (playerXPos)
    inc a
    cp 28          ;;; this prevents the player moving past edge, but if it's a door
                   ;; trigger seperate code to move to new room

    jp z, updateRestOfScreen
    ld (playerXPos), a



    ld hl, (currentPlayerLocation)
    inc hl
    ld (currentPlayerLocation), hl

    ld a, KEYBOARD_READ_PORT_SPACE_TO_B
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 0, a						    ; SPACE
    jp z, doFireMissile

    ld a, KEYBOARD_READ_PORT_SHIFT_TO_V
    in a, (KEYBOARD_READ_PORT)					; read from io port
    bit 1, a						    ; Z
    jp z, doFireMissile

    jr updateRestOfScreen

doFireMissile      ; triggered when jump key pressed just sets the
    call fireMissile

updateRestOfScreen

    ld hl, (playerSpritePointer)
    ld de, (currentPlayerLocation)
    ld c, 4
    ld b, 4
    call drawSprite

    ld a, (MissileInFlightFlag)
    cp 0
    jp z, skipMissileDraw
    
    call drawMissileAndBlank
    call checkIfMissileHit_FAST
    call updateMissilePosition
    call checkIfMissileHit_FAST      
skipMissileDraw
    call checkIfPlayerHit
    call updateAsteroidsPositions 

    ;call countNumberValidAsteroids
    ;ld de, 29
    ;call print_number8bits
    jp gameLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; END OF MAIN GAME LOOP ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; BELOW ARE SUBROUTINES CALLED FROM GAME LOOP  ;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

include initAsteroids.asm
include updateAsteroids.asm
include drawAsteroids.asm
include checkHitAndCollision.asm
include missile.asm
include player.asm

;updatePirateXPos
;    ld a, (pirateXPos)
;    cp 14
;    jr z, reversePirateDirToNeg
;    cp 3
;    jr z, reversePirateDirToPos

;    jr endOfUpdatePirateXPos

asteroidUFOCountUp
    ld a, (UFOBonusCountUp)
    inc a
    ld (UFOBonusCountUp), a
    cp 128
    jr z, triggerUFO
    jr notriggerUFO
triggerUFO
    xor a
    ld (UFOBonusCountUp), a
    ld a, 24
    ld (sharkPosX), a
    ld a, 1
    ld (UFOValid), a

notriggerUFO
    ret

updateJollyRoger
    ld a, (jollyRogerXPos)
    cp 23
    jr z, reverseDirToNeg
    cp 1
    jr z, reverseDirToPos

    jr endOfUpdateJollyRoger

reverseDirToNeg
    ld hl, -1
    ld (jollyRogerDirUpdate), hl
    ld de, 33
    ld hl, (bossSpriteLocation)
    add hl, de
    ld (bossSpriteLocation), hl
    ld a, (jollyRogerYPosCountDown)
    dec a
    cp 0
    jr z, setEndOfLevelFlagJR

    ld (jollyRogerYPosCountDown), a
    jr endOfUpdateJollyRoger
setEndOfLevelFlagJR
    ld a, 1
    ld (goNextLevelFlag),a
    ld hl, 1
    ld (pirateDirUpdate),hl
    xor a
    ld (bossLevelFlag),a

    ld a, 1
    ld (evenOddLoopFlag),a

    ld a, (playerLives)
    dec a
    cp 0
    jr z, setGameOverFlagJR
    ld (playerLives), a
    call resetJollyRogerPos
    ret
setGameOverFlagJR
    ld a, 1
    ld (gameOverRestartFlag), a
    call resetJollyRogerPos
    ret

reverseDirToPos
    ld hl, 1
    ld (jollyRogerDirUpdate), hl
    jr endOfUpdateJollyRoger

endOfUpdateJollyRoger

    ld hl, (bossSpriteLocation)
    ld (previousbossSpriteLocation), hl
    ld de, (jollyRogerDirUpdate)
    add hl, de
    ld (bossSpriteLocation), hl

    ld hl, (jollyRogerDirUpdate)
    ld a, (jollyRogerXPos)
    add a, l
    ld (jollyRogerXPos), a

    ret

executeRestartLevel
    xor a
    ld (restartLevelFlag), a
    ld (UFOValid), a
    ld (UFOBonusCountUp), a

    call CLS
    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring
    call printLivesAndScore

    ld a, 1
    ld (evenOddLoopFlag),a
    ;; drew player death animation
    ld hl, playerHitSprite
    ld (deadPlayerSpritePointer), hl
    ld b,6
playerDeathLoop
    push bc
        ld de, (currentPlayerLocation)
        ld hl, (deadPlayerSpritePointer)
        push hl
            ld c, 4
            ld b, 4
            call drawSprite
        pop hl
        ld de, 32
        add hl, de
        ld (deadPlayerSpritePointer), hl

        ld b, 64
playerDeathDelayLoop
        push bc
        ld b, 255
playerDeathDelayLoop2
            djnz playerDeathDelayLoop2
        pop bc
        djnz playerDeathDelayLoop
    pop bc
    djnz playerDeathLoop

        ld b, 255
playerDeathDelayLoop3
        push bc
        ld b, 255
playerDeathDelayLoop4
            djnz playerDeathDelayLoop4
        pop bc
        djnz playerDeathDelayLoop3


    call CLS
    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring
    call printLivesAndScore

    ld a, (playerLives)
    dec a
    cp 0
    jr z, setGameOverFlag
    ld (playerLives), a
    jr skipGameOverFlagSet
setGameOverFlag
    ld a, 1
    ld (gameOverRestartFlag), a

skipGameOverFlagSet
    xor a
    ld a, (MissileInFlightFlag)
    ld (evenOddLoopFlag), a    ; used for multi rate enemies and other things

    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    ld hl, 1
    ld (pirateDirUpdate),hl
    ld a, 5
    ld (pirateXPos),a


    ld hl, Display+1
    ld de, ASTEROID_START_POS
    add hl, de
    ld (asteroidTopLeftPositions), hl
    xor a
    ld (asteroidSpriteCycleCount), a
    ld hl, asteroidSpriteData4x4
    ld (asteroidSpritePointer), hl
    ld hl, 1
    ld (pirateDirUpdate), hl
    ;ld a, $ff   ; every pirate is alive
    ;ld a, $01   ; for test only bottom right pirate is alive
    ;ld a, $80   ; for test only top left pirate is alive
    ;ld a, $55   ; for test every other pirate is alive
    ;ld (asteroidValidBitMap), a
    ret


executeNextLevelStart
    call CLS
    ; draw top line where lives and score go
    ld de, TopLineText
    ld bc, 2
    call printstring

    ld a, 1
    ld (evenOddLoopFlag),a

    ld a, (gameLevel)
    inc a
    daa     ; convert to binary coded decimal to ease the display
    ld (gameLevel), a

    ld a, (levelCountDown)
    dec a
    cp LEV_COUNTDOWN_TO_INVOKE_BOSS
    jr z, setBossLevelFlag
    ;; could use this to start end end of level boss for now just hold at 1
    ld (levelCountDown), a
    jr continueGampLoop
setBossLevelFlag
    ld a, 1
    ld (bossLevelFlag), a
    ld a, LEVEL_COUNT_DOWN_INIT
    ld a, (levelCountDown)
continueGampLoop
    xor a
    ld a, (MissileInFlightFlag)
    ld (evenOddLoopFlag), a    ; used for multi rate enemies and other things

    ld a, (missileCountDown)
    ld a, 9
    ld (playerXPos), a
    ld hl, playerSpriteData
    ld (playerSpritePointer), hl
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    ld hl, 1
    ld (pirateDirUpdate),hl
    ld a, 5
    ld (pirateXPos),a


    ld hl, Display+1
    ld de, ASTEROID_START_POS
    add hl, de
    ld (asteroidTopLeftPositions), hl
    xor a
    ld (asteroidSpriteCycleCount), a
    ld hl, asteroidSpriteData4x4
    ld (asteroidSpritePointer), hl
    ld hl, 1
    ld (pirateDirUpdate), hl
    ;ld a, $ff   ; every pirate is alive
    ;ld a, $01   ; for test only bottom right pirate is alive
    ;ld a, $80   ; for test only top left pirate is alive
    ;ld a, $55   ; for test every other pirate is alive
    ;ld (asteroidValidBitMap), a

    xor a
    ld (goNextLevelFlag), a
    ld (UFOValid), a
    ld (UFOBonusCountUp), a
    ret

checkForSharkHit
    ret

drawUFOBonus

;debugBackOne
;   jp debugBackOne

   ld a, (sharkSpriteCycleCount)
   inc a
   cp 4
   
   jr z, resetSharkSpriteSprite
   ld (sharkSpriteCycleCount), a
   ld hl, (UFOBonusSpritePointer)
   ld de, 32
   add hl, de
   ld (UFOBonusSpritePointer), hl
   jr continueDrawShark
resetSharkSpriteSprite
   xor a
   ld (sharkSpriteCycleCount), a
   ld hl, UFOBonusSprite
   ld (UFOBonusSpritePointer), hl
continueDrawShark
    xor a
    ld d, a
    ld a, (sharkPosX)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ld (sharkAbsoluteScreenPos), hl
    ex de, hl
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite


    ld a, (sharkPosX)
    dec a
    cp 1
    jr z, noDrawSharkAndSetInvalid
    ld (sharkPosX), a
    xor a
    ld d, a
    ld a, (sharkPosX)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, (UFOBonusSpritePointer)
    ld c, 8
    ld b, 4
    call drawSprite
    jr endDrawUFOBonus
noDrawSharkAndSetInvalid
    xor a
    ld (UFOValid), a
    ld a, 1
    ld (sharkPosX), a
    xor a
    ld d, a
    ld a, (sharkPosX)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite
endDrawUFOBonus
    ret

resetJollyRogerPos
    ; reset the jolly roger for next boss level time
    ld hl, Display+1
    ld de, 39
    add hl, de
    ld (bossSpriteLocation), hl
    ld hl, 1
    ld (jollyRogerDirUpdate),hl
    ld a, 5
    ld (jollyRogerXPos),a
    ld a, 12
    ld (jollyRogerYPosCountDown),a
    xor a
    ld (bossLevelFlag), a
    ld a, LEVEL_COUNT_DOWN_INIT
    ld (levelCountDown), a
    ret

;;;; sprite code
;;;; our sprites are custom 8 by 8 charactor blocks - so will look fairly big (maybe too big)
;;;; the generic routines will look at an area of memory stored in hl before the call

;;;; on the zx81 each block is 2 "pixels" horizontally and 2 vertically pre encoded in the sprite memory
;;;; size of sprite in memory using bit pack is 16 * 16 = 256bits ==>>> 32bytes


;;; hl = start of sprite memory
;;; de = offset position in screen memory top left of sprite - no limit check done (yet)
;;; c  = width of sprite (normally 8 to keep things "simple")
;;; b  = rows in sprite (normally 8 to keep things "simple")
drawSprite
    push bc
    push de
    ld b, 0               ;; just doing columns in c so zero b
    ldir                  ;; ldir repeats ld (de), (hl) until bc = 0 and increments hl and de
    pop de
    ex de, hl
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite
    ret

;;; work in progrerss currently crashes -
;; if this could be made to work then the platforms would appear in blank bits of sprite
;; which would made game play better
drawSprite_OR_BACKGROUND
    push bc
    push de

    ld b, c    ; get column loop counter in b
drawSprite_OR_ColLoop
    ld a, (hl)
    inc hl
    or d
    or e
    ld (de), a
    inc de
    djnz drawSprite_OR_ColLoop

    pop de
    ex de, hl
    ld bc, 33             ;; move next write position to next row
    add hl, bc
    ex de, hl
    pop bc
    djnz drawSprite_OR_BACKGROUND
    ret

printLivesAndScore
    ld a, (playerLives)
    ld de, 29   
    call print_number8bits

    ld bc, 11
    ld de, score_mem_tens
    call printNumber

    ld bc, 9
    ld de, score_mem_hund
    call printNumber

    ld a, (gameLevel)
    ld de, 20
    call print_number8bits

    ret

increaseScore
    ld a,(score_mem_tens)				; add one to score, scoring is binary coded decimal (BCD)
	add a,1
	daa									; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_tens),a
	cp 153
	jr z, addOneToHund
	jr skipAddHund
addOneToHund
	ld a, 0
	ld (score_mem_tens), a
    ld a, (score_mem_hund)
	add a, 1
	daa                                   ; z80 daa instruction realigns for BCD after add or subtract
	ld (score_mem_hund), a
skipAddHund

; compare with high score and set that if higher

    ld a, (score_mem_hund)
    ld b,a     ; load the second 8-bit number into register b (via a)
    ld a, (high_score_hund)   ; load the first 8-bit number into register a
    cp b            ; compare a with the second 8-bit number (in register b)
    jr c, setHighScore ; jump if carry flag is set (high_score_hund < score_mem_hund)

    ; check if equal, and if so then check the tens, could be 00 50, or 01 50 in high score and current score
    jr z, highScoreHundEqualCheckTens
    ; high_score_hund > score_mem_hund so don't set
    jr skipCheckRestHighScore

highScoreHundEqualCheckTens
    ld a, (score_mem_tens)
    ld b, a
    ld a, (high_score_tens)
    cp b
    jp c, setHighScore ; jump if carry flag is set (a < b)

    jr skipCheckRestHighScore

setHighScore
    ld a, (score_mem_tens)
    ld (high_score_tens), a
    ld a, (score_mem_hund)
    ld (high_score_hund), a
    jr skipCheckRestHighScore

skipCheckRestHighScore

;; check if points reached 1000 if so then award new life
;; currently exactly 1000 so not too good:(
    ld a, (score_mem_hund)
    ld b,a     ; load the second 8-bit number into register b (via a)
    ld a, $10   ; load the first 8-bit number into register a
    cp b            ; compare a with the second 8-bit number (in register b)
    jr z, scoreNewLifeCheckTens
    ; high_score_hund > score_mem_hund so don't set
    jr endOfIncreaseScore

scoreNewLifeCheckTens
    ld a, (score_mem_tens)
    ld b, a
    ld a, $00
    cp b
    jp z, awardNewLife ; jump if carry flag is set (a < b)
    jr endOfIncreaseScore

awardNewLife
    ld a, (playerLives)
    inc a
    ld (playerLives),a
endOfIncreaseScore
    ret

; this prints at to any offset (stored in bc) from the top of the screen Display, using string in de
printstring
    push de ; preserve de
    ld hl,Display
    add hl,bc
printstring_loop
    ld a,(de)
    cp $ff
    jp z,printstring_end
    ld (hl),a
    inc hl
    inc de
    jr printstring_loop
printstring_end
    pop de  ; preserve de
    ret

print_number16bits    ; bc stores the 16bits, print b then c, de stores offset from Display
    ld a, b
    call print_number8bits
    ld a, c
    inc de  ; move de over by 2
    inc de
    call print_number8bits
    ret


print_number8bits       ; de stores the location to print relative to DF_CC
    ld hl, (DF_CC)
    add hl, de
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a

    ret

printNumber
    ld hl,Display
    add hl,bc
printNumber_loop
    ld a,(de)
    push af ;store the original value of a for later
    and $f0 ; isolate the first digit
    rra
    rra
    rra
    rra
    add a,$1c ; add 28 to the character code
    ld (hl), a
    inc hl
    pop af ; retrieve original value of a
    and $0f ; isolate the second digit
    add a,$1c ; add 28 to the character code
    ld (hl), a
    ret


;check if TV synchro (FRAMES) happend
vsync
	ld a,(FRAMES)
	ld c,a
sync
	ld a,(FRAMES)
	cp c
	jr z,sync
endOfVsync
	ret


                DB $76                        ; Newline
Line1End
Line2			DB $00,$14
                DW Line2End-Line2Text
Line2Text     	DB $F9,$D4                    ; RAND USR
				DB $1D,$22,$21,$1D,$20        ; 16514
                DB $7E                        ; Number
                DB $8F,$01,$04,$00,$00        ; Numeric encoding
                DB $76                        ; Newline
Line2End
endBasic

Display        	DB $76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
Variables
ExtraAllowAstOffBottom   ; bit wasteful of memory!
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
                DB  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,$76
missileData
    ;; small straight missile 
    DB $00, $00, $00, $00 
    DB $00, $85, $05, $00 
    DB $00, $85, $05, $00
	DB $00, $00, $00, $00

UFOBonusSprite    ; 96 bytes , 8x4 characters times 3 frames
   DB $00,$87,$83,$83,$83,$83,$04,$00
   DB $06,$03,$03,$03,$80,$07,$84,$82
   DB $86,$83,$83,$83,$80,$82,$81,$07
   DB $00,$02,$03,$03,$03,$03,$01,$00
   DB $00,$87,$83,$83,$83,$83,$04,$00
   DB $06,$03,$84,$80,$03,$84,$80,$86
   DB $86,$83,$81,$80,$83,$81,$80,$06
   DB $00,$02,$03,$03,$03,$03,$01,$00
   DB $00,$87,$83,$83,$83,$83,$04,$00
   DB $06,$84,$80,$03,$84,$80,$03,$86
   DB $86,$81,$80,$83,$81,$80,$83,$06
   DB $00,$02,$03,$03,$03,$03,$01,$00
   DB $00,$87,$83,$83,$83,$83,$04,$00
   DB $81,$80,$03,$84,$80,$03,$03,$86
   DB $84,$80,$83,$81,$80,$83,$83,$81
   DB $00,$02,$03,$03,$03,$03,$01,$00


explsion4x4     ;4x4 and 5 frames total of animation (80bytes)
   DB $00,$00,$00,$00
   DB $00,$87,$04,$00
   DB $00,$02,$01,$00
   DB $00,$00,$00,$00
   DB $00,$00,$00,$00
   DB $00,$00,$87,$00
   DB $00,$86,$00,$00
   DB $00,$01,$02,$00
   DB $87,$00,$00,$04
   DB $00,$05,$02,$04
   DB $01,$04,$87,$00
   DB $02,$00,$00,$01
   DB $87,$87,$02,$00
   DB $04,$01,$02,$04
   DB $02,$04,$01,$04
   DB $86,$00,$01,$06
   DB $01,$00,$00,$06
   DB $01,$00,$00,$87
   DB $02,$00,$04,$00

jollyRoger
     DB	$87, $03, $00, $00, $00, $00, $03, $04, $05, $86, $00, $83
     DB	$83, $00, $06, $85, $00, $00, $06, $04, $87, $86, $00, $00
     DB	$00, $00, $05, $87, $04, $85, $00, $00, $00, $00, $02, $83
     DB	$83, $01, $00, $00, $00, $00, $04, $07, $84, $87, $00, $00
     DB	$05, $06, $00, $01, $02, $00, $86, $85, $02, $83, $00, $00
     DB	$00, $00, $83, $01

playerHitSprite        ; 16x8 "pixels" 8x4 characters (bytes) times 4 frames animation
   DB $87,$02,$86,$00
   DB $87,$86,$06,$04
   DB $82,$86,$07,$80
   DB $00,$02,$01,$01
   DB $87,$02,$86,$00
   DB $87,$86,$06,$04
   DB $82,$86,$07,$80
   DB $00,$02,$01,$01
   DB $87,$00,$01,$04
   DB $87,$86,$00,$05
   DB $86,$01,$06,$81
   DB $04,$00,$01,$05
   DB $87,$00,$01,$04
   DB $87,$86,$00,$05
   DB $86,$01,$06,$81
   DB $04,$00,$01,$05
   DB $86,$00,$01,$87
   DB $87,$00,$00,$01
   DB $00,$04,$00,$02
   DB $04,$00,$06,$87
   DB $86,$00,$01,$87
   DB $87,$00,$00,$01
   DB $00,$04,$00,$02
   DB $04,$00,$06,$87


asteroidSpriteData4x4       ; these are 16 bytes each 4 by 4)
  DB $87,$86,$80,$04
   DB $82,$81,$07,$84
   DB $05,$84,$04,$07
   DB $02,$81,$07,$00
   DB $87,$84,$07,$04
   DB $82,$02,$07,$84
   DB $82,$85,$07,$07
   DB $02,$80,$07,$00
   DB $87,$80,$07,$04
   DB $80,$07,$06,$84
   DB $80,$85,$05,$07
   DB $02,$82,$07,$00


; the players space ship graphic, 4 by 4 blocks
playerSpriteData
   DB $00,$85,$05,$00
   DB $87,$82,$81,$04
   DB $80,$80,$80,$80
   DB $00,$02,$01,$00

; used to clear current location before move
blankSprite
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
    DB   0,  0,  0,  0,  0,  0,  0,  0
blockFilled    ;8*10
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8
    DB   8,  8,  8,  8,  8,  8,  8,  8

randNextAsteroidPosition
    DB 0
pirateFiringFlag
    DB 0
playerXPos
    DB 0
evenOddLoopFlag
    DB 0
evenOddLoopCount
    DB 0
enemySpriteZeroPos_ST
    DW 0
enemySpriteOnePos_ST
    DW 0
enemySpriteZeroPos_END
    DW 0
enemySpriteOnePos_END
    DW 0
enemySpriteZeroPos_DIR
    DW 0
enemySpriteOnePos_DIR
    DW 0
enemySpriteZeroPos_CUR
    DW 0
enemySpriteOnePos_CUR
    DW 0
enemySpriteZeroPos_RATE
    DB 0
enemySpriteOnePos_RATE
    DB 0
TEMP_enemySpritePointer
    DW 0
TEMP_enemySpritePos_CUR
    DW 0
enemySpriteZero_HorizVert
    DB 0
enemySpriteOne_HorizVert
    DB 0
TEMP_enemySpriteFrame
    DB 0
enemySpriteFrameZero
    DB 0
enemySpriteFrameOne
    DB 0
enemySprites   ;; keeping these to 4*4 for speed and size
enemySprite4by4BlankPointer
    DW 0
YSpeed
    DB 0
currentPlayerLocation
    DW 0
asteroidPosTemp
    DW 0
MissileInFlightFlag
    DB 0
missileCountDown
    DB 0
currentMissilePosition
    DW 0
levelCountDown
    DB 0
gameLevel
    DB 0
    DW 0
    DW 0
restartLevelFlag
    DB 1
enemySprite5by8Blank
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
    DB 0, 0, 0 ,0, 0
bossLevelFlag
    DB 0
sharkPosX
    DB 0
sharkAbsoluteScreenPos
    DW 0
UFOValid
    DB 0
UFOBonusCountUp
    DB 0
UFOBonusSpritePointer
    DW 0
sharkSpriteCycleCount
    DB 0
deadPlayerSpritePointer
    DW 0
playerSpritePointer
    DW 0
;asteroidTopLeftPositions   ; now moved to other file
;    DW 16384,16385,16386,16387,16388,16389,16390,16391

asteroidRowLeftPositionTemp
    DW 0
;asteroidValidBitMapMaskTemp
;    DB 0
bitsetMaskAsteroidTemp
    DB 0
asteroidSpriteCycleCount
    DB 0
asteroidSpritePointer
    DW 0
pirateDirUpdate
    DW 1
pirateXPos
    DB 0
playerLives
    DB 0
score_mem_tens
    DB 0
score_mem_hund
    DB 0
high_score_tens
    DB 0
high_score_hund
    DB 0
last_score_mem_tens
    DB 0
last_score_mem_hund
    DB 0
jollyRogerDirUpdate
    DW 1
jollyRogerXPos
    DB 0
bossSpriteLocation
    DW 0
jollyRogerYPosCountDown
    DB 0
previousbossSpriteLocation
    DW 0
gameOverRestartFlag
    DB 0
goNextLevelFlag
    DB 0
randomSeed
    DW 0
asteroidTopLeftPositions        ; these are the offsets from Display
    DW 0,0,0,0,0,0,0,0
;asteroidValidBitMapMask         ; valid asteroids "bitmap", we're having 8 asteroids so one bit per astreroid
 ;   DB 0



LivesText
    DB _L,_I,_V,_E,_S,_EQ,$ff
TopLineText
    DB _S,_C,_O,_R,_E,_CL,__,__,__,__,__,__,__,_L,_E,_V,_E,_L,_CL,__,__,__,_L,_I,_V,_E,_S,_CL,__,__,__,$ff

title_screen_txt
	DB	_Z,_X,_8,_1,__,_C,_R,_A,_Z,_Y,$16,_R,_O,_I,_D,_S,$ff
keys_screen_txt_1
	DB	_S,__,_T,_O,__,_S,_T,_A,_R,_T,26,__,_O,__,_L,_E,_F,_T,26,_P,__,_R,_I,_G,_H,_T,$ff
keys_screen_txt_2
	DB	__,__,__,__,__,__,__,_Z,__,_O,_R,__,_S,_P,_A,_C,_E,__,_EQ,__,_F,_I,_R,_E,$ff

game_objective_txt
	DB	_T,_O,__,_W,_I,_N,__,_S,_U,_R,_V,_I,_V,_E,__, _F,_A,_L,_L,_I,_N,_G,__,_S,_T,_U,_F,_F,$ff
game_objective_boarder   ; I know a bit wasteful but we have a whole 16K!
	DB	137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,137,$ff

last_Score_txt
	DB 21,21,21,21,_L,_A,_S,_T,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff
high_Score_txt
	DB 21,21,21,21,_H,_I,_G,_H,__,__,_S,_C,_O,_R,_E,21,21,21,21,$ff
credits_and_version_1
	DB __,_B,_Y,__,_A,__,_P,_I,_L,_K,_I,_N,_G,_T,_O,_N,__, _2,_0,_2,_5,$ff
credits_and_version_2
	DB __,__,_V,_E,_R,_S,_I,_O,_N,__,_V,_0,_DT,_0,_DT,_4,$ff
credits_and_version_3
	DB __,__,__,_Y,_O,_U,_T,_U,_B,_E,_CL, _B,_Y,_T,_E,_F,_O,_R,_E,_V,_E,_R,$ff


VariablesEnd:   DB $80
BasicEnd:


