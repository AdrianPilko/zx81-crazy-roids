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


;; FILE PURPOSE
;; ============
;;    - Subroutines related to firing and drawing the missile
;;
;; TODO LIST
;; =========
;;   - 


missileXPosition     ; set this when fired to the current X player location 
    db 0

drawMissileAndBlank
    ld a, (MissileInFlightFlag)
    cp 0
    jr z, noDrawMissile

    ld hl, (currentMissilePosition)
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite

    ld hl, missileData
    ld de, (currentMissilePosition)
    ld c, 4
    ld b, 4
    call drawSprite
noDrawMissile    
    ret

fireMissile
    ld a, (MissileInFlightFlag)
    cp 1
    jr z, skipLaunchMissile
    
    ; we first need to work out where the missiles should fire from based 
    ; on current player location unless we're in power up mode then just 
    ; fires form middle of nose of ship

    ; in power up mode we'll fire from nose and wing tips :) (note: not yet implemented)
    ld hl, (currentPlayerLocation)
    ld de, -33
    add hl, de
    ld (currentMissilePosition), hl
    
    ; setup the missile "Time To Live"  (like ethernet TTL right :)
    ld a, MISSILE_COUNTDOWN_INIT
    ld (missileCountDown), a
    ld a, 1
    ld (MissileInFlightFlag), a

skipLaunchMissile
    ret

updateMissilePosition
    ld a, (missileCountDown)
    dec a
    cp 0
    jp z, noMissileUpClearMissile

    ld (missileCountDown), a
    ld hl, (currentMissilePosition)
    ld de, -33
    add hl, de
    ld (currentMissilePosition), hl
    jr noMissileUpdate
noMissileUpClearMissile

    ; blank the missile at the top
    ld hl, (currentMissilePosition)
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite

    xor a
    ld (MissileInFlightFlag), a
noMissileUpdate
    ret

printMisilePosition   ; and missile in flight flag debug
    ld bc, (currentMissilePosition)    
    ld de, 727
    call print_number16bits
    ld a, (MissileInFlightFlag)
    ld de, 760
    call print_number8bits
    ret


testMissileText_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,$ff

test_Missile
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    call initialiseAsteroids
    call initialiseAsteroidValidAllOn
    call printAsteroidValidStatus

    call fireMissile

    ld b, 30                ; loop update missile for more than screen hieght
                            ; this tests that it stops at top  
testMoveMissileLoop
    push bc
        push af
	        ld b,10
waitForTVSyncTestMissile
	        call vsync
	        djnz waitForTVSyncTestMissile
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
    pop bc
    djnz testMoveMissileLoop

    ld bc,728
    ld de,testMissileText_done
    call printstring

    ret