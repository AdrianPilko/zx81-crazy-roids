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
;;    - Subroutines related to collisions with missile and asteroids
;;
;; TODO LIST
;; =========
;;   - in the fast hit detection need to make sure it doesn't check the top row of screen
;;     as that's where the game info's displayed
;;
;;  BUGS
;;  ====
;;   - when firing a missile if the player x position (and you would think) the mnissile x position is greater than 
;;      two thirds the screen wideth, then the missile stops and won't go all way up

tempFindAsteroidIndex
    db 0
testDEBUGText_HIT
    db _H,_I,_T,__,__,$ff
testDEBUGText_NOTHIT
    db _N,_O,_T,__,_H,_I,_T,$ff

checkIfMissileHit_FAST    ; prototype, instead of checking all the positions just 
                          ; check if ahead of missile is not blank

    ;; check if missile in MissileInFlightFlag set

    ld a, (MissileInFlightFlag)
    cp 0    
    jr nz, contCheckIfMissileHit_FAST
    ret

contCheckIfMissileHit_FAST

    ;; check if missile is near top of screen, we don't want to get score if
    ;; it hits the top!    
    ld hl, Display+99
    ld de, (currentMissilePosition)
    sbc hl, de
    jp nc, missileNearTopSkipAll

    ;; we're checking for anything centred in next row up
    ld hl, (currentMissilePosition)
    ld de, -33                 
    add hl, de
    ld a, (hl)
    cp 0
    jp nz, fastHit

    inc hl
    ld a, (hl)  
    cp 0
    jp nz, fastHit

    inc hl
    ld a, (hl)  
    cp 0
    jp nz, fastHit

    inc hl
    ld a, (hl)  
    cp 0
    jp nz, fastHit

    xor a
    jp fastHitDone
fastHit
;;; find out which asteroid it was to set valid map
    ld a, 1
    ld (tempFindAsteroidIndex), a
    ld hl, asteroidXPositions        
    ld b, TOTAL_NUMBER_OF_ASTEROIDS
findAsteroidLoop1    
    push bc
        ld a, (missileXPosition)
        dec a
        push af
            ld a, (hl)    ; x position                
            ld b, a 
        pop af         
        inc hl
        cp b 
        jp z, foundIndexAsteroid
        inc a
        cp b          
        jp z, foundIndexAsteroid
        inc a        
        cp b         
        jp z, foundIndexAsteroid        
        inc a
        cp b         
        jp z, foundIndexAsteroid

        ld a, (tempFindAsteroidIndex)
        inc a
        ld (tempFindAsteroidIndex), a
    pop bc
    djnz findAsteroidLoop1

;;; also need to check if it hit UFO!!
    ld a, (missileXPosition)
    push af
        ld a, (UFOXPosition)
        ld b, a 
    pop af         
    cp b     
    jp z, drawExplosionPreLoop
    inc a
    cp b     
    jp z, drawExplosionPreLoop
    inc a
    cp b     
    jp z, drawExplosionPreLoop
           
    ;;; not matched anything
    jp fastHitDone

foundIndexAsteroid
    xor a
    ld (MissileInFlightFlag), a

    pop bc ; because we exited loop abouve early
    ld hl, asteroidValidMap    
    ld a, (tempFindAsteroidIndex)

    ld b, a
    xor a
findAsteroidLoop2
    inc hl
    inc a
    djnz findAsteroidLoop2
    dec hl

    xor a           ;;zero a
    ld (hl), a      ;; now zero asteroidValidMap[tempFindAsteroidIndex]

drawExplosionPreLoop
    ;; draw an explosion
    ld b, 4
    ld hl, explsion4x4
explosionDrawLoop_FAST
    push bc
        push hl
            push hl
                ld hl, (currentMissilePosition)
                ld de, -132
                add hl, de
                push hl 
                pop de
            pop hl     ; hl is should now be back to the sprite data    
            ;ld de, (currentMissilePosition)
            ld c, 4
            ld b, 4
            call drawSprite
            ld b, 255
explosionDelayLoop2_FAST
            rld  ; waste some cycles  18 T-states * 5 * 255 =  -> 22950 / 3250000 ~ 0.007seconds (if z80 was doing nothing else!)
            rld
            rld
            rld
            rld
            djnz explosionDelayLoop2_FAST
        pop hl
        ld de, 16
        add hl, de
    pop bc
    djnz explosionDrawLoop_FAST            

    ld de, (currentMissilePosition)
    ld hl, -132
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 8
    call drawSprite
    
    call increaseScore 

    ld a, 2
fastHitDone
missileNearTopSkipAll
    ret



;;; slow version 

checkIfMissileHit
    ld bc, TOTAL_NUMBER_OF_ASTEROIDS
    ld hl, asteroidTopLeftPositions
    ld a, 1
    ld (asteroid8BitIndex), a    
checkHitLoop 
    push bc
        push hl
            ld hl, asteroidValidMap
            ld a, (asteroid8BitIndex)
            ld b, a
getCurrentValidHLIndexHITLoop
            inc hl
            djnz getCurrentValidHLIndexHITLoop
            dec hl
            ld a, (hl)    ; register a contains the valid 
        pop hl
        cp 1
        jp nz, noHitMissile

        ld a, (hl)
        ld e, a
        inc hl
        ld a, (hl)
        ld d, a
        inc hl
        push hl
            push de
            pop hl 
            ld hl, (currentMissilePosition)
            ; compare upper and lower bytes of hl and de
            ld a, h
            cp d
        pop hl
        jr z, checkNextMissileHit            
        jr noHitMissile
checkNextMissileHit
        ld a, l
        cp e
        jr z, MissileHitAsteroid
        jr noHitMissile

MissileHitAsteroid
        ;; missile HIT!!!
        push hl
            ld hl, asteroidValidMap
            ld a, (asteroid8BitIndex)
            ld b, a
getCurrentHLIndexHITLoop_2
            inc hl
            djnz getCurrentHLIndexHITLoop_2
            dec hl
            xor a
            ld (hl), a    ; set valid asteroidValidMap[asteroid8BitIndex] to zero
        pop hl

        ;also if we have hit then disable the missile now!!
        xor a
        ld (MissileInFlightFlag), a
        
            ;; draw an explosion
            ld b, 3
            ld hl, explsion4x4
explosionDrawLoop
            push bc
                push hl
                    ld de, (currentMissilePosition)
                    ld c, 4
                    ld b, 4
                    call drawSprite
                    ld b, 8
explosionDelayLoop
                    push bc
                    ld b, 8
explosionDelayLoop2
                        djnz explosionDelayLoop2
                    pop bc
                    djnz explosionDelayLoop
                pop hl
                ld de, 16
                add hl, de
            pop bc
            djnz explosionDrawLoop            
            call increaseScore    
            ;ld bc,653
            ;ld de, testDEBUGText_HIT
            ;call printstring    
        pop hl ; we only have one missile at once, and so once one hits break loop early
        pop bc ; so restore stack to pre call state and jump end
        ld a, 2
        jr exitLoopEarlyCheckCollision
noHitMissile


    ld a, (asteroid8BitIndex)
    inc a
    ld  (asteroid8BitIndex),a

    pop bc 
    djnz checkHitLoop
exitLoopEarlyCheckCollision
    ld bc,620
    ld de, testDEBUGText_NOTHIT
    call printstring        
    ret


testCollisionTextMulti_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,__,_M,_U,_L,_T,_I,$ff
testCollisionTextOne_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,__,_O,_N,_E,$ff
testCollisionTextTop_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,__,_T,_O,_P,$ff

test_checkCollisionMulti
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    ld a, $09
    ld (missileXPosition), a

    ld de, 25
    push af
        call print_number8bits
    pop af 
    call initialiseAsteroids
    call initialiseAsteroidValidAllOn 
    call printAsteroidValidStatus
    call setFirstPositionForTest      ; set to same as the misile "X" position
    call fireMissile

    ld b, $ff                ; loop update missile for more than screen hieght
                            ; this tests that it stops at top  
testCheckColMissileLoop1
    push bc
        push af
	        ld b,40
waitForTVSyncTestCheckCol1
	        call vsync
	        djnz waitForTVSyncTestCheckCol1
        pop af

        ;; the code here should be used as basis for normal game loop
        call updateAsteroidsPositions     
        call drawAsteroids
        call updateMissilePosition
        call checkIfMissileHit_FAST     
        call checkIfPlayerHit
        call drawMissileAndBlank


        call printAsteroidValidStatus
        call printAsteroidPoistions
        call printAsteroidXPositions
        ; if a == 2
        cp 2
        ;pop bc   ; pop bc so it doesn't cause crash when breaking out early
        ;jr z, testCheckCollisionDone
        jr z, fireMissileAgain
        ld a, (MissileInFlightFlag)
        cp 1
        jr nz, fireMissileAgain
        jr skipfireMissileAgain
fireMissileAgain
        call printAsteroidValidStatus
        call fireMissile                     
        ;push bc
skipfireMissileAgain
        ld de, 695
        ld bc, (asteroidTopLeftPositions)
	    call print_number16bits
        call countNumberValidAsteroids
        ld de, 29
        call print_number8bits
    pop bc
    djnz testCheckColMissileLoop1
testCheckCollisionDone
    call printAsteroidValidStatus
    ld bc,728
    ld de,testCollisionTextMulti_done
    call printstring
    ret


test_checkCollisionAtTopRow
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    call initialiseAsteroids
    call initialiseAsteroidValidAllOn 
    call printAsteroidValidStatus
    call setFirstPositionForTest      ; set to same as the misile "X" position

    call drawAsteroids
    call fireMissile

    ld b, $ff                ; loop update missile for more than screen hieght
                            ; this tests that it stops at top  
testCheckColMissileLoopTop1
    push bc
        push af
	        ld b,4
waitForTVSyncTestCheckColTop1
	        call vsync
	        djnz waitForTVSyncTestCheckColTop1
        pop af
        call drawAsteroids
        call drawMissileAndBlank
        call checkIfMissileHit_FAST
        call updateMissilePosition
        call checkIfMissileHit_FAST
        ;call updateAsteroidsPositions        ;;; in this mode keep asteroid on top row
        call printAsteroidValidStatus        
        ; if a == 2
        cp 2
        ;pop bc   ; pop bc so it doesn't cause crash when breaking out early
        ;jr z, testCheckCollisionDone
        jr z, fireMissileAgainTop
        ld a, (MissileInFlightFlag)
        cp 1
        jr nz, fireMissileAgain
        jr skipfireMissileAgainTop
fireMissileAgainTop
        call printAsteroidValidStatus
        call fireMissile                     
        ;push bc
skipfireMissileAgainTop
        ld de, 695
        ld bc, (asteroidTopLeftPositions)
	    call print_number16bits
    pop bc
    djnz testCheckColMissileLoopTop1
testCheckCollisionDoneTop
    call printAsteroidValidStatus
    ld bc,728
    ld de,testCollisionTextTop_done
    call printstring
    ret







test_checkCollision_One   
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    call initialiseAsteroids
    call initialiseFirstAsteroidValid  ; we only need the first asteroid set for this test
    call printAsteroidValidStatus
    call setFirstPositionForTest      ; set to same as the misile "X" position

    call drawAsteroids
    call fireMissile

    ld b, 30                ; loop update missile for more than screen hieght
                            ; this tests that it stops at top  
testCheckColMissileLoop2
    push bc
        push af
	        ld b,2
waitForTVSyncTestCheckCol2
	        call vsync
	        djnz waitForTVSyncTestCheckCol2
        pop af

        call drawAsteroids
        call drawMissileAndBlank
        call checkIfMissileHit_FAST
        call updateMissilePosition
        call checkIfMissileHit_FAST
        call updateAsteroidsPositions        
        call printAsteroidValidStatus
        ; if a == 2
        cp 2
        pop bc   ; pop bc so it doesn't cause crash when breaking out early
        jr z, testCheckCollisionDone2             
        push bc

        ld de, 695
        ld bc, (asteroidTopLeftPositions)
	    call print_number16bits
    pop bc
    djnz testCheckColMissileLoop2
testCheckCollisionDone2
    ld bc,728
    ld de,testCollisionTextOne_done
    call printstring
    ret