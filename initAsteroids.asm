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
;;    - Subroutines related to the initialising the asteroids positions and validity
;;
;; TODO LIST
;; =========
;;   - 




initialise_3_AsteroidValid
      
    ld b, 3
    ld hl, asteroidValidMap
    ld a, 1
initValid3AsteroidLoop
    ld (hl), a
    inc hl   
    djnz initValid3AsteroidLoop
    ret

initialiseAsteroidValidAllOn       
    ld b, TOTAL_NUMBER_OF_ASTEROIDS
    ld hl, asteroidValidMap
    ld a, 1
initValidAsteroidLoop
    ld (hl), a
    inc hl   
    djnz initValidAsteroidLoop
    ret

initialiseAValidAlternate
    ld b, 4                     ; set half of the asteroids to on
    ld hl, asteroidValidMap
initValidAsteroidLoop2
    xor a
    ld (hl), a
    inc hl
    ld a, 1
    ld (hl), a
    inc hl    
    djnz initValidAsteroidLoop2
    ret

initialiseFirstAsteroidValid       
    ld b, 7
    ld hl, asteroidValidMap
    ld a, 1
    ld (hl), a
    inc hl
    xor a
initValidAsteroidLoop3
    ld (hl), a
    inc hl   
    djnz initValidAsteroidLoop3
    ret

setFirstPositionForTest
    ld hl, Display+1
    ld de, 76              ; this is the third row down same position as missile test
    add hl, de
    ld (asteroidTopLeftPositions), hl
    ret


initialiseAsteroids    
    ld b, TOTAL_NUMBER_OF_ASTEROIDS 
    ld hl, asteroidTopLeftPositions
initAsteroidsLoop
    push bc
        push hl
            call randAsteroidLocation   
        pop hl
        ld d, 0
        ld e, a
        push hl
            ld hl, Display+1
            add hl, de
            ld de,66       ; add an extra 33 to keep it 2 off the top - so blank works
            add hl, de
            push hl
            pop de
        pop hl

        ld a, e         ; store the asteroid location into the hl offsets from asteroidTopLeftPositions
        ld (hl), a
        ld a, d
        inc hl
        ld (hl), a
        inc hl          ; move to next asteroid location from asteroidTopLeftPositions
    pop bc
    djnz initAsteroidsLoop

    xor a
    ld (asteroidSpriteCycleCount), a
    ld hl, asteroidSpriteData4x4
    ld (asteroidSpritePointer), hl
    
    ret

randAsteroidLocation
    call setRandomNumber16
    cp 28  ; cannot be more than 28 due to 4 byte width of the sprite
    jr c, noAdjustRand
    ld b, 4
    sub b
noAdjustRand
    ld (randNextAsteroidPosition), a   
    ;; we ideally don't want any overlapping asteroids so check the X positions
    ld hl, asteroidXPositions    
    ld b, TOTAL_NUMBER_OF_ASTEROIDS
checkAsteroidXPosLoop                    
        ld e, a
        xor a
        ld a, (hl)                
        inc hl
       ; call printDebugRandAsteroid 
        cp e        
        jr z, randAsteroidLocation         ; we've had a match in terms of an extsting asteroid so try again               
        jr nz, doneRandAsteroid
    djnz checkAsteroidXPosLoop
doneRandAsteroid    
    ld a, (randNextAsteroidPosition)   ; store the position
    ret


setRandomNumber16
    ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
    inc hl
    ld (randomSeed),hl
    
    ld a, (hl)
;; limit to 2 to 28
    cp 2           ; Compare A with 2
    jr c, limitTo2 ; If A < 2, jump to clamp to 2
    cp 29       ; Compare A with 28
    jr nc, limitTo28 ; If A >= 29, jump to clamp to 28
    ; A is already between 2 and 28 inclusive
    jr randLimitComplete
limitTo2:
    ld a, 2
    jr randLimitComplete
limitTo28:
    ld a, 28
randLimitComplete
    ; A is now guaranteed to be between 2 and 28

    ; a now contains random number 0,1,2,3,..,31
    ret

; in an attempt to improve ability to write relicable code I've added 
; some code in each file for testing individual subroutines,
; these currently have to be assembled in and called from the main asm file
; when in test mode, see main file and comment "n test mode comment in each thing below in turn"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TEST CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test code
test_initialiseAsteroids

    ;; call the function we're testing
    call initialiseAsteroidValidAllOn
    ;call initialiseAValidAlternate
    call initialiseAsteroids
    call printAsteroidPoistions
    call printAsteroidValidStatus
    call printAsteroidXPositions

endTestHaltLoop
    jp   endTestHaltLoop 
    ret ; never gets here

printAsteroidValidStatus
    ;; print valid status of each asteroid
    ld b, 8                                 ; we have 8 asteroids on screen at any one time
    ld hl, asteroidValidMap
    ld de, 0
testValidPrintAsteroidLoop
    push bc
    ld a, (hl)
    push hl        
        call print_number8bits
    pop hl    
    inc hl
    inc de
    inc de
    inc de
    pop bc
    djnz testValidPrintAsteroidLoop
    ret


test_randAsteroidLocation

    ld b, TOTAL_NUMBER_OF_ASTEROIDS
    ld de, 1
    ld hl, asteroidXPositions    
test_randAsteroidLoop
        push bc 
            push hl
                push de
                    call randAsteroidLocation                
                pop de
                push de
                    call print_number8bits
                pop de
                inc de
                inc de
                inc de
            pop hl
        pop bc
        ld (hl), a
        inc hl        
    djnz test_randAsteroidLoop

    call printAsteroidXPositions
    ret


printAsteroidPoistions
    ld b, 4
    ld hl, asteroidTopLeftPositions
    ld de, 727  ; position of print initially then inc'd below
debugPrintAsteroidPos1stRow
    push bc
        ld a, (hl)
        ld c, a 
        inc hl
        ld a, (hl)
        ld b, a
        inc hl
        push de
            push hl
                call print_number16bits
            pop hl
        pop de  
        inc de 
        inc de
        inc de
        inc de
        inc de
    pop bc
    djnz debugPrintAsteroidPos1stRow
    
    ld b, 4
    ld de, 760  ; position of print initially then inc'd below
debugPrintAsteroidPos2ndRow
    push bc
        ld a, (hl)
        ld c, a 
        inc hl
        ld a, (hl)
        ld b, a
        inc hl
        push de
            push hl
                call print_number16bits
            pop hl
        pop de  
        inc de 
        inc de
        inc de
        inc de
        inc de
    pop bc
    djnz debugPrintAsteroidPos2ndRow

    ret

printAsteroidXPositions
    ld hl, asteroidXPositions
    ld b, TOTAL_NUMBER_OF_ASTEROIDS
    ld de, 66
printAsteroidXPosLoop    
    push hl
        push bc
            push de        
                ld a, (hl)
                call print_number8bits
            pop de
            inc de
            inc de
            inc de    
        pop bc
    pop hl
    inc hl
    djnz printAsteroidXPosLoop
    ret


printDebugRandAsteroid
    push hl
    push de
    push bc
    push af
        ld de, 99
        ld a, (hl)
        call print_number8bits
    pop af
    pop bc
    pop de
    pop hl

    push hl
    push de
    push bc
    push af
        ld de, 132
        ld a, (randNextAsteroidPosition)
        call print_number8bits
    pop af
    pop bc
    pop de
    pop hl


    push hl
    push de
    push bc
    push af
        ld de, 165
        push hl
        pop bc
        call print_number16bits
    pop af
    pop bc
    pop de
    pop hl
    ret