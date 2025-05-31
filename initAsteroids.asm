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

tempAsteroidXPosition
    db 0
randomXPosTableIndex
    dw 0
randomPrecalculatedXPos
    db  16,	24,	0,	15,	7,	18,	8,	2,	12,	24,	25,	11  
    db  17,	22,	0,	23,	26,	13,	2,	6,	6,	1,	11,	15
    db  18,	3,	16,	20,	26,	14,	24,	1,	19,	26,	2,	17
    db  5,	24,	9,	26,	17,	15,	25,	5,	16,	25,	6,	14
    db  9,	28,	18,	7,	3,	7,	9,	5,	25,	2,	14,	16
    db  0,	18,	18,	25,	9,  27,	20,	9,	22,	8,	5,	12
    db  24,	0,	27,	9,	10,	20,	11,	26,	12,	1,	13,	3
    db  5,	12,	22,	27,	14,	1,	3,	1,	3,	13,	14,	25
    db  12,	14,	3,	6,	18,	19,	4,	21,	28,	3,	9,	23
    db  3,	14,	17,	12,	13,	27,	16,	14,	0,	12,	19,	24
    db  7,	22,	23,	6,	16,	10,	0,	16,	26,	7,	17,	7
    db  11,	8,	5,	21,	18,	22,	12,	2,	28,	14,	21,	3

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


initialiseSingleAsteroid
;PURPOSE
;========
; initialise one asteroid with index stored in asteroid8BitIndex
; after this the asteroid will have
;     X position set - effectively asteroidXPositions[asteroid8BitIndex]
;     Valid status set - effectively asteroidValidMap[asteroid8BitIndex]


    ld a, (asteroid8BitIndex)
    ld b, a
    push bc
        call randAsteroidLocation         
        ld hl, asteroidTopLeftPositions
    pop bc

initSingleIncLoop_1
    inc hl    ; inc twice as asteroid screen location is two bytes
    inc hl
    djnz initSingleIncLoop_1
    dec hl    ; convention is that asteroid8BitIndex has to be one more than required else djnz wraps 255
    dec hl    ; so dec hl twice at end - I know a bit janky
    ;; hl now contains the memory location of the asteroid we want to update

   
    ld d, 0
    ld e, a
    push hl
        ld hl, Display+1
        add hl, de
        ld de,66       ; add an extra 33 to keep it 2 off the top - not 1 so blanking works
        add hl, de
        push hl
        pop de
    pop hl

    ld a, e            ; store the asteroid location into the hl offsets from asteroidTopLeftPositions
    ld (hl), a
    ld a, d
    inc hl
    ld (hl), a    


    ;;;; Update valid map
    ld a, (asteroid8BitIndex)
    ld b, a  
    ld hl, asteroidValidMap
initSingleIncLoop_2
    inc hl    ; inc twice as asteroid screen location is two bytes
    djnz initSingleIncLoop_2
    dec hl    ; convention is that asteroid8BitIndex has to be one more than required else djnz wraps 255
    ;; hl now contains the memory location of the asteroidValidMap we want to update
    ld a, 1
    ld (hl), a

;; update astgeroid x position
    ld a, (asteroid8BitIndex)
    ld b, a  
    ld hl, asteroidXPositions
initSingleIncLoop_3
    inc hl    ; inc twice as asteroid screen location is two bytes
    djnz initSingleIncLoop_3
    dec hl    ; convention is that asteroid8BitIndex has to be one more than required else djnz wraps 255
    ;; hl now contains the memory location of the asteroidValidMap we want to update
    ld a, (tempAsteroidXPosition)
    ld (hl), a

    ; may as well reset the sprite pointer as well (but affects all asteroids
    xor a
    ld (asteroidSpriteCycleCount), a
    ld hl, asteroidSpriteData4x4
    ld (asteroidSpritePointer), hl

    ret


initialiseAsteroids    
    call initialiseAsteroidValidAllOn
    ld b, TOTAL_NUMBER_OF_ASTEROIDS 
    ld hl, asteroidTopLeftPositions
    ld a, 1
    ld (asteroid8BitIndex), a
initAsteroidsLoop
    push bc
        push hl
            ld a, (asteroid8BitIndex)
            call randAsteroidLocation
            ld hl, asteroidXPositions
            push af 
                ld a, (asteroid8BitIndex)
                ld b, a
getCurrentXHLLocationLoop
                inc hl
                djnz getCurrentXHLLocationLoop
                dec hl
            pop af
            ld (hl), a
        pop hl
        ld d, 0
        ld e, a
        push hl
            ld hl, Display+1
            add hl, de
            ld de,66       ; add an extra 33 to keep it 2 off the top - not 1 so blanking works
            add hl, de
            push hl
            pop de
        pop hl

        ld a, e            ; store the asteroid location into the hl offsets from asteroidTopLeftPositions
        ld (hl), a
        ld a, d
        inc hl
        ld (hl), a
        inc hl             ; move to next asteroid location from asteroidTopLeftPositions
    pop bc
    ld a, (asteroid8BitIndex)
    inc a
    ld (asteroid8BitIndex), a
    djnz initAsteroidsLoop

    xor a
    ld (asteroidSpriteCycleCount), a
    ld hl, asteroidSpriteData4x4
    ld (asteroidSpritePointer), hl
    
    ret


randAsteroidLocation 

;PURPOSE
;========
; set "random" position of asteroid numbered asteroid8BitIndex, where asteroid8BitIndex = 0 to TOTAL_NUMBER_OF_ASTEROIDS-1
; and sets a to that x position as well as randomPrecalculatedXPos
;
; clobbers registers hl, a, b
; 
; reads:    asteroid8BitIndex,randomPrecalculatedXPos
; test by:  test_randAsteroidLocation

    call setRandomNumber6
    ;a now has number 0 to 6 which we'll use to index randomPrecalculatedXPos

    ;; we ideally don't want any overlapping asteroids so check the X positions
    ld hl, randomPrecalculatedXPos

    ld a, (asteroid8BitIndex)
    ld b, a
    inc b
incIndexOfRandomLoop_outer
    push bc
        ld a, (asteroid8BitIndex)
        ld b, a
        inc b    ; prevent b being zero
incIndexOfRandomLoop_inner
            inc hl
        djnz incIndexOfRandomLoop_inner
        inc hl
    pop bc
    djnz incIndexOfRandomLoop_outer
    dec hl   ; we added one to b before to stop zero negative so dec on last hl
    ld a, (hl)
    ld (tempAsteroidXPosition), a
    ret


setRandomNumber6
    ld hl, (randomSeed)  ; attempt to set random seed based on time user takes to press start
    inc hl
    ld (randomSeed),hl
    
    ld a, (hl)
;; limit to 0 to 10 as this is the number of pre calculated random rows in randomPrecalculatedXPos
    cp 1           ; Compare A with 1
    jr c, limitTo0 ; If A < 1, jump to clamp to 0
    cp 10       ; Compare A with 10
    jr nc, limitTo6 ; If A >= 10, jump to clamp to 9
    ; A is already between 0 and 9 inclusive
    jr randLimitComplete
limitTo0:
    ld a, 0
    jr randLimitComplete
limitTo6:
    ld a, 9
randLimitComplete
    ; A is now guaranteed to be between 0 and 9
    ret

; in an attempt to improve ability to write relicable code I've added 
; some code in each file for testing individual subroutines,
; these currently have to be assembled in and called from the main asm file
; when in test mode, see main file and comment "n test mode comment in each thing below in turn"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TEST CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

testInitAstDoneText
    db _T,_E,_S,_T,__,_I,_N,_I,_T,__, _A,_S,_T,_E,_R,_O,_I,_D,__,_D,_O,_N,_E,$ff

test_initialiseAsteroids

    ld b, $ff
testIniAsteroidLoop
    push bc
        ;; call the function we're testing
        call initialiseAsteroidValidAllOn
        ;call initialiseAValidAlternate
        call initialiseAsteroids
        
        call printAsteroidPoistions
        call printAsteroidValidStatus
        call printAsteroidXPositions
    pop bc
    djnz testIniAsteroidLoop
 

    ld de, testInitAstDoneText
    ld bc, 664
    call printstring

    ret 

printAsteroidValidStatus
    ;; print valid status of each asteroid
    ld b, TOTAL_NUMBER_OF_ASTEROIDS    ; we have this many asteroids on screen at any one time
    ld hl, asteroidValidMap
    ld de, 33
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


testSetOneDoneText
    db _T,_E,_S,_T,__,_I,_N,_I,_T,__, _O,_N,_E,__,_D,_O,_N,_E,$ff
    

test_initialiseSingleAsteroid
    ;; test initialising just 3rd asteroid, remeber not zero indexed, 1 to TOTAL_NUMBER_OF_ASTEROIDS inclusive
    ld a, 3
    ld (asteroid8BitIndex),a

    call initialiseSingleAsteroid

    call printAsteroidPoistions
    call printAsteroidValidStatus
    call printAsteroidXPositions

    ld de, testSetOneDoneText
    ld bc, 664
    call printstring

    ret

testRandDoneText
    db _T,_E,_S,_T,__,_R,_A,_N,_D,__, _A,_S,_T,_E,_R,_O,_I,_D,__,_D,_O,_N,_E,$ff

test_randAsteroidLocation

    ld b, $f
testRandLoop
    push bc
    ld b, TOTAL_NUMBER_OF_ASTEROIDS
    ld de, 0
    ld hl, asteroidXPositions 
    ld a, 1
    ld (asteroid8BitIndex), a
test_randAsteroidLoop
        push bc 
            push hl
                push de
                    ld a, (asteroid8BitIndex)
                    call randAsteroidLocation                    
                    push af
                        ; here checking that a contains the random x position
                        call print_number8bits
                    pop af
                pop de
                inc de
                inc de
                inc de
            pop hl
        pop bc
        ld (hl), a
        inc hl        
        ld a, (asteroid8BitIndex)
        inc a
        ld (asteroid8BitIndex), a
    djnz test_randAsteroidLoop

    ;check here that the x positions were stored by randAsteroidLocation
    call printAsteroidXPositions
    pop bc
    djnz testRandLoop
 
    ld de, testRandDoneText
    ld bc, 169
    call printstring
    ret


printAsteroidPoistions
    push bc
    push hl
    push af
    push de

    ld b, TOTAL_NUMBER_OF_ASTEROIDS
    ld hl, asteroidTopLeftPositions
    ld de, 165  ; position of print initially then inc'd below

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

        push hl
            ld hl, 33
            add hl, de
            ex de, hl
        pop hl
    pop bc
    djnz debugPrintAsteroidPos1stRow
    
    pop de
    pop af
    pop hl
    pop bc
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