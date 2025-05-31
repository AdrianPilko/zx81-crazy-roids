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
;;    - Subroutines related to drawing the asteroids
;;
;; TODO LIST
;; =========
;;   - 

asteroidValidMap
    DB 0,0,0,0,0,0,0,0
asteroidXPositions
    DB 0,0,0,0,0,0,0,0

drawAsteroids
    ld b, TOTAL_NUMBER_OF_ASTEROIDS       ; we have TOTAL_NUMBER_OF_ASTEROIDS asteroids on screen at any one time
    ld hl, asteroidTopLeftPositions         ; load hl with start of asteroid location memory
    ld a, 1
    ld (asteroid8BitIndex), a
drawAsteroidLoop
    push bc
        push hl
            ld a, (asteroid8BitIndex)
            ld b, a
            ld hl, asteroidValidMap
getCurrentValidHLIndexLoop
            inc hl
            djnz getCurrentValidHLIndexLoop
            dec hl
            ld a, (hl)    ; register a contains the valid 
        pop hl
        cp 0
        jp z, skipDrawAsteroid
        
        
        ;; get the next asteroid position from asteroidTopLeftPositions via hl
        push hl
            ld a, (hl)
            ld e, a
            inc hl
            ld a, (hl)
            ld d, a      ; de now contains the contents of the location offset from asteroidTopLeftPositions
            push de
            pop hl
                ld de, -33
                add hl, de
            push hl
            pop de 
            ld hl, blankSprite
            ld c, 4
            ld b, 1
            call drawSprite
        pop hl

        push hl
            ld a, (hl)
            ld e, a
            inc hl
            ld a, (hl)
            ld d, a                         ; de now contains the contents of the location offset from asteroidTopLeftPositions
            ld hl, (asteroidSpritePointer)
            ld b, 4
            ld c, 4 
            call drawSprite
        pop hl
        inc hl                              ; move hl onto the next asteroid position
        inc hl
        push hl
            ld a, (asteroidSpriteCycleCount)
            inc a
            cp 2
            jr z, resetAsteroidSprite
            ld (asteroidSpriteCycleCount), a
            ld hl, (asteroidSpritePointer)
            ld de, 32
            add hl, de
                ld (asteroidSpritePointer), hl
            jr skipUpdateAsteroidSpritePtr
resetAsteroidSprite
            xor a
            ld (asteroidSpriteCycleCount), a
            ld hl, asteroidSpriteData4x4
            ld (asteroidSpritePointer), hl    
skipUpdateAsteroidSpritePtr
        pop hl
        jp endDrawAstLoop
skipDrawAsteroid
    push hl
        ;; call subroutine to reset this asteroid, uses dec hl, dec hl to skip back
        push af 
            ld a, (asteroid8BitIndex)
            call resetAsteroid_HL
        pop af
    pop hl
endDrawAstLoop

    ld a, (asteroid8BitIndex)
    inc a
    ld  (asteroid8BitIndex),a
    pop bc 
    djnz drawAsteroidLoop

    ret   

;asteroidResetDebugText
;    db _A,_S,_T,__,_R,_S,_E,_T,$ff

resetAsteroid_HL   ; this needs asteroid8BitIndex set to the index (indexed from 1 to the asteroid to update)
    push hl        
        call initialiseSingleAsteroid    
    pop hl
    ret


testAsteroidText_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,$ff

test_drawAsteroids
    
    call initialiseAsteroids

    ld b, 128
test_DrawAsteroidLoop1
    push bc
        push af
	        ld b,3
waitForTVSyncTestDraw1
	        call vsync
	        djnz waitForTVSyncTestDraw1
        pop af
        call updateAsteroidsPositions
        call drawAsteroids
        call printAsteroidValidStatus
        call printAsteroidXPositions 
        call printAsteroidPoistions  
        push af
	        ld b,3
waitForTVSyncTestDraw2
	        call vsync
	        djnz waitForTVSyncTestDraw2
        pop af

        call printAsteroidValidStatus
        call printAsteroidXPositions 
        call printAsteroidPoistions                
    pop bc
    djnz test_DrawAsteroidLoop1
  

    ld bc,728
    ld de,testAsteroidText_done
    call printstring
    ret