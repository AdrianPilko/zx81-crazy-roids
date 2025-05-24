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
;;    - Subroutines related to the user player, for example collision detection with asteroid
;;
;; TODO LIST
;; =========
;;   - add checking of not just direct hit but each of the top 4 positions in the player sprite


currentPlayerXPos
    db 0

checkIfPlayerHit

    ld b, TOTAL_NUMBER_OF_ASTEROIDS       ; we have TOTAL_NUMBER_OF_ASTEROIDS asteroids on screen at any one time
    ld hl, asteroidTopLeftPositions         ; load hl with start of asteroid location memory
    ld a, 1
    ld (asteroid8BitIndex), a

checkIfPlayerHitLoop
    push bc
        ld a, (hl)
        ld e, a
        inc hl
        ld a, (hl)
        ld d, a
        inc hl
        ld (asteroidPosTemp), de

        push hl
            ld hl, asteroidValidMap
            ld a, (asteroid8BitIndex)
            ld b, a            
getPlayerHitHLIndexLoop
            inc hl
            djnz getPlayerHitHLIndexLoop
            dec hl
            ld a, (hl)    ; register a contains the valid 
        pop hl
        cp 0
        jp z, skipPlayerHit

        push hl
            ld hl, (currentPlayerLocation)
            ld de, 33
            sbc hl, de

            ld de, (asteroidPosTemp)
            sbc hl, de
            push af
                call z, executeRestartLevel                
            pop af

            ld hl, (currentPlayerLocation)
            ld de, 33
            sbc hl, de
            ; check either side - not just direct hit
            ld de, (asteroidPosTemp)
            inc de  
            sbc hl, de
            push af
                call z, executeRestartLevel                
            pop af

            ld hl, (currentPlayerLocation)
            ld de, 33
            sbc hl, de
            ; check either side - not just direct hit
            ld de, (asteroidPosTemp)
            dec de
            dec de  
            sbc hl, de
            push af
                call z, executeRestartLevel                
            pop af





        pop hl
        jr z, checkIfPlayerHitEndEarly
skipPlayerHit
    pop bc
    djnz checkIfPlayerHitLoop
    jr endOfcheckIfPlayerHit
checkIfPlayerHitEndEarly
    ld a, (asteroid8BitIndex)
    inc a
    ld (asteroid8BitIndex), a
    pop bc  ; pop here as we exited loop early
endOfcheckIfPlayerHit
    ret


drawPlayer  
    ld hl, (playerSpritePointer)
    ld de, (currentPlayerLocation)
    ld c, 4
    ld b, 4
    call drawSprite
    ret
