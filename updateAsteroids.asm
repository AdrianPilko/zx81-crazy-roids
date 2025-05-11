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
;;    - Subroutines related to updating asteroid positions
;;
;; TODO LIST
;; =========
;;   - 



PRINT			EQU $10
PRINTAT			EQU $08F5

asteroidTempPositionStore                   ; used to store current hl in updateAsteroidPositions
    dw 0
resetAsteroidText
    db _R,_E,_S,_E,_T,$ff

updateAsteroidsPositions
    ld b, TOTAL_NUMBER_OF_ASTEROIDS                                 ; we have 8 asteroids on screen at any one time
    ld hl, asteroidTopLeftPositions         ; load hl with start of asteroid location memory

updateAsteroidLoop

    push bc                                     ; check asteroid is at bottom

        ld a, (hl)
        ld e, a
        inc hl
        ld a, (hl)
        ld d, a
        inc hl

        push af  
            push hl                                 ; save hl points to second memory location of locaitons
                push de 
                pop hl
                ld de, -33
                add hl, de
                ld hl, Display+1
                ld de, $0321   ;the offset to the lowest row the asteroid should be able to get
                add hl, de
                push hl
                pop de
            pop hl
        pop af
        cp d
        jr nc, resetUpdateAsteroid

continueUpdateAsteroid
        push hl                             ; save hl
            dec hl                              ; return hl to previous position
            dec hl

            ld a, (hl)
            ld e, a
            inc hl
            ld a, (hl)
            ld d, a
            push de
            pop hl
            ld de, 33
            add hl, de 
            push hl
            pop de                          ; de now contains asteroid position on next row  
        pop hl
        dec hl
        dec hl   
    
        ld a, e
        ld (hl), a
        inc hl
        ld a, d
        ld (hl), a
        inc hl
        jp endLoopUpdateAsteroids
resetUpdateAsteroid
    push hl
        ;; we need to make sure asteroids are at unique positions
        call randAsteroidLocation
        ; a now contains the random pos, need to get it in de
        ld a, (randNextAsteroidPosition)
        ld d, 0
        ld e, a    
        ld hl, Display+1
        add hl, de
        ld de, 66
        add hl, de
        push hl
        pop de
    pop hl

    dec hl
    dec hl   

    ld a, e
    ld (hl), a
    inc hl
    ld a, d
    ld (hl), a
    inc hl
    
endLoopUpdateAsteroids
    pop bc
    djnz updateAsteroidLoop
    ret

; in an attempt to improve ability to write relicable code I've added 
; some code in each file for testing individual subroutines,
; these currently have to be assembled in and called from the main asm file
; when in test mode, see main file and comment "n test mode comment in each thing below in turn"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TEST CODE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test code
test_UpdateAsteroids

    ld de, 166
    call printAsteroidPositionsTestUpdate

    ; call the function we're testing
    call initialiseAsteroids
    call initialiseAValidAlternate

    ld de, 265
    call printAsteroidPositionsTestUpdate

    call updateAsteroidsPositions
    ld de, 364
    call printAsteroidPositionsTestUpdate

    ; test full screen update hieght to check reset works
    ld b, 19
testFullScreenAsteroidUpdateLoop
    push bc
        call updateAsteroidsPositions
    pop bc
    djnz testFullScreenAsteroidUpdateLoop

    ld de, 463
    call printAsteroidPositionsTestUpdate


endTestUpAstHaltLoop
    jp   endTestUpAstHaltLoop 
    ret ; never gets here

printAsteroidPositionsTestUpdate
    ld b, 4
    ld hl, asteroidTopLeftPositions
    push de
printUpdateAsteroidPos1stRow
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
        djnz printUpdateAsteroidPos1stRow
   
    ld b, 4
    pop de
    push hl
        ld hl, 33
        add hl, de
        push hl
        pop de
    pop hl

printUpdateAsteroidPos2ndRow
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
    djnz printUpdateAsteroidPos2ndRow
    ret


debugPrintRegisters
    ; take copy of all the registers
    
    push hl
    push de
    push af    
    push bc
    
    ; position the cursor
    ;set b to row, c to first col, which is the last row    
    ld b, 0     ; have seen strange thing when debug comes out and bug happens it drops one line
    ld c, 1
    ld b, 21        
    call PRINTAT
    pop bc
    pop af
    pop de
    pop hl    

    push hl
    push de
    push af    
    push bc
    
    ld a, a
    call hprint    
    ld a, 14
    call PRINT  

    ld a, h
    call hprint    
    ld a, l    
    call hprint
    ld a, 14
    call PRINT
          
    ld a, d
    call hprint
    ld a, e
    call hprint
    ld a, 14
    call PRINT

    ld a, b
    call hprint
    ld a, c
    call hprint    
    ld a, 14
    call PRINT      

    ;restore registers (in correct reverse order!)        
    pop bc
    pop af
    pop de
    pop hl
    
    ret
    
hprint 		;;http://swensont.epizy.com/ZX81Assembly.pdf?i=1
	push af ;store the original value of a for later
	and $f0 ; isolate the first digit
	rra
	rra
	rra
	rra
	add a,$1c ; add 28 to the character code
	call PRINT ;
	pop af ; retrieve original value of a
	and $0f ; isolate the second digit
	add a,$1c ; add 28 to the character code
	call PRINT
	ret

