
asteroidTopLeftPositions        ; these are the offsets from Display
    DW 0,0,0,0,0,0,0,0
asteroidValidBitMapMask         ; valid asteroids "bitmap", we're having 8 asteroids so one bit per astreroid
    DB 0

initialiseAsteroidValidAllOn
    ld a, $ff
    ld (asteroidValidBitMapMask), a
    ret

initialiseAValidAlternate
    ld a, $55
    ld (asteroidValidBitMapMask), a
    ret

initialiseAsteroids    
    ld b, 8
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
    ret

randAsteroidLocation
    call setRandomNumber16
    cp 28  ; cannot be more than 28 due to 4 byte width of the sprite
    jr noAdjustRand
    ld b, 4
    sub b
noAdjustRand
    ld (randNextAsteroidPosition), a
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
    call initialiseAsteroids
    call initialiseAValidAlternate

    ld b, 4
    ld hl, asteroidTopLeftPositions
    ld de, 265  ; position of print initially then inc'd below
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
    ld de, 298  ; position of print initially then inc'd below
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

    ld b, 8    
    ld de, 364
    ld a, (asteroidValidBitMapMask)
    ld c, a

debugAsteroidValidLoop
    rl c           ; Rotate left through carry
    push bc
        jr c, debugAsteroidValidLoopSET  ; If carry is set, bit was 1
        jr debugAsteroidValidLoopSKIP
debugAsteroidValidLoopSET:
        push de
            push hl
                ld a, 1
                call print_number8bits
            pop hl
        pop de  
        jr checkdebugAsteroidValid
debugAsteroidValidLoopSKIP:
        push de
            push hl
                ld a, 0
                call print_number8bits
            pop hl
        pop de       
checkdebugAsteroidValid:        
        inc de  ; move screen write pos on by 3 form start (so there's a space between)
        inc de
        inc de
    pop bc
    djnz debugAsteroidValidLoop


endTestHaltLoop
    jp   endTestHaltLoop 
    ret ; never gets here


