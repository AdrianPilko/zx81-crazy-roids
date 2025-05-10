asteroidValidMap
    DB 0,0,0,0,0,0,0,0
asteroidValidMapPtr
    DW 0


drawAsteroids

assumeAllAsteroidsValid
    ld hl, asteroidValidMap
    ld (asteroidValidMapPtr), hl

    ld b, TOTAL_NUMBER_OF_ASTEROIDS       ; we have TOTAL_NUMBER_OF_ASTEROIDS asteroids on screen at any one time
    ld hl, asteroidTopLeftPositions         ; load hl with start of asteroid location memory

drawAsteroidLoop
    push bc
        push hl
            ;check if asteroid is valid
            ld hl, (asteroidValidMapPtr)
            ld a, (hl)
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
        call resetAsteroid_HL
    pop hl
endDrawAstLoop
    push hl
        ld hl, (asteroidValidMapPtr)
        inc hl
        ld (asteroidValidMapPtr), hl
    pop hl 
    pop bc 
    djnz drawAsteroidLoop

    ret   



resetAsteroid_HL

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
       ; pop de

        pop bc
        push de
          ld de, 44
          call print_number16bits
        pop de
;stopHere
 ;       jr stopHere
    
    pop hl

    ld a, e         ; store the asteroid location into the hl offsets from asteroidTopLeftPositions
    ld (hl), a
    ld a, d
    inc hl
    ld (hl), a
    inc hl          ; move to next asteroid location from asteroidTopLeftPositions
    
    ;set this asteroid is valid
    ld hl, (asteroidValidMapPtr)
    ld a, 1
    ld (hl), a

    ret


testAsteroidText_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,$ff

test_drawAsteroids
    
    call initialiseAsteroids
    call initialiseAsteroidValidAllOn
    ;call initialiseAValidAlternate
    call printAsteroidValidStatus

    ld b, 128
test_DrawAsteroidLoop1
    push bc
        push af
	        ld b,2
waitForTVSyncTestDraw1
	        call vsync
	        djnz waitForTVSyncTestDraw1
        pop af
        call updateAsteroidsPositions
        call drawAsteroids
    pop bc
    djnz test_DrawAsteroidLoop1
    
    call initialiseFirstAsteroidValid
    call printAsteroidValidStatus

    ld b, 128
test_DrawAsteroidLoop2
    push bc
        push af
	        ld b,2
waitForTVSyncTestDraw2
	        call vsync
	        djnz waitForTVSyncTestDraw2
        pop af
        call updateAsteroidsPositions
        call drawAsteroids
    pop bc
    djnz test_DrawAsteroidLoop2
    


    ld bc,728
    ld de,testAsteroidText_done
    call printstring

endTestDrawAsteroid
    jr endTestDrawAsteroid
    ret