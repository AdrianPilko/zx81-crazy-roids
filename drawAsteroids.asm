asteroidValidMap
    DB 0,0,0,0,0,0,0,0
asteroidValidMapPtr
    DW 0


drawAsteroids

assumeAllAsteroidsValid
    ld hl, asteroidValidMap
    ld (asteroidValidMapPtr), hl

    ld b, 8                                 ; we have 8 asteroids on screen at any one time
    ld hl, asteroidTopLeftPositions         ; load hl with start of asteroid location memory

drawAsteroidLoop
    push bc
        push hl
            ;check if asteroid is valid
            ld hl, (asteroidValidMapPtr)
            ld a, (hl)
            inc hl
            ld (asteroidValidMapPtr), hl
        pop hl
        cp 1
        jp nz, skipDrawAsteroid
        
        
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
skipDrawAsteroid
    pop bc 
    djnz drawAsteroidLoop

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