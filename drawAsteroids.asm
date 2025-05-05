;;; todo not yet ugraded to multiple asteroids (only does first oin

drawAsteroid

assumeAllAsteroidsValid
    ld b, 1

drawAsteroidLoop
    push bc
        ;check if asteroid is valid
        ld a, $fe
        ld b, a
        ld a, (asteroidValidBitMap)
        cp b
        jp z, skipDrawAsteroid

        ;ld de, (asteroidTopLeftPositions)  ;; should be doing this in the drawAsteroid subroutine
        ld hl, (asteroidTopLeftPositions)  ;; should be doing this in the drawAsteroid subroutine
        ld de, -33
        add hl, de
        push hl
        pop de 
        ld hl, blankSprite
        ld c, 4
        ld b, 1
        call drawSprite

        ld de, (asteroidTopLeftPositions)
        ld hl, (asteroidSpritePointer)
        ld b, 4
        ld c, 4 
        call drawSprite

        ld a, (asteroidSpriteCycleCount)
        inc a
        cp 2
        jr z, resetAsteroidSprite
        ld (asteroidSpriteCycleCount), a
        ld hl, (asteroidSpritePointer)
        ld de, 32
        add hl, de
        ld (asteroidSpritePointer), hl
        jr skipDrawAsteroid

resetAsteroidSprite
        xor a
        ld (asteroidSpriteCycleCount), a
        ld hl, asteroidSpriteData4x4
        ld (asteroidSpritePointer), hl    
    skipDrawAsteroid
    pop bc 
    djnz drawAsteroidLoop

    ret   

