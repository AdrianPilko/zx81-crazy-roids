; check if missile hit asteroids or hit UFO or other


testDEBUGText
    db _H,_I,_T,__,__,$ff

checkIfMissileHit
    ld hl, asteroidValidMap
    ld (asteroidValidMapPtr), hl
    ld bc, 8
    ld hl, asteroidTopLeftPositions
checkHitLoop 
    push bc
        push hl
            ;check if asteroid is valid
            ld hl, (asteroidValidMapPtr)
            ld a, (hl)
        ;pop hl   ; fandangling with stack to maintain consistency
        cp 1
        jp nz, noHitMissile
        pop hl

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
            jr z, checkNextMissileHit
            jr noHitMissile
checkNextMissileHit
            ld a, l
            cp e
            jr z, MissileHitAsteroid
            jr noHitMissile

MissileHitAsteroid
            ;; missile HIT!!!
            xor a       
            ld (asteroidValidMap), a

            ;also if we have hit then disable the missile now!!
            xor a
            ld (MissileInFlightFlag), a
        
            ;; let's draw an explosion
            ld b, 5
            ld hl, explsion4x4
explosionDrawLoop
            push bc
                push hl
                    ld de, (currentMissilePosition)
                    ld c, 4
                    ld b, 4
                    call drawSprite
                    ld b, 32
explosionDelayLoop
                    push bc
                    ld b, 64
explosionDelayLoop2
                        djnz explosionDelayLoop2
                    pop bc
                    djnz explosionDelayLoop
                pop hl
                ld de, 16
                add hl, de
            pop bc
            djnz explosionDrawLoop

            ;; set this asteroid to not valid
            xor a
            ld hl, (asteroidValidMapPtr)
            ld (hl), a
            
            ld bc,695
            ld de, testDEBUGText
            call printstring
            call increaseScore    
        pop hl ; we only have 0one missile at once, and so once one hits break loop early
        pop bc ; so restore stack to pre call state and jump end
        ld a, b 
        ld de, 672
        call print_number8bits
        ld a, 2
        jr exitLoopEarlyCheckCollision
noHitMissile
            ; increment asteroid valid temp "pointer"
            ld hl, (asteroidValidMapPtr)
            inc hl
            ld (asteroidValidMapPtr), hl
        pop hl      ; restore hl now is next asteroid position
    pop bc 
    djnz checkHitLoop
exitLoopEarlyCheckCollision
    ret


testCollisionText_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,$ff

test_checkCollisionMulti
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
testCheckColMissileLoop1
    push bc
        push af
	        ld b,10
waitForTVSyncTestCheckCol1
	        call vsync
	        djnz waitForTVSyncTestCheckCol1
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
        call updateAsteroidsPositions
        call drawAsteroids
        call printAsteroidValidStatus
        call checkIfMissileHit
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
        call fireMissile                     
        ;push bc
skipfireMissileAgain
        ld de, 695
        ld bc, (asteroidTopLeftPositions)
	    call print_number16bits
    pop bc
    djnz testCheckColMissileLoop1
testCheckCollisionDone
    call printAsteroidValidStatus
    ld bc,728
    ld de,testCollisionText_done
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
	        ld b,10
waitForTVSyncTestCheckCol2
	        call vsync
	        djnz waitForTVSyncTestCheckCol2
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
        call updateAsteroidsPositions
        call drawAsteroids
        call checkIfMissileHit
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
    ld de,testCollisionText_done
    call printstring
    ret