; check if missile hit asteroids or hit UFO or other

tempFindAsteroidIndex
    db 0
testDEBUGText_HIT
    db _H,_I,_T,__,__,$ff
testDEBUGText_NOTHIT
    db _N,_O,_T,__,_H,_I,_T,$ff

checkIfMissileHit_FAST    ; prototype, instead of checking all the positions just 
                          ; check if ahead of missile is not blank
    ld hl, (currentMissilePosition)
    ld de, -33
    add hl, de
    ld a, (hl)
    cp 0
    jp nz, fastHit
    xor a
    jp fastHitDone
fastHit
    xor a
    ld (MissileInFlightFlag), a
    ld (tempFindAsteroidIndex), a

    ;; work out which asteroid was hit
    ld b, TOTAL_NUMBER_OF_ASTEROIDS
    ld hl, asteroidTopLeftPositions
    
fastHitCheckLoop
    push bc
        push hl
            ld de, (currentMissilePosition)
            sbc hl, de
            jr nz, fastHitFoundAsteroid
            jr fastHitNotFoundAsteroid
fastHitFoundAsteroid
        pop hl
        pop bc

        ;; we can use the loop count in bc to skip to the
        ;; place in asteroidValidBitMap

        ld hl, asteroidValidMap 
        ld a, (tempFindAsteroidIndex)
        ld b, a
        cp 0
        jr z, skipasteroidIndexLoop
asteroidIndexLoop
        inc hl
        djnz asteroidIndexLoop
skipasteroidIndexLoop
        xor a
        ld (hl), a
 ;       ld bc,620
 ;       ld de, testDEBUGText_HIT
 ;       call printstring  
 ;       call printAsteroidValidStatus       

 ;       ld a,(tempFindAsteroidIndex)
 ;       ld de, 760
 ;       call print_number8bits

;stopLoopHere
;        jr stopLoopHere

        jr drawExplosionPreLoop
fastHitNotFoundAsteroid
        inc hl     ; push hl onto the next asteroid position
        inc hl
        pop hl 
    pop bc
    ld a, (tempFindAsteroidIndex)
    inc a 
    ld (tempFindAsteroidIndex), a

    djnz fastHitCheckLoop

drawExplosionPreLoop
    ;; draw an explosion
    ld b, 5
    ld hl, explsion4x4
explosionDrawLoop_FAST
    push bc
        push hl
            push hl
                ld hl, (currentMissilePosition)
                ld de, -33
                add hl, de
                push hl 
                pop de
            pop hl     ; hl is should now be back to the sprite data    
            ;ld de, (currentMissilePosition)
            ld c, 4
            ld b, 4
            call drawSprite
            ld b, 64
explosionDelayLoop_FAST
            push bc
            ld b, 64
explosionDelayLoop2_FAST
                djnz explosionDelayLoop2_FAST
            pop bc
            djnz explosionDelayLoop_FAST
        pop hl
        ld de, 16
        add hl, de
    pop bc
    djnz explosionDrawLoop_FAST            

    ld de, (currentMissilePosition)
    ld hl, -33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite
    
    call increaseScore 

    ld a, 2
fastHitDone
    ret


checkIfMissileHit
    ld hl, asteroidValidMap
    ld (asteroidValidMapPtr), hl
    ld bc, TOTAL_NUMBER_OF_ASTEROIDS
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
            ld hl, (asteroidValidMapPtr)
            ld a, 0
            ld (hl), a   

            ;also if we have hit then disable the missile now!!
            xor a
            ld (MissileInFlightFlag), a
        
            ;; draw an explosion
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
                    ld b, 32
explosionDelayLoop2
                        djnz explosionDelayLoop2
                    pop bc
                    djnz explosionDelayLoop
                pop hl
                ld de, 16
                add hl, de
            pop bc
            djnz explosionDrawLoop            
            call increaseScore    
            ;ld bc,653
            ;ld de, testDEBUGText_HIT
            ;call printstring    
        pop hl ; we only have one missile at once, and so once one hits break loop early
        pop bc ; so restore stack to pre call state and jump end
        ld a, 2
        jr exitLoopEarlyCheckCollision
noHitMissile
            ; increment asteroid valid temp "pointer"
            ld hl, (asteroidValidMapPtr)
            ;inc hl
            ld (asteroidValidMapPtr), hl
        pop hl      ; restore hl now is next asteroid position
    pop bc 
    djnz checkHitLoop
exitLoopEarlyCheckCollision
    ;ld bc,620
    ;ld de, testDEBUGText_NOTHIT
    ;call printstring        
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
	        ld b,4
waitForTVSyncTestCheckCol1
	        call vsync
	        djnz waitForTVSyncTestCheckCol1
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
        call updateAsteroidsPositions
        call drawAsteroids
        call printAsteroidValidStatus
        call checkIfMissileHit_FAST
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
        call printAsteroidValidStatus
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
	        ld b,2
waitForTVSyncTestCheckCol2
	        call vsync
	        djnz waitForTVSyncTestCheckCol2
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
        call updateAsteroidsPositions
        call drawAsteroids
        call checkIfMissileHit_FAST
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