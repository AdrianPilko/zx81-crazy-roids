; check if missile hit asteroids or hit UFO or other


checkIfMissileHit
;;;; check if missile UFO, if the UFO is valid
    ld a, (UFOValid)
    cp 0
    jr z, skipCheckUFOHit
    ld de, (currentMissilePosition)
    ld hl, (sharkAbsoluteScreenPos)
    sbc hl, de
    ld a, h
    cp 0
    jp nz, skipCheckUFOHit
    ld a, l
    cp 0
    jp nz, skipCheckUFOHit

    ; UFO hit
    xor a
    ld (UFOValid), a
    ld b, 10
increaseScoreSharkHitLoop
    push bc
    call increaseScore
    pop bc
    djnz increaseScoreSharkHitLoop


skipCheckUFOHit
    ld a, (bossLevelFlag)
    cp 0
    jr z, skipCheckBossHit

    ; code for jolly roger hit detect

    ld de, (bossSpriteLocation)
    ld hl, 36
    add hl, de
    ex de, hl
    ld hl, (currentMissilePosition)

    ; now compare upper and lower bytes of hl and de
    ld a, h
    cp d
    jr z, checkNextRegMissileHit
    jr noHitMissileBoss
checkNextRegMissileHit
    ld a, l
    cp e
    jr z, MissileHitBoss
    inc hl
    ld a, h
    cp d
    jr z, checkNextRegMissileHit2
    jr noHitMissileBoss
checkNextRegMissileHit2
    ld a, l
    cp e
    jr z, MissileHitBoss

    jr noHitMissileBoss
MissileHitBoss
    xor a
    ld (bossLevelFlag), a
    ld b, 100
incScoreBossHitLoop
    push bc
       call increaseScore
    pop bc
    djnz incScoreBossHitLoop
    ld a, 1
    ld (goNextLevelFlag), a
    xor a
    ld (bossLevelFlag),a
    call resetJollyRogerPos

    ret
noHitMissileBoss
    ret

skipCheckBossHit
    ld hl, (asteroidTopLeftPositions)

    ld (asteroidRowLeftPositionTemp), hl
    ;becasue the whole loop is setup to count down, and because we want to check the
    ; lower row first we need to move the "Tope left position to be the bottom right
;;    ld de, 177
;;    add hl, de
    ;ld (asteroidRowLeftPositionTemp), hl  

    ld a, $fe
    ;ld (asteroidValidBitMapMaskTemp), a

    ; this is used to and with the current mask to check if missile collision check is needed
    ld a, $01
    ld (bitsetMaskAsteroidTemp), a
   ; ld b, 8
    ld b, 1
missileCheckHitLoop
    push bc
        ;; check if we even need to check this asteroid, if not valid then skip

;;;; TODO need to change this to multiple asteroid logic
;        ld a, (bitsetMaskAsteroidTemp)
;        ld b, a
;        ld a, (asteroidValidBitMap)
;        and b
;        jr z, noHitMissile

        ;; ok so we have checked everything ready for finally seeing if missile hit
        ld de, (asteroidRowLeftPositionTemp)
        ld hl, (currentMissilePosition)
        ; compare upper and lower bytes of hl and de
        ld a, h
        cp d
        jr z, checkNextPirateMissileHit
        jr noHitMissile
checkNextPirateMissileHit
        ld a, l
        cp e
        jr z, MissileHitAsteroid
        ; check next position along (makes game better to play)
        inc hl
        ; compare upper and lower bytes of hl and de
        ld a, h
        cp d
        jr z, checkNextPirateMissileHit2
        jr noHitMissile
checkNextPirateMissileHit2
        ld a, l
        cp e
        jr z, MissileHitAsteroid

        jr noHitMissile
MissileHitAsteroid
        ;; missile/cannon HIT!!!

;;; TODO switch off asteroid if hit       
        ;ld a, (asteroidValidBitMapMaskTemp)
        ;ld b, a
        ;ld a, (asteroidValidBitMap)
        ;and b
        ;ld (asteroidValidBitMap), a


        ;ld hl, asteroidValidMap
;        ld b, 8                                 
;checkHitMissileLoop1
;            ;check if asteroid is valid
;            ld a, (hl)
;            inc hl
;            ld (asteroidValidMapPtr), hl
;        pop hl
;        cp 1
;        jp z, noHitMissile



        ;also if we have hit then disable the missile now!!
        xor a
        ld (MissileInFlightFlag), a
        ld hl, 0
        ld (currentMissilePosition), hl
        call increaseScore
        pop bc   ; have to do this becasue we're exiting early out of loop

        ;; let's draw an explosion and tombstone breifly
        ld b, 3
        ld hl, explsion4x4
explosionDrawLoop
        push bc
            push hl
                ld de, (asteroidRowLeftPositionTemp)
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


        ret ;; exit early
noHitMissile
        ;; update mask which is the only bit not set we check next
        ;; e.g second pirate is 0x10111111
        ;ld a, (asteroidValidBitMapMaskTemp)
        ;rra
        rlc a
        ;ld (asteroidValidBitMapMaskTemp),a

        ;; update the mask which is the bit we're setting set all others z80
        ;; e.g second pirate is 0x01000000
        ld a, (bitsetMaskAsteroidTemp)
        ;rra
        rlc a
        ld (bitsetMaskAsteroidTemp), a

        ; now move the position to compare (ie a pirate)
        ld de, -4
        ld hl, (asteroidRowLeftPositionTemp)
        add hl, de
        ld (asteroidRowLeftPositionTemp), hl
    pop bc
        ld a, b  ; check the loop counter, if it's 3 then move the whole lot down by +33-16
        cp 5
        jr nz, endLoopLabelPriateCheck
        ld de, -149

        add hl, de
        ld (asteroidRowLeftPositionTemp), hl

endLoopLabelPriateCheck

    ;djnz missileCheckHitLoop
    ld a, b
    dec a
    ld b, a
    cp 0
    jp nz, missileCheckHitLoop
    ret


testCollisionText_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,$ff


test_checkCollision   
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
testCheckColMissileLoop
    push bc
        push af
	        ld b,10
waitForTVSyncTestCheckCol
	        call vsync
	        djnz waitForTVSyncTestCheckCol
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
        call updateAsteroidsPositions
        call drawAsteroids
        call checkIfMissileHit

        ld de, 695
        ld bc, (asteroidTopLeftPositions)
	    call print_number16bits
    pop bc
    djnz testCheckColMissileLoop

    ld bc,728
    ld de,testCollisionText_done
    call printstring
    ret