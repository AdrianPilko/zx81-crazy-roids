drawMissileAndBlank
    ld hl, (currentMissilePosition)
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite

    ld hl, missileData
    ld de, (currentMissilePosition)
    ld c, 4
    ld b, 4
    call drawSprite
    ret

fireMissile
    ld a, (MissileInFlightFlag)
    cp 1
    jr z, skipLaunchMissile
    
    ; we first need to work out where the missiles should fire from based 
    ; on current player location unless we're in power up mode then just 
    ; fires form middle of nose of ship

    ; in power up mode we'll fire from nose and wing tips :) (note: not yet implemented)
    ld hl, (currentPlayerLocation)
    ld de, -33
    add hl, de
    ld (currentMissilePosition), hl
    
    ; setup the missile "Time To Live"  (like ethernet TTL right :)
    ld a, MISSILE_COUNTDOWN_INIT
    ld (missileCountDown), a
    ld a, 1
    ld (MissileInFlightFlag), a

skipLaunchMissile
    ret

updateMissilePosition
    ld a, (missileCountDown)
    dec a
    cp 0
    jp z, noMissileUpClearMissile

    ld (missileCountDown), a
    ld hl, (currentMissilePosition)
    ld de, -33
    add hl, de
    ld (currentMissilePosition), hl
    jr noMissileUpdate
noMissileUpClearMissile

    ; blank the missile at the top
    ld hl, (currentMissilePosition)
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, blankSprite
    ld c, 4
    ld b, 4
    call drawSprite

    xor a
    ld (MissileInFlightFlag), a
noMissileUpdate
    ret

testMissileText_done
    db _T,_E,_S,_T,__,_D,_O,_N,_E,$ff

test_Missile
    ld hl, Display+1
    ld de, PLAYER_START_POS
    add hl, de
    ld (currentPlayerLocation), hl

    call initialiseAsteroids
    call initialiseAsteroidValidAllOn
    call printAsteroidValidStatus

    call fireMissile

    ld b, 30                ; loop update missile for more than screen hieght
                            ; this tests that it stops at top  
testMoveMissileLoop
    push bc
        push af
	        ld b,10
waitForTVSyncTestMissile
	        call vsync
	        djnz waitForTVSyncTestMissile
        pop af

        call drawMissileAndBlank
        call updateMissilePosition
    pop bc
    djnz testMoveMissileLoop

    ld bc,728
    ld de,testMissileText_done
    call printstring

    ret