
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


;; FILE PURPOSE
;; ============
;;    - Subroutines related to the UFO bonus - except collision detection with missile
;;
;; TODO LIST
;; =========
;;   - 




asteroidUFOCountUp
    ld a, (UFOBonusCountUp)
    inc a
    ld (UFOBonusCountUp), a
    cp 254
    jr z, triggerUFO
    jr notriggerUFO
triggerUFO
    xor a
    ld (UFOBonusCountUp), a
    ld a, 24
    ld (UFOXPosition), a
    ld a, 1
    ld (UFOValid), a

notriggerUFO
    ret



drawUFOBonus
   ld a, (sharkSpriteCycleCount)
   inc a
   cp 4
   
   jr z, resetSharkSpriteSprite
   ld (sharkSpriteCycleCount), a
   ld hl, (UFOBonusSpritePointer)
   ld de, 32
   add hl, de
   ld (UFOBonusSpritePointer), hl
   jr continueDrawShark
resetSharkSpriteSprite
   xor a
   ld (sharkSpriteCycleCount), a
   ld hl, UFOBonusSprite
   ld (UFOBonusSpritePointer), hl
continueDrawShark
    xor a
    ld d, a
    ld a, (UFOXPosition)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ld (sharkAbsoluteScreenPos), hl
    ex de, hl
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite


    ld a, (UFOXPosition)
    dec a
    cp 1
    jr z, noDrawSharkAndSetInvalid
    ld (UFOXPosition), a
    xor a
    ld d, a
    ld a, (UFOXPosition)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ex de, hl
    ld hl, (UFOBonusSpritePointer)
    ld c, 8
    ld b, 4
    call drawSprite
    jr endDrawUFOBonus
noDrawSharkAndSetInvalid
    xor a
    ld (UFOValid), a
    ld a, 1
    ld (UFOXPosition), a
    xor a
    ld d, a
    ld a, (UFOXPosition)
    ld e, a
    ld hl, Display+1
    add hl, de
    ld de, 33
    add hl, de
    ld hl, blankSprite
    ld c, 8
    ld b, 4
    call drawSprite
endDrawUFOBonus
    ret