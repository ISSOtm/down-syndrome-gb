
SECTION "Overworld", ROM0

Overworld::
    ld a, $E4
    ldh [hBGP], a
    ldh [hOBP1], a
    ld a, %11100000
    ldh [hOBP0], a
    xor a
    ldh [hLCDC], a
    ld [wGameState], a
    ld [wPlayerDirection], a
    ld [wDigDelay], a
    ld [wAnimationFrameCounter], a
    ld [wHavensEncountered], a
    rst wait_vblank


    ld a, HIGH(_SCRN1)
    ld [wWhichTilemap], a

    ld hl, $8800
    ld bc, $800
    xor a
    rst memset

    ld de, OverworldGfx
    ; ld hl, $9000
    ld b, 9 * 4
    call pb16_unpack_block

    ld hl, $8000
    ld b, 6
.initScoreTiles
    ld de, DigitTiles
    ld c, 16
    rst memcpy_small
    dec b
    jr nz, .initScoreTiles

    ld de, OverworldSpriteGfx
    ; hl = ...
    ld b, 18 * 4
    call pb16_unpack_block

    ld hl, wShadowOAM
    xor a
    ld c, $A0 - (6 / 2) * 2 * 4
    rst memset_small
    lb de, 16, SCRN_X
    lb bc, $46, 6 / 2
.writeEnergySprites
    ld a, d
    ld [hli], a ; Y pos
    add a, 16
    ld d, a
    ld a, e
    ld [hli], a ; X pos
    ld a, b
    ld [hli], a ; Tile
    inc b
    inc b
    xor a
    ld [hli], a ; Attr
    dec c
    jr nz, .writeEnergySprites
    lb de, 16, 8
    lb bc, 0, 6 / 2
.writeScoreSprites
    ld a, d
    ld [hli], a ; Y pos
    add a, 16
    ld d, a
    ld a, e
    ld [hli], a ; X pos
    ld a, b
    ld [hli], a ; Tile
    inc b
    inc b
    xor a
    ld [hli], a ; Attr
    dec c
    jr nz, .writeScoreSprites

    xor a
    ld hl, wGeneratedScreens
    ld c, NB_SCREENS_PER_ROW
    rst memset_small

    ; xor a
    ld hl, wScore
    ld c, 6 / 2
    rst memset_small

    ; xor a
    ld [wPlayerDepth], a
    inc a ; ld a, 1
    ld [wPlayerXScreen], a
    ld hl, wPlayer
    ld de, PlayerObject
    ld c, sizeof_Object
    rst memcpy_small

    call DrawScreen

    ld de, wShadowOAM
    ld hl, wPlayer
    call ProcessAndDrawObject
    ld a, d ; ld a, HIGH(wShadowOAM)
    ldh [hOAMBufferHigh], a

    ld a, SCRN_Y
    ldh [hWY], a

    ld a, LCDCF_ON | LCDCF_WINON | LCDCF_BG8800 | LCDCF_BG9800 | LCDCF_OBJ16 | LCDCF_OBJON | LCDCF_BGON
    ldh [rLCDC], a
    ldh [hLCDC], a

OverworldLoop:
    rst wait_vblank

    ld a, [wGameState]
    add a, a
    add a, LOW(GameStateProcessorTable)
    ld l, a
    adc a, HIGH(GameStateProcessorTable)
    sub l
    ld h, a
    ld a, [hli]
    ld h, [hl]
    ld l, a
    rst call_hl

    ld a, HIGH(wShadowOAM)
    ldh [hOAMBufferHigh], a
    jr OverworldLoop


GameStateProcessorTable:
    dw .normalState
    dw .downTransition
    dw .leftTransition
    dw .rightTransition

.normalState
    ld a, [wDigDelay]
    and a
    jr z, .dontDecrementDigDelay
    dec a
    ld [wDigDelay], a
.dontDecrementDigDelay

    ld hl, wAnimationFrameCounter
    inc [hl]

    ; Draw player
    ld hl, wPlayer
    ld de, wShadowOAM
    call ProcessAndDrawObject

    xor a
    ld [wTransitionCounter], a

    ; Draw screen objects
    ld l, a ; ld l, 0
    ld a, [wPlayerXScreen]
    add a, HIGH(wScreen0Object0)
    ld h, a
    ld c, NB_OBJECTS_PER_SCREEN
.drawObject
    push bc
    push hl
    call ProcessAndDrawObject
    pop hl
    pop bc
    ld a, l
    and -sizeof_Object
    add a, sizeof_Object
    ld l, a
    ; Avoid overflowing OAM
    ld a, e
    cp $A0 - 7 * 4 ; Score only uses 3 objects, but no object uses only 1 sprite.
    ret nc
    dec c
    jr nz, .drawObject

.clearObjects
    xor a
    ld [de], a
    ld a, e
    add a, 4
    ld e, a
    cp $A0 - 6 * 4
    jr nz, .clearObjects
    ret


.downTransition
    ld a, [wTransitionCounter]
    and a
    jr nz, .notFirstFrame
    ; xor a
    ld [wPlayer_YPos], a
    ld hl, wGeneratedScreens
    ld c, NB_SCREENS_PER_ROW
    ; xor a
    rst memset_small
    inc a ; ld a, 1
    ld [wTransitionCounter], a
    ld hl, wPlayerDepth
    ld a, [hl]
    inc a
    cp 251
    jr nz, .noWrap
    sub 25
.noWrap
    ld [hl], a
    call DrawScreen
    ld a, SCRN_Y
    ldh [hWY], a
    ld a, 7
    ldh [hWX], a
    ; TODO: generate sprites for next screen
    ld a, [wWhichTilemap]
    sub $98
    jr z, .using98Map
    ld a, LCDCF_WIN9C00 | LCDCF_BG9C00
.using98Map
    xor LCDCF_ON | LCDCF_WINON | LCDCF_WIN9800 | LCDCF_BG8800 | LCDCF_BG9C00 | LCDCF_OBJON | LCDCF_OBJ16 | LCDCF_BGON
    ldh [hLCDC], a
.notFirstFrame

    ld hl, wTransitionCounter
    inc [hl]
    ld a, [hl]
    sub 10
    ld de, wShadowOAM
    jr nz, .dontMovePlayer
    inc a
    ld [hl], a
    ld e, LOW(wShadowOAM + 4 * 2)
.dontMovePlayer
.moveSprites
    ld a, [de]
    cp SCRN_Y + 16
    jr nc, .dontMoveSprite
    sub 4
    ld [de], a
.dontMoveSprite
    ld a, e
    add a, 4
    ld e, a
    cp $A0 - 6 * 4
    jr nz, .moveSprites
    ld a, d ; ld a, HIGH(wShadowOAM)
    ldh [hOAMBufferHigh], a
    ldh a, [hSCY]
    add a, 4
    ldh [hSCY], a
    ldh a, [hWY]
    sub 4
    ldh [hWY], a
    ret nz
    ; Last frame, set up turning back to normal
    ld [wGameState], a
    ldh [hSCY], a
    ld a, SCRN_Y
    ldh [hWY], a
    ldh a, [hLCDC]
    xor LCDCF_BG9C00
    ldh [hLCDC], a
    ret

.leftTransition
.rightTransition
    ; TODO:
    ret



; Draw the current screen, including generating it if it's not generated yet
DrawScreen::
    ld a, [wPlayerXScreen]
    ld l, a
    ld h, HIGH(wGeneratedScreens)
    ld a, [hl]
    and a
    jp nz, .screenGenerated
    inc a ; ld a, 1
    ld [hl], a

    ; Generate the screen

    ; Clear the screen's objects
    ld a, l
    add a, HIGH(wScreen0Object0)
    ld h, a
    xor a
    ld l, a;  ld l, 0
    ld c, 0 ; ld c, NB_OBJECTS_PER_SCREEN * sizeof_Object
    rst memset_small

    ; Fill the table with dirt
    ld a, [wPlayerXScreen]
    add a, a
    add a, LOW(ScreenMapTable)
    ld l, a
    adc a, HIGH(ScreenMapTable)
    sub l
    ld h, a
    ld a, [hli]
    ld h, [hl]
    ld l, a
    ld [wWhichScreenMap], a
    ld a, h
    ld [wWhichScreenMap+1], a
    ld a, BLOCK_DIRT
    ld c, (SCRN_X_B / 2 - 1) * (SCRN_Y_B / 2)
    rst memset_small

    xor a
    ld [wOOBTile], a

    ; Special case: on surface, clear the top half of the screen, and disable rest of generation
    ld a, [wPlayerDepth]
    and a
    jr nz, .notFirstLayer
    ld hl, wWhichScreenMap
    ld a, [hli]
    ld h, [hl]
    ld l, a
    xor a
    ld c, (SCRN_X_B / 2 - 1) * (SCRN_X_B / 4)
    rst memset_small
    jp .screenGenerated
.notFirstLayer

    ld a, BLOCK_UNBREAKABLE
    ld [wOOBTile], a

    ; Generate a few holes, maybe?
    call RandInt
    and $07
    jr z, .noHoles
    ld e, a
.createHole
    call RandInt
    cp (SCRN_X_B / 2 - 1) * (SCRN_Y_B / 2)
    jr nc, .skipHole ; Prevent overflows, but also fails ~1/5 of the time
    ld a, [wWhichScreenMap]
    add a, h
    ld l, a
    ld a, [wWhichScreenMap+1]
    adc a, 0
    ld h, a
    xor a
    ld [hld], a
    ; TODO: maybe create larger holes? Using other bits from the generated number
.skipHole
    dec e
    jr nz, .createHole
.noHoles

    ; Maybe generate some food?
    call RandInt
    and 1
    jr z, .noFood
    call RandInt
    cp SCRN_Y - $10
    jr nc, .noFood
    ld b, h
    ld c, l
    call GetEmptyObject
    jr nz, .noFood
    ld a, 1
    ld [hli], a ; Exists
    ld a, b
    and $30
    ld a, 0
    jr nz, .noRareFood
    bit 7, c
    jr nz, .noRareFood
    ld a, STATE_GOLDEN_POTATO - STATE_CARROT
.noRareFood
    ld d, a
    add a, STATE_CARROT
    ld [hli], a ; State
    inc l ; inc hl
    ld a, b
    and $F0
    ld [hli], a ; Y pos
    ld a, c
    and $70
    add a, $08
    ld [hli], a ; X pos
    ld a, d
    inc a
    ld [hli], a ; Frame
    ld a, LOW(FoodDrawLists)
    ld [hli], a
    ld [hl], HIGH(FoodDrawLists)
.noFood

    ; Perhaps generate some goodies? OwO
    call RandInt
    and 1
    jr z, .noGoodies
    call RandInt
    cp SCRN_Y - $10
    jr nc, .noGoodies
    ld d, h
    ld e, l
    call RandInt
    call GetEmptyObject
    jr nz, .noGoodies
    ld a, 1
    ld [hli], a ; Exists
    ldh a, [hRandIntHigh]
    and $07
    ld a, 1
    jr nz, .gotRarity
    ldh a, [hRandIntLow]
    and $07
    ld a, 2
    jr nz, .gotRarity
    xor a
.gotRarity
    ld d, a
    add a, STATE_DIAMOND
    ld [hli], a ; State
    inc l ; inc hl
    ld a, d
    and $F0
    ld [hli], a ; Y pos
    ld a, e
    and $70
    add a, $08
    ld [hli], a ; X pos
    ld a, d
    inc a
    add a, a
    ld [hli], a ; Frame
    ld a, LOW(GoodiesDrawLists)
    ld [hli], a
    ld [hl], HIGH(GoodiesDrawLists)
.noGoodies

    ld a, [wPlayerDepth]
.checkHaven
    sub 25
    jr c, .notHaven
    jr nz, .checkHaven
    ld a, [wHavensEncountered]
    cp 10
    jr z, .dontIncHavens
    inc a
    ld [wHavensEncountered], a
.dontIncHavens
    ld hl, wWhichScreenMap
    ld a, [hli]
    ld h, [hl]
    ld l, a
    ld c, 9
    xor a
    rst memset_small
    ld [wOOBTile], a
    jr .screenGenerated
.notHaven
    ld a, BLOCK_UNBREAKABLE
    ld [wOOBTile], a

    ; Generate some rocks (always! But a different amount depending on depth)
    call RandInt
    and $7F
    ld h, a
    ld a, [wHavensEncountered]
    add a, LOW(RockCountsTable)
    ld e, a
    adc a, HIGH(RockCountsTable)
    sub e
    ld d, a
    ld a, [de] ; Base rolls
    add a, h
    ld e, a
.generateRock
    call RandInt
    cp SCRN_Y
    jr nc, .done
    and $F0
    ld [wCoordX], a
    ld a, l
    and $70
    ld [wCoordY], a
    call GetTerrainAt
    and a
    jr z, .ok
    dec a
    jr nz, .generateRock
.ok
    ld d, h
    ld e, l
    call GetEmptyObject
    jr nz, .noMoreRocks
    ld a, BLOCK_ROCK
    ld [de], a
    ld a, 1
    ld [hli], a
    ld a, STATE_ROCK_WATCHING
    ld [hli], a
    inc l ; inc hl
    ld a, [wCoordY]
    ld [hli], a
    ld a, [wCoordX]
    ld [hli], a
    xor a
    ld [hli], a
    ld a, LOW(FallingRockDrawLists)
    ld [hli], a
    ld [hl], HIGH(FallingRockDrawLists)
.done
    dec e
    jr nz, .generateRock
.noMoreRocks
    
    ; Empty tile where player stands, so things make sense
    ld a, [wPlayer_YPos]
    and $F0
    ; *9
    rrca
    ld e, a
    rrca
    rrca
    rrca
    add a, e
    ld e, a
    ld a, [wPlayer_XPos]
    sub 16
    and $F0
    swap a
    add a, e
    ld e, a
    ld a, [wWhichScreenMap]
    add a, e
    ld e, a
    ld a, [wWhichScreenMap+1]
    adc a, 0
    ld d, a
    xor a
    ld [de], a

.screenGenerated
    ld a, [wWhichTilemap]
    xor HIGH(_SCRN0) ^ HIGH(_SCRN1)
    ld [wWhichTilemap], a
    ld h, a
    xor a
    ld l, a
    lb bc, $20, SCRN_Y_B
    ld de, SCRN_VX_B - SCRN_X_B
.drawBorder
    wait_vram
    ld [hl], b
    ld a, l
    add a, SCRN_X_B - 1
    ld l, a
    ld a, b
    inc a
    inc a
    ld [hli], a
    add hl, de
    ld a, b
    xor 1
    ld b, a
    dec c
    jr nz, .drawBorder

    ld a, [wPlayerXScreen]
    add a, a
    add a, LOW(ScreenMapTable)
    ld l, a
    adc a, HIGH(ScreenMapTable)
    sub l
    ld h, a
    ld a, [hli]
    ld d, [hl]
    ld e, a

    ld a, [wWhichTilemap]
    ld h, a
    ld l, 1

    ld b, SCRN_Y_B / 2
.drawRow
    ld c, SCRN_X_B / 2 - 1
.drawBlock
    ld a, [de]
    and a
    jr nz, .notBackground
    ; Background tiles depend on their position, not just the block type
    ; TODO:
    ld a, $80 >> 2 + 1
.notBackground
    dec a
    add a, a
    add a, a
    push af
    wait_vram
    pop af
    ld [hl], a
    inc a
    set 5, l
    ld [hli], a
    push af
    wait_vram
    pop af
    inc a
    inc a
    ld [hl], a
    res 5, l
    dec a
    ld [hli], a
    inc de
    dec c
    jr nz, .drawBlock
    ld a, l
    add a, SCRN_VX_B * 2 - SCRN_X_B + 2
    ld l, a
    adc a, h
    sub l
    ld h, a
    dec b
    jr nz, .drawRow
    ret


; Looks for an empty object slot in the current screen
; @return Z Set if an object was found
; @return hl Points to the free object slot if one was found
GetEmptyObject:
    ld a, [wPlayerXScreen]
    add a, HIGH(wScreen0Object0)
    ld h, a
    xor a
    ld l, a
.searchSprite
    ld a, [hl]
    and a
    ret z
    ld a, l
    add a, sizeof_Object
    ld l, a
    jr nz, .searchSprite
    inc a
    ret


; Draws the object at `hl`
; @param hl A pointer to the object to be drawn
; @param de A pointer to where the sprites will be written to (normally some OAM shadow)
; @destroy a bc
ProcessAndDrawObject:
    ld a, [hli] ; Exists
    and a
    ret z ; Nope?
    ld a, [hli] ; State
    ld b, a
    ld c, [hl] ; Counter
    push hl
    add a, a
    add a, LOW(StateProcessorTable)
    ld l, a
    adc a, HIGH(StateProcessorTable)
    sub l
    ld h, a
    ld a, [hli]
    ld h, [hl]
    ld l, a
    push de
    rst call_hl
    pop de
    pop hl
    jr nz, .noStateChange
    dec l ; dec hl
    ld a, b
    ld [hli], a
.noStateChange
    ld a, c
    ld [hli], a ; Counter
    ld a, [hli] ; Y pos
    ldh [hObjectY], a
    ld a, [hli] ; X pos
    ldh [hObjectX], a
    ld a, [hli] ; Frame
    add a, a
    ld b, a
    ld a, [hli] ; Frame ptr
    add a, b
    ld c, a
    adc a, [hl]
    push hl
    sub c
    ld h, a
    ld l, c
    ld a, [hli]
    ld h, [hl]
    ld l, a
    ; First, read len
    ld a, [hli]
    and a
    jr z, .noDrawing ; Don't draw if hidden
    ld c, a
    xor a
    ldh [hObjectTile], a
    ldh [hObjectAttr], a
.drawSprite
    ldh a, [hObjectY]
    ld b, a
    ld a, [hli]
    add a, b
    ldh [hObjectY], a
    ld [de], a
    inc e ; inc de
    ldh a, [hObjectX]
    ld b, a
    ld a, [hli]
    add a, b
    ldh [hObjectX], a
    ld [de], a
    inc e ; inc de
    ldh a, [hObjectTile]
    ld b, a
    ld a, [hli]
    add a, b
    ldh [hObjectTile], a
    ld [de], a
    inc e ; inc de
    ldh a, [hObjectAttr]
    ld b, a
    ld a, [hli]
    add a, b
    ldh [hObjectAttr], a
    ld [de], a
    inc e ; inc de
    dec c
    jr nz, .drawSprite
.noDrawing
    pop hl
    ret


; Computes the pointer to the coordinates in wCoord*
; Also updates wWhichScreenMap
; @return hl
; @destroy b
GetPointerToCoords:
    ld a, [wCoordY]
    swap a
    ld b, a
    add a, a
    add a, a
    add a, a
    add a, b
    ld b, a
    ld a, [wCoordX]
    sub 16
    swap a
    add a, b
    ld b, a
    ld a, [wPlayerXScreen]
    add a, a
    add a, LOW(ScreenMapTable)
    ld l, a
    adc a, HIGH(ScreenMapTable)
    sub l
    ld h, a
    ld a, [hli]
    ld [wWhichScreenMap], a
    ld h, [hl]
    add a, b
    ld l, a
    ld a, h
    ld [wWhichScreenMap+1], a
    adc a, 0
    ld h, a
    ret

; Gets the terrain type at the given coordinates
; Note: also updates wWhichScreenMap
; @param wCoordY, wCoordX
; @destroy hl b
GetTerrainAt:
    call GetPointerToCoords
    ld a, [wCoordY]
    sub SCRN_Y
    ret z
    ld a, [wCoordX]
    and a
    jr nz, .notLeft
    ld b, a
    ld a, [wPlayerXScreen]
    and a
    jr z, .preventMoving
    ld a, b
.notLeft
    sub $10
    cp SCRN_X - $10
    jr c, .notOOB
    jr nz, .notRight
    ld a, [wPlayerXScreen]
    cp NB_SCREENS_PER_ROW - 1
    jr z, .preventMoving
.notRight
    ld a, [wOOBTile]
    db $2E
.notOOB
    ld a, [hl]
    and a
    ret

.preventMoving
    ld a, BLOCK_UNBREAKABLE
    ret


DamageRock:
    call GetPointerToCoords
    ld a, [hl]
    inc a
    call ChangeBlock
    ld a, 16
    ld [wDigDelay], a
    and a
    ret

DigBlock:
    cp BLOCK_DIRT
    jr z, .ok
    ld d, a
    ld a, [wDigDelay]
    and a
    ret nz
    ld a, d
    cp BLOCK_ROCK_DAMAGED
    jr nz, DamageRock
.ok
    ld b, 0
    ld de, 10
    call AddToScore

    call GetEmptyObject
    jr nz, .dontSpawnDebris
    inc a ; ld a, 1
    ld [hli], a
    ld a, STATE_DEBRIS_SHATTERING
    ld [hli], a
    ld a, 8
    ld [hli], a
    ld a, [wCoordY]
    ld [hli], a
    ld a, [wCoordX]
    ld [hli], a
    xor a
    ld [hli], a
    ld a, LOW(DebrisDrawLists)
    ld [hli], a
    ld [hl], HIGH(DebrisDrawLists)
.dontSpawnDebris

    xor a
ChangeBlock::
    ld c, a
    call GetPointerToCoords
    ld a, [wCoordY]
    sub SCRN_Y
    ret z
    ld a, [wCoordX]
    sub $10
    cp SCRN_X - $10
    ret nc
    ld [hl], c
    ld a, [wCoordY]
    and $F0
    ld l, a
    ld a, [wWhichTilemap]
    rra
    rra
    ld h, a
    add hl, hl
    add hl, hl
    ld a, [wCoordX]
    and $F0
    rra
    rra
    rra
    add a, l
    ld l, a
    dec l

    ld a, c
    and a
    jr nz, .notBackground
    ; Background tiles depend on their position, not just the block type
    ; TODO:
    ld a, $80 >> 2 + 1
.notBackground
    dec a
    add a, a
    add a, a
    ld c, a
    wait_vram
    ld a, c
    ld [hl], a
    inc a
    set 5, l
    ld [hli], a
    wait_vram
    ld a, c
    inc a
    inc a
    inc a
    ld [hl], a
    res 5, l
    dec a
    ld [hli], a

    xor a
    ret



; Adds the score in `bde` to the score, and redraws it
AddToScore:
    ld hl, wScore + 2
    ld a, [hld]
    add a, e
    daa
    ld [wNewScore+2], a
    ld a, [hld]
    adc a, d
    daa
    ld [wNewScore+1], a
    ld a, [hl]
    adc a, b
    daa
    ld [wNewScore], a

    ; ld hl, wScore
    ld de, wNewScore
    ld c, LOW($8000)
.compare
    ld a, [hl]
    and $F0
    ld b, a
    ld a, [de]
    and $F0
    cp b
    jr z, .sameHighDigits
    push hl
    ld l, a
    ld h, HIGH(DigitTiles)
    ld b, $80
.copyHighDigit
    wait_vram
    ld a, [hli]
    ld [bc], a
    inc c
    ld a, l
    and $0F
    jr nz, .copyHighDigit
    pop hl
    jr .skipHighDigitSkip
.sameHighDigits
    ld a, c
    and $F0
    add a, $10
    ld c, a
.skipHighDigitSkip
    ld a, [hl]
    and $0F
    ld b, a
    ld a, [de]
    ld [hli], a
    inc de
    and $0F
    cp b
    jr z, .sameLowDigits
    push hl
    swap a
    ld l, a
    ld h, HIGH(DigitTiles)
    ld b, $80
.copyLowDigit
    wait_vram
    ld a, [hli]
    ld [bc], a
    inc c
    ld a, l
    and $0F
    jr nz, .copyLowDigit
    pop hl
    jr .skipLowDigitSkip
.sameLowDigits
    ld a, c
    and $F0
    add a, $10
    ld c, a
.skipLowDigitSkip
    ld a, c
    cp $60
    jr nz, .compare
    ret


ScreenMapTable:
SCREEN_ID = 0
REPT NB_SCREENS_PER_ROW
SCREEN_NAME equs STRCAT("wScreen", STRCAT(STRSUB("{SCREEN_ID}", 2, STRLEN("{SCREEN_ID}") - 1), "Map"))
    dw SCREEN_NAME
    PURGE SCREEN_NAME
SCREEN_ID = SCREEN_ID + 1
ENDR
PURGE SCREEN_ID


RockCountsTable:
    db 10
    db 17
    db 22
    db 28
    db 35
    db 40
    db 48
    db 53
    db 59
    db 63



SECTION "Initial structs", ROM0

PlayerObject:
    db 1
    db STATE_PLAYER_STILL
    db 0
    db 0
    db 5 * 16
    db 0
    dw PlayerDrawLists


SECTION "Draw lists", ROM0

InvisibleDrawList:
    db 0

PlayerDrawLists:
    dw .leftDrawList
    dw .rightDrawList
    dw .leftWalk1DrawList
    dw .rightWalk1DrawList
    dw .leftWalk2DrawList
    dw .rightWalk2DrawList

.leftDrawList
    db 2

    db 16, 0
    db 6
    db OAMF_PAL0

    db 0, 8
    db 2
    db 0

.rightDrawList
    db 2

    db 16, 8
    db 6
    db OAMF_PAL0 | OAMF_XFLIP

    db 0, -8
    db 2
    db 0

.leftWalk1DrawList
    db 2

    db 16, 0
    db 10
    db OAMF_PAL0

    db 0, 8
    db 2
    db 0

.rightWalk1DrawList
    db 2

    db 16, 8
    db 10
    db OAMF_PAL0 | OAMF_XFLIP

    db 0, -8
    db 2
    db 0

.leftWalk2DrawList
    db 2

    db 16, 0
    db 14
    db OAMF_PAL0

    db 0, 8
    db 2
    db 0

.rightWalk2DrawList
    db 2

    db 16, 8
    db 14
    db OAMF_PAL0 | OAMF_XFLIP

    db 0, -8
    db 2
    db 0


FallingRockDrawLists:
    dw InvisibleDrawList
    dw .shakingLeft
    dw .shakingMiddle
    dw .shakingRight
    dw .falling

.shakingLeft
    db 2
    
    db 16, -1
    db $42
    db OAMF_PAL0
    
    db 0, 8
    db 2
    db 0

.shakingMiddle
    db 2
    
    db 16, 0
    db $42
    db OAMF_PAL0
    
    db 0, 8
    db 2
    db 0

.shakingRight
    db 2
    
    db 16, 1
    db $42
    db OAMF_PAL0
    
    db 0, 8
    db 2
    db 0

.falling
    db 2

    db 16, 0
    db 14
    db OAMF_PAL0

    db 0, 8
    db 2
    db 0


FoodDrawLists:
    dw InvisibleDrawList
    dw .carrot
    dw .potato1
    dw .potato2

.carrot
    db 2

    db 16, 8
    db $1E
    db 0

    db 0, 8
    db 2
    db 0

.potato1
    db 2

    db 16, 8
    db $2A
    db 0

    db 0, 8
    db 2
    db 0

.potato2
    db 2
    
    db 16, 8
    db $2E
    db 0

    db 0, 8
    db 2
    db 0


GoodiesDrawLists:
    dw InvisibleDrawList
    dw 0
    dw .diamond1
    dw .diamond2
    dw .emerald1
    dw .emerald2
    dw .crystal1
    dw .crystal2

.emerald1
    db 2

    db 16, 8
    db $32
    db 0

    db 0, 8
    db 2
    db 0

.emerald2
    db 2

    db 16, 8
    db $36
    db 0

    db 0, 8
    db 2
    db 0

.diamond1
    db 2

    db 16, 8
    db $22
    db 0

    db 0, 8
    db 2
    db 0

.diamond2
    db 2

    db 16, 8
    db $26
    db 0

    db 0, 8
    db 2
    db 0

.crystal1
    db 2

    db 16, 8
    db $3A
    db 0

    db 0, 8
    db 2
    db 0

.crystal2
    db 2

    db 16, 8
    db $3E
    db 0

    db 0, 8
    db 2
    db 0


DebrisDrawLists:
    dw .debris
    dw .rockDebris

.debris
    db 2

    db 16, 0
    db $1A
    db 0

    db 0, 8
    db 2
    db 0

.rockDebris
    db 2

    db 16, 0
    db $16
    db 0

    db 0, 8
    db 2
    db 0


SECTION "State processing", ROM0

; A state function should set the Z flag on exit to signify it wants to change the state
; The state will be changed to the value of B
StateProcessorTable:
    dw .playerStill
    dw .playerLeft
    dw .playerRight
    dw .playerFalling
    dw .playerSquish

    dw .rockWatching
    dw .rockShaking
    dw .rockShaking
    dw .rockShaking
    dw .rockFalling
    dw .rockFalling
    dw .rockFalling

    dw .rockDebrisShattering
    dw .debrisShattering

    dw .carrot
    dw .potato

    dw .diamond
    dw .emerald
    dw .crystal


.playerStill
    ld hl, wPlayer_YPos
    ld a, [hli]
    add a, 16
    ld [wCoordY], a
    ld a, [hli]
    ld [wCoordX], a
    ld a, [wPlayerDirection]
    ld [hl], a
    call GetTerrainAt
    and a
    jr z, .beginPlayerFall

    ldh a, [hHeldButtons]
    ld e, a
    bit PADB_DOWN, e
    jr z, .noDown
    ld hl, wPlayer_YPos
    ld a, [hli]
    add a, 16
    ld [wCoordY], a
    ld a, [hl]
    ld [wCoordX], a
    call GetTerrainAt
    cp BLOCK_UNBREAKABLE
    jr nz, .digDown
.noDown

    bit PADB_LEFT, e
    jr z, .noLeft
    ld hl, wPlayer_YPos
    ld a, [hli]
    ld [wCoordY], a
    ld a, [hl]
    sub 16
    ld [wCoordX], a
    call GetTerrainAt
    and a
    lb bc, STATE_PLAYER_LEFT, 8
    ret z
    cp BLOCK_UNBREAKABLE
    jr nz, .digLeft
.noLeft

    bit PADB_RIGHT, e
    jr z, .noRight
    ld hl, wPlayer_YPos
    ld a, [hli]
    ld [wCoordY], a
    ld a, [hl]
    add a, 16
    ld [wCoordX], a
    call GetTerrainAt
    and a
    lb bc, STATE_PLAYER_RIGHT, 8
    ret z
    cp BLOCK_UNBREAKABLE
    jr nz, .digRight
.noRight

    xor a
    inc a
    ret

.digLeft
    call DigBlock
    ret nz
    lb bc, STATE_PLAYER_LEFT, 8
    ret

.digRight
    call DigBlock
    ret nz
    lb bc, STATE_PLAYER_RIGHT, 8
    ret

.digDown
    call DigBlock
    ret nz
    ; Fallthrough

.beginPlayerFall
    lb bc, STATE_PLAYER_FALLING, 8
    xor a
    ret


.playerLeft
    ld hl, wPlayer_XPos
    dec [hl]
    dec [hl]
    jr z, .initLeftTransition
    xor a
.playerWalkingAnimation
    ld [wPlayerDirection], a
    ld e, a
    dec c
    ld a, c
    and 3
    jr z, .gotFrame
    ld a, c
    and 4
    jr nz, .gotFrame
    ld a, 2
.gotFrame
    or e
    ld [wPlayer_Frame], a
    ld b, STATE_PLAYER_STILL
    ld a, c
    and a
    ret

.playerRight
    ld hl, wPlayer_XPos
    inc [hl]
    inc [hl]
    ld a, 1
    jr .playerWalkingAnimation
.initRightTransition
    ld a, GAME_STATE_RIGHT_TRANSITION
    ld [wGameState], a
    ld b, STATE_PLAYER_STILL
    xor a
    ret

.initLeftTransition
    ld a, GAME_STATE_LEFT_TRANSITION
    ld [wGameState], a
    ld b, STATE_PLAYER_STILL
    xor a
    ret

.playerFalling
    ld hl, wPlayer_YPos
    inc [hl]
    inc [hl]
    dec c
    ret nz
    ld a, [hli]
    cp SCRN_Y - $10
    jr nc, .initDownTransition
    add a, 16
    ld [wCoordY], a
    ld a, [hl]
    ld [wCoordX], a
    call GetTerrainAt
    and a
    lb bc, STATE_PLAYER_FALLING, 8
    ret z
    ld b, STATE_PLAYER_STILL
    xor a
    ret

.initDownTransition
    ld a, GAME_STATE_DOWN_TRANSITION
    ld [wGameState], a
    lb bc, STATE_PLAYER_STILL, 0
    xor a
    ret


.playerSquish
    ret


.rockWatching
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l ; inc hl
    ld a, [hli]
    ld [wCoordY], a
    ld a, [hl]
    ld [wCoordX], a
    call GetTerrainAt
    and a
    lb bc, STATE_ROCK_DEBRIS_SHATTERING, 4
    ret z
    ld a, [wCoordY]
    add a, 16
    ld [wCoordY], a
    call GetTerrainAt
    and a
    ret nz
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l
    inc l
    inc l
    ld [hl], 1
    ld a, [wCoordY]
    sub 16
    ld [wCoordY], a
    call GetTerrainAt
    add a, STATE_ROCK1_SHAKING - BLOCK_ROCK
    ld b, a
    ld c, 16
    xor a
    ret

.rockShaking
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l
    inc l
    inc l
    dec c
    ld a, c
    and 6
    jr nz, .gotAnimationFrame
    ld a, 4
.gotAnimationFrame
    rra
    ld [hld], a
    ld a, c
    and a
    ret nz
    ld a, b
    sub STATE_ROCK1_SHAKING - STATE_ROCK1_FALLING
    ld d, a
    ld a, [hld]
    ld [wCoordX], a
    ld a, [hl]
    ld [wCoordY], a
    xor a
    call ChangeBlock
    ld b, d
    ld c, 8
    xor a
    ret

.rockFalling
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l ; inc hl
    ld a, [hl]
    add a, 2
    ld [hl], a
    dec c
    ret nz
    ld a, [hli]
    add a, 16
    ld [wCoordY], a
    ld a, [hli]
    ld [wCoordX], a
    ld e, l
    ld d, b
    call GetTerrainAt
    and a
    ld b, d
    jr nz, .doneFalling
    ld c, 8
    xor a
    ret
.doneFalling
    ld a, [wCoordY]
    sub 16
    ld [wCoordY], a
    ld e, a
    ld a, [wPlayer_YPos]
    cp e
    jr nz, .notGameOver
    ld a, [wCoordX]
    ld e, a
    ld a, [wPlayer_XPos]
    cp e
    jp z, GameOver
.notGameOver
    ld a, b
    sub STATE_ROCK1_FALLING - BLOCK_ROCK
    call ChangeBlock
    ld b, STATE_ROCK_WATCHING
    ld hl, sp+5
    ld h, [hl]
    ld l, e
    xor a
    ld [hl], a
    ret

.rockDebrisShattering
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l
    inc l
    inc l
    ld a, 1
    ld [hli], a
    ld a, LOW(DebrisDrawLists)
    ld [hli], a
    ld [hl], HIGH(DebrisDrawLists)
    dec c
    ret nz
    lb bc, STATE_DEBRIS_SHATTERING, 4
    ret

.debrisShattering
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l
    inc l
    inc l
    xor a
    ld [hli], a
    ld a, LOW(DebrisDrawLists)
    ld [hli], a
    ld [hl], HIGH(DebrisDrawLists)
    dec c
    ret nz
    ; Destroy the object
    ld hl, sp+4
    ld a, [hli]
    ld h, [hl]
    ld l, a
    dec l ; dec hl
    dec l ; dec hl
    ld [hl], 0
    ret


.carrot
    call CheckPlayerOverlap
    ret nz
    inc a ; FIXME:
    ret

.potato
    call DoGoodyAnimationAndCheckOverlap
    ret nz
    inc a ; FIXME:
    ret

.diamond
    call DoGoodyAnimationAndCheckOverlap
    ret nz
    ld b, 5
    ld d, a
    ld e, a ; ld de, 0
    jp AddToScore

.emerald
    call DoGoodyAnimationAndCheckOverlap
    ret nz
    ld b, a ; ld b, 0
    ld de, $100
    jp AddToScore

.crystal
    call DoGoodyAnimationAndCheckOverlap
    ret nz
    ld b, a ; ld b, 0
    ld de, $2000
    jp AddToScore


DoGoodyAnimationAndCheckOverlap:
    ld hl, sp+6
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l ; inc hl
    ld a, [wAnimationFrameCounter]
    and $0F
    jr nz, _CheckPlayerOverlap
    inc l ; inc hl
    inc l ; inc hl
    ld a, [hl]
    xor 1
    ld [hld], a
    dec l ; dec hl
    jr _CheckPlayerOverlap
CheckPlayerOverlap:
    ld hl, sp+6
    ld a, [hli]
    ld h, [hl]
    ld l, a
    inc l ; inc hl
_CheckPlayerOverlap:
    ld a, [wPlayer_YPos]
    cp [hl]
    ret nz
    inc l ; inc hl
    ld a, [wPlayer_XPos]
    sub 8
    cp [hl]
    ret nz
    dec l ; dec hl
    dec l ; dec hl
    dec l ; dec hl
    dec l ; dec hl
    xor a
    ld [hl], a
    ret
