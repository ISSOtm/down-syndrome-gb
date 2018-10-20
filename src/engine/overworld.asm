
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
    rst wait_vblank


    ld a, HIGH(_SCRN1)
    ld [wWhichTilemap], a

    ld de, OverworldGfx
    ld hl, $9000
    ld b, 32
    call pb16_unpack_block

    ld hl, $8000
    ld b, 6
.initScoreTiles
    ld de, ZeroTile
    ld c, 16
    rst memcpy_small
    dec b
    jr nz, .initScoreTiles

    ld hl, wShadowOAM
    xor a
    ld c, $A0 - (6 / 2) * 4
    rst memset_small
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

    ; Draw player
    ld de, wShadowOAM
    ld hl, wPlayer
    call ProcessAndDrawObject

    ; Draw screen objects
    ld l, a
    ld a, [wPlayerXScreen]
    rra
    rr l
    add a, HIGH(wScreen0Object0)
    ld h, a
    ld c, 16
.drawObject
    push bc
    call ProcessAndDrawObject
    pop bc
    ; Avoid overflowing OAM
    ld a, e
    cp $A0 - 4 * 4 ; Score only uses 3 objects, but no object uses only 1 sprite.
    ret nc
    dec c
    jr nz, .drawObject

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
    xor a
    ld [wTransitionCounter], a
    ret


.downTransition
    ld a, [wTransitionCounter]
    and a
    jr nz, .notFirstFrame
    ld hl, wPlayerDepth
    inc [hl]
    ld hl, wGeneratedScreens
    ld c, NB_SCREENS_PER_ROW
    ; xor a
    rst memset_small
    call DrawScreen
    ld a, SCRN_Y
    ldh [hWY], a
    ld a, 7
    ldh [hWX], a
    ld a, [wWhichTilemap]
    sub $98
    jr z, .using98Map
    ld a, LCDCF_WIN9C00 | LCDCF_BG9C00
.using98Map
    xor LCDCF_BGON | LCDCF_WINON | LCDCF_WIN9800 | LCDCF_BG8800 | LCDCF_BG9C00 | LCDCF_OBJON | LCDCF_OBJ16 | LCDCF_BGON
    ldh [hLCDC], a
.notFirstFrame
    ld hl, wTransitionCounter
    inc [hl]
    ldh a, [hWY]
    sub 4
    ldh [hWY], a
    ld a, [wPlayer_YPos]
    sub 4
    ld [wPlayer_YPos], a
.leftTransition
.rightTransition
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
    ld h, l
    ld l, a ; ld l, 0
    rr h
    rr l
    ld a, HIGH(wScreen0Object0)
    add a, h
    ld h, a
    xor a
    ld c, 16 * sizeof_Object
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
    jr .screenGenerated
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

    ; Generate some rocks (always! But a different amount depending on depth)
    call RandInt
    ; Add a min, add a max
    
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
    lb bc, $18, SCRN_Y_B
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
    wait_vram
    ld a, [de]
    and a
    jr nz, .notBackground
    ; Background tiles depend on their position, not just the block type
    ; TODO:
.notBackground
    add a, a
    add a, a
    ld [hl], a
    inc a
    set 5, l
    ld [hli], a
    wait_vram
    ld a, [de]
    add a, a
    inc a
    add a, a
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


; Draws the object at `hl`
; @param hl A pointer to the object to be drawn
; @param de A pointer to where the sprites will be written to (normally some OAM shadow)
; @destroy a bc
ProcessAndDrawObject:
    ld a, [hli] ; Exists
    and a
    ret z ; Nope?
    ; Don't process objects during transition states
    ld a, [wGameState]
    and a
    ld a, [hli] ; State
    ld c, [hl] ; Counter
    jr nz, .dontProcess
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
.dontProcess
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
    sub c
    ld h, a
    ld l, c
    ld a, [hli]
    ld h, [hl]
    ld l, a
    ; First, read len
    ld a, [hli]
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
    ret


; Gets the terrain type at the given coordinates
; Note: also updates wWhichScreenMap
; @param wCoordY, wCoordX
; @destroy hl b
GetTerrainAt:
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
    ld a, [wCoordY]
    sub SCRN_Y
    ret z
    ld a, [wCoordX]
    sub $10
    cp SCRN_X - $10
    jr c, .notOOB
    ld a, [wOOBTile]
    db $2E
.notOOB
    ld a, [hl]
    and a
    ret


ChangeBlock::
    ld c, a
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

    wait_vram
    ld a, c
    and a
    jr nz, .notBackground
    ; Background tiles depend on their position, not just the block type
    ; TODO:
.notBackground
    add a, a
    add a, a
    ld [hl], a
    inc a
    set 5, l
    ld [hli], a
    wait_vram
    ld a, c
    add a, a
    inc a
    add a, a
    inc a
    ld [hl], a
    res 5, l
    dec a
    ld [hli], a
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
    



SECTION "Digit tiles", ROM0,ALIGN[8]

DigitTiles:

ZeroTile:
    dw `00111100
    dw `01133110
    dw `01311310
    dw `01311310
    dw `01311310
    dw `01311310
    dw `01133110
    dw `00111100


SECTION "Initial structs", ROM0

PlayerObject:
    db 1
    db STATE_PLAYER_STILL
    db 0
    db 0
    db 5 * 16
    db 0
    dw PlayerDrawLists

FallingRockObject:
    db 1
    db STATE_ROCK_SHAKING
    db 16
    dw ; This will be written by the function creating the object
    db 0
    dw FallingRockDrawLists


SECTION "Draw lists", ROM0

PlayerDrawLists:
    dw .leftDrawList
    dw .rightDrawList

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
    db 8
    db OAMF_PAL0 | OAMF_XFLIP

    db 0, 8
    db -2
    db 0


FallingRockDrawLists:
    dw .shakingLeft
    dw .shakingMiddle
    dw .shakingRight
    dw .falling

.shakingLeft
    db 2
    
    db 16, -1
    db 10
    db OAMF_PAL0
    
    db 0, 8
    db 2
    db 0

.shakingMiddle
    db 2
    
    db 16, 0
    db 10
    db OAMF_PAL0
    
    db 0, 8
    db 2
    db 0

.shakingRight
    db 2
    
    db 16, 1
    db 10
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


SECTION "State processing", ROM0

; A state function should set the Z flag on exit to signify it wants to change the state
; The state will be changed to the value of B
StateProcessorTable:
    dw .playerStill
    dw .playerLeft
    dw .playerRight
    dw .playerFalling
    dw .playerSquish

    dw .rockShaking
    dw .rockFalling

    dw .debrisShattering


.playerStill
    ld hl, wPlayer_YPos
    ld a, [hli]
    add a, 16
    ld [wCoordY], a
    ld a, [hl]
    ld [wCoordX], a
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
    lb bc, STATE_PLAYER_LEFT, 16
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
    lb bc, STATE_PLAYER_RIGHT, 16
    ret z
    cp BLOCK_UNBREAKABLE
    jr nz, .digRight
.noRight

    xor a
    inc a
    ret

.digLeft
    xor a
    call ChangeBlock
    lb bc, STATE_PLAYER_LEFT, 16
    ret

.digRight
    xor a
    call ChangeBlock
    lb bc, STATE_PLAYER_RIGHT, 16
    ret

.digDown
    xor a
    call ChangeBlock
    ; Fallthrough

.beginPlayerFall
    lb bc, STATE_PLAYER_FALLING, 8
    xor a
    ret


.playerLeft
    ld hl, wPlayer_XPos
    dec [hl]
    ld b, STATE_PLAYER_STILL
    dec c
    ret

.playerRight
    ld hl, wPlayer_XPos
    inc [hl]
    ld b, STATE_PLAYER_STILL
    dec c
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


.rockShaking
    ld b, STATE_ROCK_FALLING
    dec c
    ret

.rockFalling
    ; TODO: check for collision
    ret

.debrisShattering
    dec c
    ret nz
    ; TODO: destroy object
    ret

SECTION "Overworld gfx", ROM0

OverworldGfx:
INCBIN "res/overworld/overworld.chr.pb16"
