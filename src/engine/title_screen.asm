
SECTION "Title screen", ROM0

TitleScreen::
    ld a, $E4
    ldh [hBGP], a
    xor a
    ldh [hLCDC], a
    rst wait_vblank

    ld de, TitleScreenGfx
    ld hl, $8000
    ld b, 0
    call pb16_unpack_block

    ld de, TitleScreenMap
    ld hl, $9800
    ld b, SCRN_Y_B
.copyRow
    ld c, SCRN_X_B
    rst memcpy_small
    ld a, l
    add a, SCRN_VX_B - SCRN_X_B
    ld l, a
    jr nc, .noCarry
    inc h
.noCarry
    dec b
    jr nz, .copyRow

    ld a, LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON
    ld [rLCDC], a
    ldh [hLCDC], a
.loop
    rst wait_vblank

    call RandInt
    ldh a, [hPressedButtons]
    and PADF_START
    jr z, .loop
    ret


TitleScreenGfx:
INCBIN "res/title_screen/title_screen.chr.pb16"

TitleScreenMap:
INCBIN "res/title_screen/title_screen.tilemap"
