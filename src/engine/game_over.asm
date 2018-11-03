
SECTION "Game over", ROM0

GameOver::
    xor a
    ldh [hLCDC], a
    rst wait_vblank
    ld de, ErrorFont
    ld hl, $9000
    ld bc, $800
    call Memcpy
    xor a
    ld bc, $400
    rst memset
    ld de, GameOverStr
    ld hl, $9864
    call Strcpy
    ld hl, $98C2
    call Strcpy
    ld c, 3
    ld de, wScore
.printScore
    ld a, [de]
    and $F0
    swap a
    add a, "0"
    ld [hli], a
    ld a, [de]
    and $0F
    add a, "0"
    ld [hli], a
    inc de
    dec c
    jr nz, .printScore
    ld a, LCDCF_ON | LCDCF_WINOFF | LCDCF_BG8800 | LCDCF_BG9800 | LCDCF_OBJOFF | LCDCF_BGON
    ldh [rLCDC], a
    ldh [hLCDC], a
.loop
    rst wait_vblank
    ldh a, [hPressedButtons]
    and PADF_START
    jr z, .loop
    jp TitleScreen


GameOverStr:
    dstr "GAME OVER!"
    dstr "Score: "
