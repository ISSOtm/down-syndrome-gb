
SECTION "Title screen", ROM0

TitleScreen::
    rst wait_vblank

    call RandInt
    ldh a, [hPressedButtons]
    and PADF_START
    jr z, TitleScreen
    ret
