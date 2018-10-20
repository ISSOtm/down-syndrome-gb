
SECTION "Overworld", ROM0

Overworld::


OverworldLoop:
    rst wait_vblank
    ld a, [wPlayerYPos]
    ld [wShadowOAM], a
    ld a, [wPlayerXPos]
    ld [wShadowOAM+1], a
    jr OverworldLoop
