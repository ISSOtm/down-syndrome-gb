
SECTION "Error handler memory", WRAM0

; ID of the error that occurred
wErrorType::
; Once the ID has been used, this is re-used as a status, to route calls because stack space is available
wErrorDrawStatus::
; The status is also used to determine which dump to print
wErrorWhichDump::
    db

wErrorRegs::
; Value of A when the handler is called
; Re-used as part of the reg dump
wErrorA::
; Re-used to hold last frame's keys
wErrorHeldButtons::
    db ; a
; Re-used to hold the number of frames till the debugger is unlocked
wErrorFramesTillUnlock::
    db ; f
    dw ; bc
    dw ; de
wErrorHL::
    dw
wErrorSP::
    dw


SECTION "Shadow OAM", WRAM0,ALIGN[8]

wShadowOAM::
    ds $A0


SECTION "Overworld memory", WRAM0,ALIGN[8]

; Per-screen objects
SCREEN_ID = 0
REPT NB_SCREENS_PER_ROW
OBJ_ID = 0
    REPT 32
OBJ_NAME equs STRCAT(STRSUB("{SCREEN_ID}", 2, STRLEN("{SCREEN_ID}") + 1), STRCAT("Object", STRSUB("{OBJ_ID}", 2, STRLEN("{OBJ_ID}") + 1)))
        dstruct Object, wScreen{OBJ_NAME}
        PURGE OBJ_NAME
OBJ_ID = OBJ_ID+1
    ENDR
SCREEN_ID = SCREEN_ID+1
ENDR
PURGE OBJ_ID
PURGE SCREEN_ID


wGeneratedScreens::
    ds NB_SCREENS_PER_ROW

SCREEN_ID = 0
REPT NB_SCREENS_PER_ROW
SCREEN_NAME equs STRCAT("wScreen", STRCAT(STRSUB("{SCREEN_ID}", 2, STRLEN("{SCREEN_ID}") - 1), "Map"))
SCREEN_NAME::
    ds (SCRN_X_B / 2 - 1) * (SCRN_Y_B / 2)
    PURGE SCREEN_NAME
SCREEN_ID = SCREEN_ID + 1
ENDR
PURGE SCREEN_ID

; Player object (always on-screen)
    dstruct Object, wPlayer

wPlayerDirection::
    db
; Player screen position
wPlayerDepth::
    db
wPlayerXScreen::
    db
wHavensEncountered::
    db

wDigDelay::
    db

wEnergy::
    db


wScore::
    ds 6 / 2
wNewScore::
    ds 6 / 2


; Which tilemap is currently on-screen
wWhichTilemap::
    db

; (Temporary) Pointer to the current screen map
wWhichScreenMap::
    dw

; Which tile is supposed to be present out-of-bounds
; Should be either BLOCK_DUG_DIRT or BLOCK_UNBREAKABLE
wOOBTile::
    db


; Buffer inside which coordinates are passed
wCoordY::
    db
wCoordX::
    db


; Frame counter for screen transitions
wTransitionCounter::
    db

wGameState::
    db


wAnimationFrameCounter::
    db


SECTION "Stack", WRAM0[$E000 - STACK_SIZE]

wStackTop::
    ds STACK_SIZE
wStackBottom::
