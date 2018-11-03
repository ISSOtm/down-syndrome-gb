
set_enum_value: MACRO
enum_value = \1
ENDM

enum_start: MACRO
    IF _NARG == 0
        set_enum_value 0
    ELSE
        set_enum_value \1
    ENDC
ENDM

enum_elem: MACRO
    IF _NARG >= 2
        set_enum_value \2
    ENDC

\1 = enum_value
    set_enum_value enum_value+1
ENDM


; SGB packet types
    enum_start
    enum_elem PAL01
    enum_elem PAL23
    enum_elem PAL12
    enum_elem PAL03
    enum_elem ATTR_BLK
    enum_elem ATTR_LIN
    enum_elem ATTR_DIV
    enum_elem ATTR_CHR
    enum_elem SOUND    ; $08
    enum_elem SOU_TRN
    enum_elem PAL_SET
    enum_elem PAL_TRN
    enum_elem ATRC_EN
    enum_elem TEST_EN
    enum_elem ICON_EN
    enum_elem DATA_SND
    enum_elem DATA_TRN ; $10
    enum_elem MLT_REQ
    enum_elem JUMP
    enum_elem CHR_TRN
    enum_elem PCT_TRN
    enum_elem ATTR_TRN
    enum_elem ATTR_SET
    enum_elem MASK_EN
    enum_elem OBJ_TRN  ; $18


; Error IDs
    enum_start
    enum_elem ERROR_JUMP_HL
    enum_elem ERROR_JUMP_DE
    enum_elem ERROR_NULL_EXEC
    enum_elem ERROR_RST38
    enum_elem ERROR_UNKNOWN


; Directions
    enum_start
    enum_elem DIR_UP
    enum_elem DIR_DOWN
    enum_elem DIR_LEFT
    enum_elem DIR_RIGHT


; Game states
    enum_start
    enum_elem GAME_STATE_NORMAL
    enum_elem GAME_STATE_DOWN_TRANSITION
    enum_elem GAME_STATE_LEFT_TRANSITION
    enum_elem GAME_STATE_RIGHT_TRANSITION


; Block types
    enum_start
    enum_elem BLOCK_DUG_DIRT
    enum_elem BLOCK_GRASS
    enum_elem BLOCK_GRASS_HIT
    enum_elem BLOCK_DIRT
    enum_elem BLOCK_DIRT_HIT1
    enum_elem BLOCK_DIRT_HIT2
    enum_elem BLOCK_ROCK
    enum_elem BLOCK_ROCK_CRACKLED
    enum_elem BLOCK_ROCK_DAMAGED
    ; enum_elem BLOCK_HEAVY_ROCK
    enum_elem BLOCK_UNBREAKABLE


; Object states
    enum_start
    enum_elem STATE_PLAYER_STILL
    enum_elem STATE_PLAYER_LEFT
    enum_elem STATE_PLAYER_RIGHT
    enum_elem STATE_PLAYER_FALLING
    enum_elem STATE_PLAYER_SQUISH

    enum_elem STATE_ROCK_WATCHING
    enum_elem STATE_ROCK1_SHAKING
    enum_elem STATE_ROCK2_SHAKING
    enum_elem STATE_ROCK3_SHAKING
    enum_elem STATE_ROCK1_FALLING
    enum_elem STATE_ROCK2_FALLING
    enum_elem STATE_ROCK3_FALLING

    enum_elem STATE_ROCK_DEBRIS_SHATTERING
    enum_elem STATE_DEBRIS_SHATTERING

    enum_elem STATE_CARROT
    enum_elem STATE_GOLDEN_POTATO

    enum_elem STATE_DIAMOND
    enum_elem STATE_EMERALD
    enum_elem STATE_CRYSTAL
