

; Since all these functions are independent, declare each of them in individual sections
; Lets the linker place them more liberally. Hooray!
f: MACRO
PURGE \1
SECTION "Utility function \1", ROM0
\1::
ENDM

SECTION "Dummy section", ROM0 ; To have the first `Memcpy` declare properly

; Copies bc bytes of data from de to hl
Memcpy::
 f Memcpy
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, Memcpy
    ret

; Copies a null-terminated string from de to hl, including the terminating NUL
Strcpy::
 f Strcpy
    ld a, [de]
    ld [hli], a
    inc de
    and a
    jr nz, Strcpy
    ret

; Copies c bytes of data from de to hl in a LCD-safe manner
LCDMemcpySmall::
 f LCDMemcpySmall
    ldh a, [rSTAT]
    and STATF_BUSY
    jr nz, LCDMemcpySmall
    ld a, [de]
    ld [hli], a
    inc de
    dec c
    jr nz, LCDMemcpySmall
    ret

; Copies bc bytes of data from de to hl in a LCD-safe manner
LCDMemcpy::
 f LCDMemcpy
    ldh a, [rSTAT]
    and STATF_BUSY
    jr nz, LCDMemcpy
    ld a, [de]
    ld [hli], a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, LCDMemcpy
    ret

; Sets c bytes of data at hl with the value in a
LCDMemsetSmall::
 f LCDMemsetSmall
    ld b, a

; Sets c bytes of data at hl with the value in b
LCDMemsetSmallFromB::
; No f (...) because there's the slide-in above
.loop
    ldh a, [rSTAT]
    and STATF_BUSY
    jr nz, .loop
    ld a, b
    ld [hli], a
    dec c
    jr nz, .loop
    ret

; Sets bc bytes of data at hl with the value in a
LCDMemset::
 f LCDMemset
    ld d, a

; Sets bc bytes of data at hl with the value in d
LCDMemsetFromD::
; No f (...) because of the slide-in above
.loop
    ldh a, [rSTAT]
    and STATF_BUSY
    jr nz, .loop
    ld a, d
    ld [hli], a
    dec bc
    ld a, b
    or c
    jr nz, .loop
    ret


; Opens SRAM at some bank
; @param a The bank's number
; @return a CART_RAM_ENABLE, ie. $0A
GetSRAMBank::
 f GetSRAMBank
    ld [rRAMB], a
    ld a, CART_RAM_ENABLE
    ld [rRAMG], a
    ret

; Closes SRAM
; @return hl = rRAMB (I know, it sounds stupid)
CloseSRAM::
 f CloseSRAM
; Implementation note: MUST preserve the Z flag to avoid breaking the call to `PrintSRAMFailure`
    ld hl, rRAMG
    ld [hl], l ; ld [hl], 0
    ld h, HIGH(rRAMB)
    ld [hl], l ; Avoid unintentional unlocks corrupting saved data, switch to bank 0 (which is scratch)
    ret


; Gets the Nth struct in an array of 'em
; @param hl Array base
; @param bc Size of a struct
; @param a  ID of the desired struct
; @return hl Pointer to the struct's base
; @destroys a
GetNthStruct::
 f GetNthStruct
    and a
    ret z
.next
    add hl, bc
    dec a
    jr nz, .next
    ret


; Copies tiles into VRAM, using an unrolled loop to go faster
; @param hl Destination (pointer)
; @param de Source
; @param c  Number of tiles
; @return hl, de Pointer to end of blocks
; @return c 0
; @destroys a
Tilecpy::
 f Tilecpy
REPT $10 / 2
    wait_vram
    ld a, [de]
    ld [hli], a
    inc de
    ld a, [de]
    ld [hli], a
    inc de
ENDR
    dec c
    jr nz, Tilecpy
    ret


; Copies a tilemap to VRAM, assuming the LCD is off
; @param hl Destination
; @param de Source
; @return hl, de Pointer to end of blocks
; @return bc Zero
; @return a Equal to h
Mapcpy::
 f Mapcpy
    ld b, SCRN_Y_B
.copyRow
    ld c, SCRN_X_B
.copyTile
    ld a, [de]
    ld [hli], a
    inc de
    dec c
    jr nz, .copyTile
    ld a, l
    add a, SCRN_VX_B - SCRN_X_B
    ld l, a
    adc a, h
    sub l
    ld h, a
    dec b
    jr nz, .copyRow
    ret


; Generates a random number
; Shamelessly stolen from SM64
; It's a very neat bijection, and I'm not good enough to produce such a nice function
; I didn't implement the weird re-looping they did, though
; @returns hl The number generated
; @returns a  The high byte of that number
; @destroy bc
RandInt::
 f RandInt
	ldh a, [hRandIntLow]
	ld c, a
	cp $0A
	ldh a, [hRandIntHigh]
	jr nz, .dontChangeCycles
	cp $56
	jr nz, .dontChangeCycles
	xor a
	ld c, a
.dontChangeCycles
	xor c
	ld h, a
	ld l, c
	ld b, l
	ld c, h
	add hl, hl
	ld a, l
	xor c
	ld l, a
	ld a, h
	and $01 ; Remove pre-shift upper byte
	xor b ; Resets carry
	rra
	cpl
	ld h, a
	ld a, l
	rra
	ld bc, $1F74
	jr nc, .useThatConstant
	ld bc, $8100
.useThatConstant
	xor c
	ld l, a
	ldh [hRandIntLow], a
	ld a, h
	xor b
	ld h, a
	ldh [hRandIntHigh], a
	ret


PURGE f
