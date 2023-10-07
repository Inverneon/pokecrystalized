UpdateItemIconAndDescription::
	farcall UpdateItemDescription
	jr _UpdateItemIcon
UpdateTMHMIconAndDescription::
	ld a, [wMenuSelection]
	cp -1
	jr z, .cancel
	call LoadTMHMIcon
	jr .ok
.cancel
	ld a, [wCurItem]
	call LoadTMHMIcon
	; call ClearTMHMIcon
.ok
	farcall LoadTMHMIconPalette
	jp SetPalettes

UpdateItemIconAndDescriptionAndBagQuantity::
	farcall UpdateItemDescription
_UpdateItemIcon:
	ld hl, ItemIconPointers
	ld a, [wCurItem]
	cp NUM_ITEMS + 1
	jr c, .has_icon
	xor a
.has_icon
	call _LoadItemOrKeyItemIcon
	farcall LoadItemIconPalette
	call SetPalettes
	ret

UpdateKeyItemIconAndDescription::
	farcall UpdateItemDescription
_UpdateKeyItemIcon:
	ld hl, ItemIconPointers
	ld a, [wCurItem]
	dec a
	call _LoadItemOrKeyItemIcon
	farcall LoadItemIconPalette
	jp SetPalettes

LoadTMHMIconForOverworld::
	ld hl, TMHMIcon
	lb bc, BANK(TMHMIcon), 9
	push bc
	ld de, wDecompressScratch
	call DecompressRequest2bpp
	call WhiteOutDecompressedItemIconCorners
	pop bc	
	ld hl, vTiles2 tile $60
	ld de, wDecompressScratch
	call Request2bpp
	farcall LoadTMHMIconPalette
	call SetPalettes
	call PrintOverworldItemIcon
	call WaitBGMap
	ret

LoadItemIconForOverworld::
LoadApricornIconForOverworld:
	ld a, d
	ld hl, ItemIconPointers
	call _SetupLoadItemOrKeyItemIcon
	push bc
	ld de, wDecompressScratch
	call DecompressRequest2bpp
	call WhiteOutDecompressedItemIconCorners
	pop bc
	ld hl, vTiles2 tile $60
	ld de, wDecompressScratch
	jp Request2bpp

_LoadItemOrKeyItemIcon:
	call _SetupLoadItemOrKeyItemIcon
	
	; ld b,b
	; ld hl, wStringBuffer5
	; ld de, wMonOrItemNameBuffer
	; ; CopyName2:: ; Copies the name from de to hl
	; call CopyName2

	push bc
	ld de, wDecompressScratch
	call DecompressRequest2bpp
	call WhiteOutDecompressedItemIconCorners
	pop bc
	ld hl, vTiles2 tile $60
	ld de, wDecompressScratch
	call Request2bpp

	; ld de, wStringBuffer5
	; ld hl, wMonOrItemNameBuffer
	; ; CopyName2:: ; Copies the name from de to hl
	; call CopyName2

	ret

_SetupLoadItemOrKeyItemIcon:
	ld c, a
	ld b, 0
	add hl, bc
	add hl, bc
	add hl, bc
	ld a, [hli]
	ld b, a
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld c, 9
	ret

LoadTMHMIcon::
	ld hl, TMHMIcon
	lb bc, BANK(TMHMIcon), 9
	push bc
	ld de, wDecompressScratch
	call DecompressRequest2bpp
	call WhiteOutDecompressedItemIconCorners
	pop bc	
	ld hl, vTiles2 tile $60
	ld de, wDecompressScratch
	jp Request2bpp

ClearKeyItemIcon::
ClearTMHMIcon::
	ld hl, NoItemIcon
	lb bc, BANK(NoItemIcon), 9
	ld de, vTiles2 tile $60
	jp DecompressRequest2bpp

WhiteOutDecompressedItemIconCorners:
	lb bc, %01111111, %11111110
	ld hl, wDecompressScratch tile 0
	ld a, [hl]
	and b
	ld [hli], a
	ld a, [hl]
	and b
	ld [hl], a
	ld hl, wDecompressScratch tile 2
	ld a, [hl]
	and c
	ld [hli], a
	ld a, [hl]
	and c
	ld [hl], a
	ld hl, wDecompressScratch tile 6 + 7 * 2
	ld a, [hl]
	and b
	ld [hli], a
	ld a, [hl]
	and b
	ld [hl], a
	ld hl, wDecompressScratch tile 8 + 7 * 2
	ld a, [hl]
	and c
	ld [hli], a
	ld a, [hl]
	and c
	ld [hl], a
	ret

PrintOverworldItemIcon::
	call SetPalettes
	ld a, $60 ; Tileset Extended unused space
	hlcoord 16, 13
	ld [hli], a
	inc a
	ld [hli], a
	inc a
	ld [hli], a
	inc a
	hlcoord 16, 14
	ld [hli], a
	inc a
	ld [hli], a
	inc a
	ld [hli], a
	inc a
	hlcoord 16, 15
	ld [hli], a
	inc a
	ld [hli], a
	inc a
	ld [hl], a
	ret

INCLUDE "data/items/icon_pointers.asm"
