INCLUDE "data/mon_menu.asm"

	const_def
	const OW_MOVE_FLASH ; 0
	const OW_MOVE_SWEET_SCENT ; 1
	const OW_MOVE_DIG ; 2
	const OW_MOVE_FLY ; 3
	const OW_MOVE_TELEPORT ; 4
	const OW_MOVE_SOFTBOILED ; 5
	const OW_MOVE_MILK_DRINK ; 6
DEF NUM_MON_SUBMENU_MOVES EQU const_value ; 7 
; DEF NO_PARAM_CHECK EQU -1
DEF MON_SUBMENU_CONSUMES_PP EQU 0
MACRO ow_submenu_move
	db \1 ; actual move ID, cannot be -1/NO_PARAM_CHECK
	db \2 ; MUST BE LEARNED and consumes PP, ie. for moves like Softboiled, NO_PARAM_CHECK otherwise (Softboiled, Milk Drink, *Heal Bell, *Recover, **Aromatherapy)
	dw \3 ; engine_badge value; or -1 or NO_PARAM_CHECK for no badge check
	db \4 ; item ID for TM/HM to check for presence in bag, -1 or NO_PARAM_CHECK for no TM/HM in bag check
	assert \1 != NO_PARAM_CHECK, "A Move ID must be supplied"
ENDM

InteractableOverworldSubmenuMoves:
	table_width 5, InteractableOverworldSubmenuMoves ; width is 4 because 1 byte for move ID, 2 for engine_badge state, 1 for item/TM/HM ID
	ow_submenu_move FLASH, 			NO_PARAM_CHECK, ENGINE_ZEPHYRBADGE, HM_FLASH
	ow_submenu_move SWEET_SCENT, 	NO_PARAM_CHECK, NO_PARAM_CHECK, 	TM_SWEET_SCENT
	; ow_submenu_move DIG, 			NO_PARAM_CHECK, NO_PARAM_CHECK, 	TM_DIG
	; ow_submenu_move FLY, 			NO_PARAM_CHECK, ENGINE_STORMBADGE, 	HM_FLY
	ow_submenu_move DIG, 			NO_PARAM_CHECK, NO_PARAM_CHECK, 	NO_PARAM_CHECK
	ow_submenu_move FLY, 			NO_PARAM_CHECK, NO_PARAM_CHECK, 	NO_PARAM_CHECK	
	ow_submenu_move TELEPORT, 		NO_PARAM_CHECK, NO_PARAM_CHECK, 	NO_PARAM_CHECK
	ow_submenu_move SOFTBOILED, 	MON_SUBMENU_CONSUMES_PP, 	NO_PARAM_CHECK, 	NO_PARAM_CHECK
	ow_submenu_move MILK_DRINK, 	MON_SUBMENU_CONSUMES_PP, 	NO_PARAM_CHECK, 	NO_PARAM_CHECK
	assert_table_length NUM_MON_SUBMENU_MOVES

TryOW_MonMenu:
	; given index in 'a'
	ld c, 5 ; index * 5 since each entry is 4 bytes
	call SimpleMultiply ; Return a * c.
	ld hl, InteractableOverworldSubmenuMoves
	ld d, 0
	ld e, a
	add hl, de ; hl is now pointing to ow_move \1, the move ID
	ld a, [hli]
	ld [wPutativeTMHMMove], a ; move ID will be safe here and used later	

	ld a, [hli] ; consume_PP check
	ld [wTempByteValue], a

	ld e, [hl] ; lower half of ow_move \2, engine_badge
	inc hl
	ld d, [hl] ; upper half of ow_move \2, engine_badge
	inc hl     ; hl now pointing to ow_move \3, the TM/HM item if any
	ld a, NO_PARAM_CHECK ; -1
	cp e
	jr nz, .badge_test
	cp d
	jr z, .badge_passed	; no badge req
.badge_test
	push hl
	; call CheckEngineFlag
	; Check engine flag de
	ld b, CHECK_FLAG
	farcall EngineFlagAction
	ld a, c
	and a
	; jr nz, .isset
	jr z, .badge_fail
	pop hl
.badge_passed
	ld a, [hl] ; TM/HM associated
	cp NO_PARAM_CHECK ; -1 ; to see if we should skip this check
	jr z, .check_knownmoves
	ld [wCurItem], a
	ld hl, wNumItems
	call CheckItem
	jr z, .fail

.check_knownmoves	
	ld a, MON_MOVES
	call GetPartyParamLocation
	ld a, [wPutativeTMHMMove]
	ld b, a
	ld c, NUM_MOVES
.checkknownloop
	ld a, [hli]
	cp b
	jr z, .sucess
	dec c
	jr nz, .checkknownloop

; check if we're a PP consuming move, if so, quit because we dont currently have the move learned
; wTempByteValue/wNamedObjectIndex is going to be overwritten so we have to do this now
	ld a, [wTempByteValue]
	cp MON_SUBMENU_CONSUMES_PP
	jr z, .fail

.check_learnset
	; level up, doesnt matter what current lvl you are
	predef CanLearnViaLvlUp
	ld a, [wNamedObjectIndex]
	ld b, a
	ld a, [wPutativeTMHMMove]
  	cp b
	jr z, .sucess
	; egg moves
	predef CanLearnViaEggMove
	ld a, [wNamedObjectIndex]
	ld b, a
	ld a, [wPutativeTMHMMove]
  	cp b
	jr z, .sucess
	; TM/HM/Move Tutor
	predef CanLearnTMHMMove ; returns result in c, 0 if cannot learn
	ld a, c
	and a
	jr z, .fail

.sucess
	xor a
	ret
.fail
	ld a, -1
	ret
.badge_fail
	pop hl
	jr .fail

Iterate_OW_Submenu_Moves:
	; this reflects the order that the moves will appear stacked on each other if multiple are added
	call CanUseFlash
	call CanUseFly
	call CanUseDig
	call Can_Use_Sweet_Scent
	call CanUseTeleport
	call CanUseSoftboiled
	call CanUseMilkdrink
	ret

CanUseFlash:
; Location Check
	farcall SpecialAerodactylChamber
	jr c, .valid_location ; can use flash
	ld a, [wTimeOfDayPalset]
	cp DARKNESS_PALSET
	ret nz ; .fail ; not a darkcave

.valid_location
	ld a, OW_MOVE_FLASH
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add
; found
	ld a, MONMENUITEM_FLASH
	call AddMonMenuItem
	ret

Can_Use_Sweet_Scent:
	farcall CanUseSweetScent
	ret nc
	farcall GetMapEncounterRate
	ld a, b
	and a
	ret z

	ld a, OW_MOVE_SWEET_SCENT
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add

	ld a, MONMENUITEM_SWEETSCENT
	call AddMonMenuItem
	ret

CanUseDig:
	call GetMapEnvironment
	cp CAVE
	jr z, .valid_location
	cp DUNGEON
	ret nz ; fail, not inside cave or dungeon

.valid_location
	ld a, OW_MOVE_DIG
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add
; found
	ld a, MONMENUITEM_DIG
	call AddMonMenuItem
	ret

CanUseFly:
	call GetMapEnvironment
	call CheckOutdoorMap
	ret nz ; not outdoors, cant fly
	
	ld a, OW_MOVE_FLY
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add
; found
	ld a, MONMENUITEM_FLY
	call AddMonMenuItem
	ret

CanUseTeleport:
	call GetMapEnvironment
	call CheckOutdoorMap
	jr z, .valid_location
	
	call GetMapEnvironment
	cp CAVE
	jr z, .valid_location
	cp DUNGEON
	ret nz ; last valid location

.valid_location
	ld a, OW_MOVE_TELEPORT
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add

	ld a, MONMENUITEM_TELEPORT
	call AddMonMenuItem	
	ret

CanUseSoftboiled:
	ld a, OW_MOVE_SOFTBOILED
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add
	ld a, MONMENUITEM_SOFTBOILED
	call AddMonMenuItem
	ret

CanUseMilkdrink:
	ld a, OW_MOVE_MILK_DRINK
	call TryOW_MonMenu
	and a
	ret nz ; if not zero, do not add

	ld a, MONMENUITEM_MILKDRINK
	call AddMonMenuItem
	ret


MonSubmenu:
	xor a
	ldh [hBGMapMode], a
	call GetMonSubmenuItems
	farcall FreezeMonIcons
	ld hl, .MenuHeader
	call LoadMenuHeader
	call .GetTopCoord
	call PopulateMonMenu

	ld a, 1
	ldh [hBGMapMode], a
	call MonMenuLoop
	ld [wMenuSelection], a

	call ExitMenu
	ret

.MenuHeader:
	db MENU_BACKUP_TILES ; flags
	menu_coords 6, 0, SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1
	dw 0
	db 1 ; default option

.GetTopCoord:
; [wMenuBorderTopCoord] = 1 + [wMenuBorderBottomCoord] - 2 * ([wMonSubmenuCount] + 1)
	ld a, [wMonSubmenuCount]
	inc a
	add a
	ld b, a
	ld a, [wMenuBorderBottomCoord]
	sub b
	inc a
	ld [wMenuBorderTopCoord], a
	call MenuBox
	ret

MonMenuLoop:
.loop
	ld a, MENU_UNUSED_3 | MENU_BACKUP_TILES_2 ; flags
	ld [wMenuDataFlags], a
	ld a, [wMonSubmenuCount]
	ld [wMenuDataItems], a
	call InitVerticalMenuCursor
	ld hl, w2DMenuFlags1
	set 6, [hl]
	call StaticMenuJoypad
	ld de, SFX_READ_TEXT_2
	call PlaySFX
	ldh a, [hJoyPressed]
	bit A_BUTTON_F, a
	jr nz, .select
	bit B_BUTTON_F, a
	jr nz, .cancel
	jr .loop

.cancel
	ld a, MONMENUITEM_CANCEL
	ret

.select
	ld a, [wMenuCursorY]
	dec a
	ld c, a
	ld b, 0
	ld hl, wMonSubmenuItems
	add hl, bc
	ld a, [hl]
	ret

PopulateMonMenu:
	call MenuBoxCoord2Tile
	ld bc, 2 * SCREEN_WIDTH + 2
	add hl, bc
	ld de, wMonSubmenuItems
.loop
	ld a, [de]
	inc de
	cp -1
	ret z
	push de
	push hl
	call GetMonMenuString
	pop hl
	call PlaceString
	ld bc, 2 * SCREEN_WIDTH
	add hl, bc
	pop de
	jr .loop

GetMonMenuString:
	ld hl, MonMenuOptions + 1
	ld de, 3
	call IsInArray
	dec hl
	ld a, [hli]
	cp MONMENU_MENUOPTION
	jr z, .NotMove
	inc hl
	ld a, [hl]
	ld [wNamedObjectIndex], a
	call GetMoveName
	ret

.NotMove:
	inc hl
	ld a, [hl]
	dec a
	ld hl, MonMenuOptionStrings
	call GetNthString
	ld d, h
	ld e, l
	ret

GetMonSubmenuItems:
	call ResetMonSubmenu
	ld a, [wCurPartySpecies]
	cp EGG
	jr z, .egg
	ld a, [wLinkMode]
	and a
	jr nz, .skip_moves
	call Iterate_OW_Submenu_Moves

.skip_moves
	ld a, MONMENUITEM_STATS
	call AddMonMenuItem
	ld a, MONMENUITEM_SWITCH
	call AddMonMenuItem
	ld a, MONMENUITEM_MOVE
	call AddMonMenuItem
	ld a, [wLinkMode]
	and a
	jr nz, .skip2
	push hl
	ld a, MON_ITEM
	call GetPartyParamLocation
	ld d, [hl]
	farcall ItemIsMail
	pop hl
	ld a, MONMENUITEM_MAIL
	jr c, .ok
	ld a, MONMENUITEM_ITEM

.ok
	call AddMonMenuItem

.skip2
	ld a, [wMonSubmenuCount]
	cp NUM_MONMENU_ITEMS
	jr z, .ok2
	ld a, MONMENUITEM_CANCEL
	call AddMonMenuItem

.ok2
	call TerminateMonSubmenu
	ret

.egg
	ld a, MONMENUITEM_STATS
	call AddMonMenuItem
	ld a, MONMENUITEM_SWITCH
	call AddMonMenuItem
	ld a, MONMENUITEM_CANCEL
	call AddMonMenuItem
	call TerminateMonSubmenu
	ret

IsFieldMove:
	ld b, a
	ld hl, MonMenuOptions
.next
	ld a, [hli]
	cp -1
	jr z, .nope
	cp MONMENU_MENUOPTION
	jr z, .nope
	ld d, [hl]
	inc hl
	ld a, [hli]
	cp b
	jr nz, .next
	ld a, d
	scf

.nope
	ret

ResetMonSubmenu:
	xor a
	ld [wMonSubmenuCount], a
	ld hl, wMonSubmenuItems
	ld bc, NUM_MONMENU_ITEMS + 1
	call ByteFill
	ret

TerminateMonSubmenu:
	ld a, [wMonSubmenuCount]
	ld e, a
	ld d, 0
	ld hl, wMonSubmenuItems
	add hl, de
	ld [hl], -1
	ret

AddMonMenuItem:
	push hl
	push de
	push af
	ld a, [wMonSubmenuCount]
	ld e, a
	inc a
	ld [wMonSubmenuCount], a
	ld d, 0
	ld hl, wMonSubmenuItems
	add hl, de
	pop af
	ld [hl], a
	pop de
	pop hl
	ret

BattleMonMenu:
	ld hl, .MenuHeader
	call CopyMenuHeader
	xor a
	ldh [hBGMapMode], a
	call MenuBox
	call UpdateSprites
	call PlaceVerticalMenuItems
	call WaitBGMap
	call CopyMenuData
	ld a, [wMenuDataFlags]
	bit 7, a
	jr z, .set_carry
	call InitVerticalMenuCursor
	ld hl, w2DMenuFlags1
	set 6, [hl]
	call StaticMenuJoypad
	ld de, SFX_READ_TEXT_2
	call PlaySFX
	ldh a, [hJoyPressed]
	bit B_BUTTON_F, a
	jr z, .clear_carry
	ret z

.set_carry
	scf
	ret

.clear_carry
	and a
	ret

.MenuHeader:
	db 0 ; flags
	menu_coords 11, 11, SCREEN_WIDTH - 1, SCREEN_HEIGHT - 1
	dw .MenuData
	db 1 ; default option

.MenuData:
	db STATICMENU_CURSOR | STATICMENU_NO_TOP_SPACING ; flags
	db 3 ; items
	db "SWITCH@"
	db "STATS@"
	db "CANCEL@"
