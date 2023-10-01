	const_def
	const PINK_PAGE   ; 0
	const GREEN_PAGE  ; 1
	const BLUE_PAGE   ; 2
	const ORANGE_PAGE ; 3
NUM_STAT_PAGES EQU const_value

DEF STAT_PAGE_MASK EQU %00000011

BattleStatsScreenInit:
	ld a, [wLinkMode]
	cp LINK_MOBILE
	jr nz, StatsScreenInit

	ld a, [wBattleMode]
	and a
	jr z, StatsScreenInit
	jr _MobileStatsScreenInit

StatsScreenInit:
	ld hl, StatsScreenMain
	jr StatsScreenInit_gotaddress

_MobileStatsScreenInit:
	ld hl, StatsScreenMobile
	jr StatsScreenInit_gotaddress

StatsScreenInit_gotaddress:
	ldh a, [hMapAnims]
	push af
	xor a
	ldh [hMapAnims], a ; disable overworld tile animations
	ld a, [wBoxAlignment] ; whether sprite is to be mirrorred
	push af
	ld a, [wJumptableIndex]
	ld b, a
	ld a, [wStatsScreenFlags]
	ld c, a

	push bc
	push hl
	call ClearBGPalettes
	call ClearTilemap
	call UpdateSprites
	farcall StatsScreen_LoadFont
	pop hl
	call _hl_
	call ClearBGPalettes
	call ClearTilemap
	pop bc

	; restore old values
	ld a, b
	ld [wJumptableIndex], a
	ld a, c
	ld [wStatsScreenFlags], a
	pop af
	ld [wBoxAlignment], a
	pop af
	ldh [hMapAnims], a
	ret

StatsScreenMain:
	xor a
	ld [wJumptableIndex], a
	ld [wStatsScreenFlags], a ; PINK_PAGE
.loop
	ld a, [wJumptableIndex]
	and ~(1 << 7)
	ld hl, StatsScreenPointerTable
	rst JumpTable
	call StatsScreen_WaitAnim
	ld a, [wJumptableIndex]
	bit 7, a
	jr z, .loop
	ret

StatsScreenMobile:
	ret

StatsScreenPointerTable:
	dw MonStatsInit       ; regular pokémon
	dw EggStatsInit       ; egg
	dw StatsScreenWaitCry
	dw EggStatsJoypad
	dw StatsScreen_LoadPage
	dw StatsScreenWaitCry
	dw MonStatsJoypad
	dw StatsScreen_Exit

StatsScreen_WaitAnim:
	ld hl, wStatsScreenFlags
	bit 6, [hl]
	jr nz, .try_anim
	bit 5, [hl]
	jr nz, .finish
	call DelayFrame
	ret

.try_anim
	farcall SetUpPokeAnim
	jr nc, .finish
	ld hl, wStatsScreenFlags
	res 6, [hl]
.finish
	ld hl, wStatsScreenFlags
	res 5, [hl]
	farcall HDMATransferTilemapToWRAMBank3
	ret

StatsScreen_SetJumptableIndex:
	ld a, [wJumptableIndex]
	and $80
	or h
	ld [wJumptableIndex], a
	ret

StatsScreen_Exit:
	ld hl, wJumptableIndex
	set 7, [hl]
	ret

MonStatsInit:
	ld hl, wStatsScreenFlags
	res 6, [hl]
	call ClearBGPalettes
	call ClearTilemap
	farcall HDMATransferTilemapToWRAMBank3
	call StatsScreen_CopyToTempMon
	ld a, [wCurPartySpecies]
	cp EGG
	jr z, .egg
	call StatsScreen_InitUpperHalf
	ld hl, wStatsScreenFlags
	set 4, [hl]
	ld h, 4
	call StatsScreen_SetJumptableIndex
	ret

.egg
	ld h, 1
	call StatsScreen_SetJumptableIndex
	ret

EggStatsInit:
	call EggStatsScreen
	ld a, [wJumptableIndex]
	inc a
	ld [wJumptableIndex], a
	ret

EggStatsJoypad:
	call StatsScreen_GetJoypad
	jr nc, .check
	ld h, 0
	call StatsScreen_SetJumptableIndex
	ret

.check
	bit A_BUTTON_F, a
	jr nz, .quit
if DEF(_DEBUG)
	cp START
	jr z, .hatch
endc
	and D_DOWN | D_UP | A_BUTTON | B_BUTTON
	jp StatsScreen_JoypadAction

.quit
	ld h, 7
	call StatsScreen_SetJumptableIndex
	ret

if DEF(_DEBUG)
.hatch
	ld a, [wMonType]
	or a
	jr nz, .skip
	push bc
	push de
	push hl
	ld a, [wCurPartyMon]
	ld bc, PARTYMON_STRUCT_LENGTH
	ld hl, wPartyMon1Happiness
	call AddNTimes
	ld [hl], 1
	ld a, 1
	ld [wTempMonHappiness], a
	ld a, 127
	ld [wStepCount], a
	ld de, .HatchSoonString
	hlcoord 8, 17
	call PlaceString
	ld hl, wStatsScreenFlags
	set 5, [hl]
	pop hl
	pop de
	pop bc
.skip
	xor a
	jp StatsScreen_JoypadAction

.HatchSoonString:
	db "▶HATCH SOON!@"
endc

StatsScreen_LoadPage:
	call StatsScreen_LoadGFX
	ld hl, wStatsScreenFlags
	res 4, [hl]
	ld a, [wJumptableIndex]
	inc a
	ld [wJumptableIndex], a
	ret

MonStatsJoypad:
	call StatsScreen_GetJoypad
	jr nc, .next
	ld h, 0
	call StatsScreen_SetJumptableIndex
	ret

.next
	and D_DOWN | D_UP | D_LEFT | D_RIGHT | A_BUTTON | B_BUTTON
	jp StatsScreen_JoypadAction

StatsScreenWaitCry:
	call IsSFXPlaying
	ret nc
	ld a, [wJumptableIndex]
	inc a
	ld [wJumptableIndex], a
	ret

StatsScreen_CopyToTempMon:
	ld a, [wMonType]
	cp TEMPMON
	jr nz, .not_tempmon
	ld a, [wBufferMonSpecies]
	ld [wCurSpecies], a
	call GetBaseData
	ld hl, wBufferMon
	ld de, wTempMon
	ld bc, PARTYMON_STRUCT_LENGTH
	call CopyBytes
	jr .done

.not_tempmon
	farcall CopyMonToTempMon
	ld a, [wCurPartySpecies]
	cp EGG
	jr z, .done
	ld a, [wMonType]
	cp BOXMON
	jr c, .done
	farcall CalcTempmonStats
.done
	and a
	ret

StatsScreen_GetJoypad:
	call GetJoypad
	ld a, [wMonType]
	cp TEMPMON
	jr nz, .not_tempmon
	push hl
	push de
	push bc
	farcall StatsScreenDPad
	pop bc
	pop de
	pop hl
	ld a, [wMenuJoypad]
	and D_DOWN | D_UP
	jr nz, .set_carry
	ld a, [wMenuJoypad]
	jr .clear_carry

.not_tempmon
	ldh a, [hJoyPressed]
.clear_carry
	and a
	ret

.set_carry
	scf
	ret

StatsScreen_JoypadAction:
	push af
	ld a, [wStatsScreenFlags]
	maskbits NUM_STAT_PAGES
	ld c, a
	pop af
	bit B_BUTTON_F, a
	jp nz, .b_button
	bit D_LEFT_F, a
	jr nz, .d_left
	bit D_RIGHT_F, a
	jr nz, .d_right
	bit A_BUTTON_F, a
	jr nz, .a_button
	bit D_UP_F, a
	jr nz, .d_up
	bit D_DOWN_F, a
	jr nz, .d_down
	jr .done

.d_down
	ld a, [wMonType]
	cp BOXMON
	jr nc, .done
	and a
	ld a, [wPartyCount]
	jr z, .next_mon
	ld a, [wOTPartyCount]
.next_mon
	ld b, a
	ld a, [wCurPartyMon]
	inc a
	cp b
	jr z, .done
	ld [wCurPartyMon], a
	ld b, a
	ld a, [wMonType]
	and a
	jr nz, .load_mon
	ld a, b
	inc a
	ld [wPartyMenuCursor], a
	jr .load_mon

.d_up
	ld a, [wCurPartyMon]
	and a
	jr z, .done
	dec a
	ld [wCurPartyMon], a
	ld b, a
	ld a, [wMonType]
	and a
	jr nz, .load_mon
	ld a, b
	inc a
	ld [wPartyMenuCursor], a
	jr .load_mon

.a_button
	ld a, c
	cp ORANGE_PAGE ; last page
	jr z, .b_button
.d_right
	inc c
	ld a, ORANGE_PAGE ; last page
	cp c
	jr nc, .set_page
	ld c, PINK_PAGE ; first page
	jr .set_page

.d_left
	ld a, c
	dec c
	and a ; cp PINK_PAGE ; first page
	jr nz, .set_page
	ld c, ORANGE_PAGE ; last page
	jr .set_page

.done
	ret

.set_page
	ld a, [wStatsScreenFlags]
	and ~STAT_PAGE_MASK
	or c
	ld [wStatsScreenFlags], a
	ld h, 4
	call StatsScreen_SetJumptableIndex
	ret

.load_mon
	ld h, 0
	call StatsScreen_SetJumptableIndex
	ret

.b_button
	ld h, 7
	call StatsScreen_SetJumptableIndex
	ret

StatsScreen_InitUpperHalf:
	call .PlaceHPBar
	xor a
	ldh [hBGMapMode], a
	ld a, [wBaseDexNo]
	ld [wTextDecimalByte], a
	ld [wCurSpecies], a
	hlcoord 8, 0
	ld [hl], "№"
	inc hl
	ld [hl], "."
	inc hl
	hlcoord 10, 0
	lb bc, PRINTNUM_LEADINGZEROS | 1, 3
	ld de, wTextDecimalByte
	call PrintNum
	hlcoord 14, 0
	call PrintLevel
	ld hl, .NicknamePointers
	call GetNicknamePointer
	call CopyNickname
	hlcoord 8, 2
	call PlaceString
	hlcoord 18, 0
	call .PlaceGenderChar
	hlcoord 9, 4
	ld a, "/"
	ld [hli], a
	ld a, [wBaseDexNo]
	ld [wNamedObjectIndex], a
	call GetPokemonName
	call PlaceString
	call StatsScreen_PlaceHorizontalDivider
	call StatsScreen_PlacePageSwitchArrows
	call StatsScreen_PlaceShinyIcon
	call StatsScreen_placeCaughtBall
	ret

.PlaceHPBar:
	ld hl, wTempMonHP
	ld a, [hli]
	ld b, a
	ld c, [hl]
	ld hl, wTempMonMaxHP
	ld a, [hli]
	ld d, a
	ld e, [hl]
	farcall ComputeHPBarPixels
	ld hl, wCurHPPal
	call SetHPPal
	ld b, SCGB_STATS_SCREEN_HP_PALS
	call GetSGBLayout
	call DelayFrame
	ret

.PlaceGenderChar:
	push hl
	farcall GetGender
	pop hl
	ret c
	ld a, "♂"
	jr nz, .got_gender
	ld a, "♀"
.got_gender
	ld [hl], a
	ret

.NicknamePointers:
	dw wPartyMonNicknames
	dw wOTPartyMonNicknames
	dw sBoxMonNicknames
	dw wBufferMonNickname

StatsScreen_PlaceVerticalDivider: ; unreferenced
; The Japanese stats screen has a vertical divider.
	hlcoord 7, 0
	ld bc, SCREEN_WIDTH
	ld d, SCREEN_HEIGHT
.loop
	ld a, $31 ; vertical divider
	ld [hl], a
	add hl, bc
	dec d
	jr nz, .loop
	ret

StatsScreen_PlaceHorizontalDivider:
	hlcoord 0, 7
	ld b, SCREEN_WIDTH
	ld a, $62 ; horizontal divider (empty HP/exp bar)
.loop
	ld [hli], a
	dec b
	jr nz, .loop
	ret

StatsScreen_PlacePageSwitchArrows:
	hlcoord 10, 6
	ld [hl], "◀"
	hlcoord 19, 6
	ld [hl], "▶"
	ret

StatsScreen_PlaceShinyIcon:
	ld bc, wTempMonDVs
	farcall CheckShininess
	ret nc
	hlcoord 19, 0
	ld [hl], "⁂"
	ret

StatsScreen_LoadGFX:
	ld a, [wBaseDexNo]
	ld [wTempSpecies], a
	ld [wCurSpecies], a
	xor a
	ldh [hBGMapMode], a
	call .ClearBox
	call .PageTilemap
	call .LoadPals
	ld hl, wStatsScreenFlags
	bit 4, [hl]
	jr nz, .place_frontpic
	call SetPalettes
	ret

.place_frontpic
	call StatsScreen_PlaceFrontpic
	ret

.ClearBox:
	ld a, [wStatsScreenFlags]
	maskbits NUM_STAT_PAGES
	ld c, a
	call StatsScreen_LoadPageIndicators
	hlcoord 0, 8
	lb bc, 10, 20
	call ClearBox
	ret

.LoadPals:
	ld a, [wStatsScreenFlags]
	maskbits NUM_STAT_PAGES
	ld c, a
	farcall LoadStatsScreenPals
	call DelayFrame
	ld hl, wStatsScreenFlags
	set 5, [hl]
	ret

.PageTilemap:
	ld a, [wStatsScreenFlags]
	maskbits NUM_STAT_PAGES
	ld hl, .Jumptable
	rst JumpTable
	ret

.Jumptable:
; entries correspond to *_PAGE constants
	table_width 2, StatsScreen_LoadGFX.Jumptable
	dw LoadPinkPage
	dw LoadGreenPage
	dw LoadBluePage
	dw LoadOrangePage
	assert_table_length NUM_STAT_PAGES

LoadPinkPage:
	hlcoord 0, 8
	ld b, $0
	predef DrawPlayerHP
	hlcoord 8, 8
	ld [hl], $41 ; right HP/exp bar end cap
	ld de, .Status_Text ; string for "STATUS/"
	hlcoord 0, 11
	call PlaceString
	ld de, .Type_Text ; string for "TYPE/"
	hlcoord 0, 13
	call PlaceString
	call PrintMonTypeTiles ; custom GFX function

	ld a, [wTempMonPokerusStatus]
	ld b, a
	and $f
	jr nz, .HasPokerus
	ld a, b
	and $f0
	jr z, .NotImmuneToPkrs
	hlcoord 19, 1
	ld [hl], "." ; Pokérus immunity dot
.NotImmuneToPkrs:
	ld a, [wMonType]
	cp BOXMON
	jr z, .done_status
	ld de, wTempMonStatus
	predef GetStatusConditionIndex
	ld a, d
	and a
	jr z, .StatusOK

	; status index in a
	ld hl, StatusIconGFX
	ld bc, 2 * LEN_2BPP_TILE
	call AddNTimes
	ld d, h
	ld e, l
	ld hl, vTiles2 tile $50
	lb bc, BANK(StatusIconGFX), 2
	call Request2bpp

	hlcoord 7, 11
	ld a, $50 ; status tile first half
	ld [hli], a
	inc a ; status tile 2nd half
	ld [hl], a
	
	jr .done_status
.HasPokerus:
	ld de, .PkrsStr
	hlcoord 1, 12
	call PlaceString
	jr .NotImmuneToPkrs
.StatusOK:
	hlcoord 7, 11
	ld de, .OK_str
	call PlaceString
.done_status
; vertical divider
	hlcoord 9, 8
	ld de, SCREEN_WIDTH
	ld b, 7
	ld a, $31 ; vertical divider
.vertical_divider
	ld [hl], a
	add hl, de
	dec b
	jr nz, .vertical_divider
; new horizontal div
	ld a, $62
	hlcoord 0, 15
	ld bc, SCREEN_WIDTH
	call ByteFill
; EXP Points
	ld de, .ExpPointStr
	hlcoord 11, 10
	call PlaceString
	hlcoord 14, 13
	call .PrintNextLevel
	hlcoord 12, 11
	lb bc, 3, 7
	ld de, wTempMonExp
	ld a, [de]
	and EXP_MASK
	ld [wStringBuffer1], a
	ld a, [wTempMonExp + 1]
	ld [wStringBuffer1 + 1], a
	ld a, [wTempMonExp + 2]
	ld [wStringBuffer1 + 2], a
	ld de, wStringBuffer1
	call PrintNum
	call .CalcExpToNextLevel
	hlcoord 12, 14
	lb bc, 3, 7
	ld de, wExpToNextLevel
	call PrintNum
	ld de, .ToStr ; .LevelUpStr
	hlcoord 11, 13
	call PlaceString
	hlcoord 11, 8
	ld a, [wTempMonLevel]
	ld b, a
	ld de, wTempMonExp + 2
	predef FillInExpBar
	hlcoord 10, 8
	ld [hl], $40 ; left exp bar end cap
	hlcoord 19, 8
	ld [hl], $41 ; right exp bar end cap

	; this messed up something w/ Type and EXP printing. but not if called here
	call StatsScreen_PlaceItemName
	ret

.PrintNextLevel:
	ld a, [wTempMonLevel]
	push af
	cp MAX_LEVEL
	jr z, .AtMaxLevel
	inc a
	ld [wTempMonLevel], a
.AtMaxLevel:
	call PrintLevel
	pop af
	ld [wTempMonLevel], a
	ret

.CalcExpToNextLevel:
	ld a, [wTempMonLevel]
	cp MAX_LEVEL
	jr z, .AlreadyAtMaxLevel
	inc a
	ld d, a
	farcall CalcExpAtLevel
	ld hl, wTempMonExp + 2
	ldh a, [hQuotient + 3]
	sub [hl]
	dec hl
	ld [wExpToNextLevel + 2], a
	ldh a, [hQuotient + 2]
	sbc [hl]
	dec hl
	ld [wExpToNextLevel + 1], a
	ldh a, [hQuotient + 1]
	push af
	ld e, a
	ld a, [hl]
	and EXP_MASK
	ld d, a
	pop af
	ld a, e
	sbc d
	ld [wExpToNextLevel], a
	ret

.AlreadyAtMaxLevel:
	ld hl, wExpToNextLevel
	xor a
	ld [hli], a
	ld [hli], a
	ld [hl], a
	ret

.Status_Text:
	db   "STATUS/@"
.Type_Text:
	db   "TYPE/@"

.OK_str:
	db "OK @"

.ExpPointStr:
	db "EXP PTS@"

.LevelUpStr:
	db "LVL-UP@"

.ToStr:
	db "TO@"

.PkrsStr:
	db "#RUS@"

LoadGreenPage:
	call StatsScreen_PlaceOTInfo

	ld hl, wTempMonMoves
	ld de, wListMoves_MoveIndicesBuffer
	ld bc, NUM_MOVES
	call CopyBytes
	; hlcoord 1, 8
	; ld a, SCREEN_WIDTH * 1
	hlcoord 1, 8
	ld a, SCREEN_WIDTH * 2
	ld [wListMovesLineSpacing], a
	predef ListMoves
	; hlcoord 13, 8
	; ld a, SCREEN_WIDTH * 1	
	hlcoord 12, 9
	ld a, SCREEN_WIDTH * 2

	ld [wListMovesLineSpacing], a
	predef ListMovePP
	ret
LoadBluePage:
	hlcoord 6, 8
	ld de, .Stats_text
	call PlaceString

	hlcoord 11, 8
	ld de, SCREEN_WIDTH
	ld b, 10
	ld a, $31 ; vertical divider
.vertical_divider
	ld [hl], a
	add hl, de
	dec b
	jr nz, .vertical_divider
	hlcoord 0, 10
	ld bc, 8 ; number of spaces after above hlcoord to print the numbers
	call StatsScreen_Print_Stats
	call StatsScreen_EVInfo
	call StatsScreen_PrintDVs
	ret
.Stats_text:
	db "STATS@"

LoadOrangePage:
	call StatsScreen_placeCaughtLocation
	call StatsScreen_placeCaughtTime
	call StatsScreen_placeCaughtLevel
	call StatsScreen_PrintHappiness
	call StatsScreen_Print_HiddenPow_Info
	ret

StatsScreen_Print_Stats:
; Print wTempMon's stats at hl, with spacing bc.
	push bc
	push hl
	ld de, .StatName_HP
	call PlaceString

	ld de, .StatName_SPE
	call .PrintStatText

	ld de, .StatName_ATK
	call .PrintStatText

	ld de, .StatName_DEF
	call .PrintStatText

	ld de, .StatName_SPA
	call .PrintStatText

	ld de, .StatName_SPD
	call .PrintStatText
	
	pop hl
	pop bc
	add hl, bc
	ld de, wTempMonMaxHP
	lb bc, 2, 3
	call .PrintStat

	ld de, wTempMonSpeed
	call .PrintStat
	ld de, wTempMonAttack
	call .PrintStat
	ld de, wTempMonDefense
	call .PrintStat
	ld de, wTempMonSpclAtk
	call .PrintStat
	ld de, wTempMonSpclDef
	call .PrintStat
	ret

.PrintStat:
	push hl
	call PrintNum
	pop hl
	ld de, SCREEN_WIDTH
	add hl, de
	ret
.PrintStatText:
	ld bc, SCREEN_WIDTH
	add hl, bc
	call PlaceString
	ret

.StatName_HP:
	db   "HP@"
.StatName_SPE:
	db "SPEED@"	
.StatName_ATK:
	db "ATTACK@"
.StatName_DEF:
	db "DEFENSE@"
.StatName_SPA:
	db "SPCLATK@"
.StatName_SPD:
	db "SPCLDEF@"

StatsScreen_EVInfo:
	hlcoord 13, 8
	ld de, .EV_text
	call PlaceString
	; hlcoord 15, 16
	; ld de, .EV_Max_text
	; call PlaceString
	hlcoord 12, 16
	ld de, .EV_MaxSingle_text
	call PlaceString

	
	hlcoord 13, 10
	lb bc, 1, 3
	ld de, wTempMonHPEV
	call .PrintEV

	ld de, wTempMonSpdEV
	call .PrintEV

	ld de, wTempMonAtkEV
	call .PrintEV

	ld de, wTempMonDefEV
	call .PrintEV

	ld de, wTempMonSpclAtkEV
	call .PrintEV

	ld de, wTempMonSpclDefEV
	call .PrintEV
	ret

.PrintEV:
	push hl
	call PrintNum
	pop hl
	ld de, SCREEN_WIDTH
	add hl, de
	ret

.EV_text:
	db "EVS DVS@"
.EV_Max_text:
	db "/510@"
.EV_MaxSingle_text:
	db "/252 /16@"

StatsScreen_PlaceItemName:
	ld de, .Item
	hlcoord 0, 16
	call PlaceString
	call .GetItemName
	hlcoord 5, 16
	call PlaceString
.GetItemName:
	ld de, .ThreeDashes
	ld a, [wTempMonItem]
	and a
	ret z
	ld b, a
	farcall TimeCapsule_ReplaceTeruSama
	ld a, b
	ld [wNamedObjectIndex], a
	call GetItemName
	; ; if we have an item, put icon
	; hlcoord 5, 16
	; ld [hl], $4a
	ret
.Item:
	db "ITEM/@"
.ThreeDashes:
	db "---@"
StatsScreen_PlaceOTInfo:
	ld de, IDNoString
	hlcoord 1, 16
	call PlaceString
	hlcoord 6, 16
	lb bc, PRINTNUM_LEADINGZEROS | 2, 5
	ld de, wTempMonID
	call PrintNum
	ld de, OTString
	hlcoord 1, 17
	call PlaceString	
	ld hl, .OTNamePointers
	call GetNicknamePointer
	call CopyNickname
	farcall CorrectNickErrors
	hlcoord 6, 17
	call PlaceString
	ld a, [wTempMonCaughtGender]
	and a
	jr z, .done
	cp $7f
	jr z, .done
	and CAUGHT_GENDER_MASK
	ld a, "♂" ; $32
	jr z, .got_gender
	ld a, "♀" ; $33 
.got_gender
	hlcoord 4, 17
	ld [hl], a
.done
	ret

.OTNamePointers:
	dw wPartyMonOTs
	dw wOTPartyMonOTs
	dw sBoxMonOTs
	dw wBufferMonOT

IDNoString:
	db "<ID>№.@"

OTString:
	db "OT/@"

StatsScreen_placeCaughtLocation:
	ld de, .MetAtMapString
	hlcoord 1, 9
	call PlaceString
	ld a, [wTempMonCaughtLocation]
	and CAUGHT_LOCATION_MASK
	jr z, .unknown_location
	cp LANDMARK_EVENT
	jr z, .unknown_location
	cp LANDMARK_GIFT
	jr z, .unknown_location
	ld e, a
	farcall GetLandmarkName
	ld de, wStringBuffer1
	hlcoord 2, 10
	call PlaceString
	ret	
.unknown_location:
	ld de, .MetUnknownMapString
	hlcoord 2, 10
	call PlaceString
	ret
.MetAtMapString:
	db "MET: @"
.MetUnknownMapString:
	db "UNKNOWN@"

StatsScreen_placeCaughtTime:
	ld a, [wTempMonCaughtTime]
	and CAUGHT_TIME_MASK
	rlca
	rlca
	dec a
	ld hl, .times
	call GetNthString
	ld d, h
	ld e, l
	call CopyName1
	ld de, wStringBuffer2
	hlcoord 6, 9
	call PlaceString
	ret
.times
	db "MORN@"
	db "DAY@"
	db "NITE@"

StatsScreen_placeCaughtLevel:
	; caught level
	ld a, [wTempMonCaughtLevel]
	and a
	jr z, .unknown_level
	cp CAUGHT_EGG_LEVEL ; egg marker value
	jr nz, .print
	ld a, EGG_LEVEL ; egg hatch level

.print
	ld [wTextDecimalByte], a
	hlcoord 12, 9
	ld de, wTextDecimalByte
	lb bc, PRINTNUM_LEFTALIGN | 1, 3
	call PrintNum
	hlcoord 11, 9
	ld [hl], "<LV>"
	ret

.unknown_level
	ld de, .MetUnknownLevelString
	hlcoord 11, 9
	call PlaceString
	ret   
.MetUnknownLevelString:
	db "TRADE@"

StatsScreen_placeCaughtBall:
	ld a, [wTempMonCaughtBall]
	; check if above last pokeball in sequence
	and a
	ret z
	ld b, a
	farcall TimeCapsule_ReplaceTeruSama
	ld a, b
	dec a ; pokeball is index 1, but the pokeball GFX index is 0
	ld hl, CaughtBallsGFX
	ld bc, 1 * LEN_2BPP_TILE
	call AddNTimes
	ld d, h
	ld e, l
	lb bc, BANK(CaughtBallsGFX), 1
	ld hl, vTiles2 tile $70
	call Request2bpp
	hlcoord 8, 6
	ld [hl], $70
	call WaitBGMap
	;hlcoord 1, 11
	;ld de, .CaughtBallStr
	;call PlaceString
	;ld a, [wTempMonCaughtBall]
	; check if above last pokeball in sequence
	;and a
	;ret z
	;ld b, a
	;farcall TimeCapsule_ReplaceTeruSama
	;ld a, b
	;ld [wNamedObjectIndex], a
	;call GetItemName
	;hlcoord 7, 11
	;call PlaceString

	ret
.unk_ball_str:
	db "???@"
.CaughtBallStr:
	db "BALL: @"
StatsScreen_PrintDVs:
	; hlcoord 1, 8
	; ld de, .DVstring1
	; call PlaceString
	; hlcoord 1, 9
	; ld de, .DVstring2
	; call PlaceString
	; hlcoord 1, 10
	; ld de, .DVstring3
	; call PlaceString

	; we're using wPokedexStatus because why not, nobody using it atm lol
	; ATK DV
	ld a, [wTempMonDVs] ; only get the first byte of the word
	and %11110000 ; most significant nybble of first byte in word-sized wTempMonDVs
	swap a ; so we can print it properly
	ld [wPokedexStatus], a
	ld c, 0
	; calc HP stat contribution
	and 1 ; a still has the ATK DV
	jr z, .atk_not_odd
	ld a, 0
	add 8
	ld b, 0
	ld c, a
	;
.atk_not_odd
	push bc
	ld de, wPokedexStatus
	lb bc,  1, 2 ; bytes, digits
	hlcoord 18, 12
	call PrintNum

	; DEF DV
	ld a, [wTempMonDVs] ; only get the first byte of the word
	and %00001111 ; least significant nybble, don't need to swap the bits of the byte
	ld [wPokedexStatus], a ;DEF
	; calc HP stat contribution
	pop bc
	and 1 ; a still has the DEF DV
	jr z, .def_not_odd
	ld a, c
	add 4
	ld b, 0
	ld c, a
	;
.def_not_odd
	push bc
	ld de, wPokedexStatus
	lb bc,  1, 2
	hlcoord 18, 13
	call PrintNum

	; SPE DV
	ld a, [wTempMonDVs + 1] ; second byte of word
	and %11110000 ; most significant nybble of 2nd byte in word-sized wTempMonDVs
	swap a ; so we can print it properly
	ld [wPokedexStatus], a ;SPEED
	; calc HP stat contribution
	pop bc
	and 1 ; a still has the SPEED DV
	jr z, .speed_not_odd
	ld a, c
	add 2
	ld b, 0
	ld c, a
	;
.speed_not_odd
	push bc
	ld de, wPokedexStatus
	lb bc,  1, 2 ; bytes, digits
	hlcoord 18, 11 ; 1, 5, 9, 13
	call PrintNum

	; SPC DV
	ld a, [wTempMonDVs + 1] ; second byte of word
	and %00001111 ; least significant nybble, don't need to swap the bits of the byte
	ld [wPokedexStatus], a ;SPC
	; calc HP stat contribution
	pop bc
	and 1 ; a still has the DEF DV
	jr z, .spc_not_odd
	ld a, c
	add 1
	ld b, 0
	ld c, a
	;
.spc_not_odd
	push bc
	ld de, wPokedexStatus
	lb bc, 1, 2 ; bytes, digits
	hlcoord 18, 14 ; 1, 4, 7, 10, 13 
	call PrintNum
	hlcoord 18, 15 ; 1, 4, 7, 10, 13 
	call PrintNum

	; HP
	; HP DV is determined by the last bit of each of these four DVs
	; odd Attack DV adds 8, Defense adds 4, Speed adds 2, and Special adds 1
	;For example, a Lugia with the DVs 5 Atk, 15 Def, 13 Spe, and 13 Spc will have:
	; 5 Attack = Odd, HP += 8
	; 15 Defense = Odd, HP += 4
	; 13 Speed = Odd, HP += 2
	; 13 Special = Odd, HP += 1
	;resulting in an HP stat of 15
	; THANKS SMOGON
	; going to "and 1" each final value and push a counter to stack to preserve it
	pop bc
	ld a, c
	ld [wPokedexStatus], a
	ld de, wPokedexStatus
	lb bc,  1, 2 ; bytes, digits
	hlcoord 18, 10 ; 1, 4, 7, 10, 13 
	call PrintNum
	ret

; .DVstring1:
; 	db "DVS: HP    SPE@"
; .DVstring2:	
; 	db "    ATK    DEF@"
; .DVstring3:
; 	db "    SPC@"

StatsScreen_PrintHappiness:
	hlcoord 2, 12
	ld [hl], $34 ; heart icon

	hlcoord 4, 12
	lb bc, 1, 3
	ld de, wTempMonHappiness
	call PrintNum
	ld de, .outofMaxLoveString
	hlcoord 7, 12
	call PlaceString
	ret
.outofMaxLoveString:
	db "/255@"

StatsScreen_LoadUnownFont:
	ld a, BANK(sScratch)
	call OpenSRAM
	ld hl, UnownFont
	; sScratch + $188 was the address of sDecompressBuffer in pokegold
	ld de, sScratch + $188
	ld bc, 38 tiles
	ld a, BANK(UnownFont)
	call FarCopyBytes
	; ld hl, sScratch + $188
	; ld bc, (NUM_UNOWN + 1) tiles
	;call Pokedex_InvertTiles
	ld de, sScratch + $188
	ld hl, vTiles1 tile $3a ;FIRST_UNOWN_CHAR
	lb bc, BANK(Pokedex_LoadUnownFont), NUM_UNOWN
	call Request2bpp
	call CloseSRAM
	ret
StatsScreen_HiddenPow_BP:
	call StatsScreen_LoadUnownFont
; Take the top/most significant bit from each stat
; basically, if the DV is 8 or above
; arrange those bits in order, into a nybble
	; Attack
	ld a, [wTempMonDVs]
	swap a
	and %1000
	; Defense
	ld b, a
	ld a, [wTempMonDVs]
	and %1000
	srl a
	or b
	; Speed
	ld b, a
	ld a, [wTempMonDVs + 1]
	swap a
	and %1000
	srl a
	srl a
	or b
	; Special
	ld b, a
	ld a, [wTempMonDVs + 1]
	and %1000
	srl a
	srl a
	srl a
	or b
; Multiply by 5
	ld b, a
	add a
	add a
	add b
; Add Special & 3
	ld b, a
	ld a, [wTempMonDVs + 1]
	and %0011
	add b
; Divide by 2 and add 30 + 1
	srl a
	add 30
	inc a
	ret

StatsScreen_Print_HiddenPow_Info:
; print Type first
	ld a, [wTempMonDVs]
	and %0011
	ld b, a
	; + (Atk & 3) << 2
	ld a, [wTempMonDVs]
	and %0011 << 4
	swap a
	add a
	add a
	or b
; Skip Normal
	inc a
; Skip Bird
	cp BIRD
	jr c, .done
	inc a
; Skip unused types
	cp UNUSED_TYPES
	jr c, .done
	add UNUSED_TYPES_END - UNUSED_TYPES
.done
	add a
	ld e, a
	ld d, 0
	ld a, BANK(TypeNames)
	ld hl, TypeNames
	add hl, de
	call GetFarWord
	ld d, h
	ld e, l

	hlcoord 2, 15
	call PlaceString_UnownFont_Type
	
	hlcoord 1, 14
	ld de, .hidden_pow_text
	call PlaceString_UnownFont

	call StatsScreen_HiddenPow_BP
	ld de, .hp_70_text
	cp 70
	jr c, .not70
	ld de, .hp_70_text
	sub 70
	jr .print1
.not70
	cp 60
	jr c, .not60
	ld de, .hp_60_text
	sub 60
	jr .print1
.not60
	cp 50
	jr c, .not50
	ld de, .hp_50_text
	sub 50
	jr .print1
.not50
	cp 40
	jr c, .not40
	ld de, .hp_40_text
	sub 40
	jr .print1
.not40
	ld de, .hp_30_text
	sub 30
.print1
	hlcoord 3, 16
	push af
	call PlaceString_UnownFont
	pop af

	cp 9
	jr c, .not9
	ld de, .hp_9_text
	jr .print2
.not9
	cp 8
	jr c, .not8
	ld de, .hp_8_text
	jr .print2
.not8
	cp 7
	jr c, .not7
	ld de, .hp_7_text
	jr .print2
.not7
	cp 6
	jr c, .not6
	ld de, .hp_6_text
	jr .print2
.not6
	cp 5
	jr c, .not5
	ld de, .hp_5_text
	jr .print2
.not5
	cp 4
	jr c, .not4
	ld de, .hp_4_text
	jr .print2
.not4
	cp 3
	jr c, .not3
	ld de, .hp_3_text
	jr .print2
.not3
	cp 2
	jr c, .not2
	ld de, .hp_2_text
	jr .print2
.not2
	cp 1
	ret c
	ld de, .hp_1_text
.print2
	call PlaceString_UnownFont	
	ret
.hidden_pow_text:
	db "HIDDEN POWER@"
.hp_70_text:
	db "SEVENTY@"
.hp_60_text:
	db "SIXTY@"
.hp_50_text:
	db "FIFTY@"
.hp_40_text:
	db "FOURTY@"
.hp_30_text:
	db "THIRTY@"
.hp_1_text:
	db "-ONE@"
.hp_2_text:
	db "-TWO@"
.hp_3_text:
	db "-THREE@"
.hp_4_text:
	db "-FOUR@"
.hp_5_text:
	db "-FIVE@"
.hp_6_text:
	db "-SIX@"
.hp_7_text:
	db "-SEVEN@"
.hp_8_text:
	db "-EIGHT@"
.hp_9_text:
	db "-NINE@"

PlaceString_UnownFont_Type:
	push hl
	push de
.loop
	pop hl
	ld a, BANK(TypeNames)
	call GetFarByte
	ld d, h
	ld e, l
	pop hl
	cp "@"
	ret z
	inc de
	sub "A"
	add $BA ; FIRST_UNOWN_CHAR
	ld [hli], a
	push hl
	push de
	jr .loop

PlaceString_UnownFont:
	push hl
	push de
.loop
	pop hl
	ld a, [hl]
	pop hl
	cp "@"
	ret z
	cp " "
	call z, .skip_space
	cp "-"
	call z, .skip_space
	inc de
	sub "A"
	add $BA ; FIRST_UNOWN_CHAR
	
	ld [hli], a
	push hl
	push de
	jr .loop	
.skip_space:
	ld [hl], a
	inc hl
	push hl
	inc de
	push de
	pop hl
	ld a, [hl]
	pop hl
	ret

StatsScreen_PlaceFrontpic:
	ld hl, wTempMonDVs
	predef GetUnownLetter
	call StatsScreen_GetAnimationParam
	jr c, .egg
	and a
	jr z, .no_cry
	jr .cry

.egg
	call .AnimateEgg
	call SetPalettes
	ret

.no_cry
	call .AnimateMon
	call SetPalettes
	ret

.cry
	call SetPalettes
	call .AnimateMon
	ld a, [wCurPartySpecies]
	call PlayMonCry2
	ret

.AnimateMon:
	ld hl, wStatsScreenFlags
	set 5, [hl]
	ld a, [wCurPartySpecies]
	cp UNOWN
	jr z, .unown
	hlcoord 0, 0
	call PrepMonFrontpic
	ret

.unown
	xor a
	ld [wBoxAlignment], a
	hlcoord 0, 0
	call _PrepMonFrontpic
	ret

.AnimateEgg:
	ld a, [wCurPartySpecies]
	cp UNOWN
	jr z, .unownegg
	ld a, TRUE
	ld [wBoxAlignment], a
	call .get_animation
	ret

.unownegg
	xor a
	ld [wBoxAlignment], a
	call .get_animation
	ret

.get_animation
	ld a, [wCurPartySpecies]
	call IsAPokemon
	ret c
	call StatsScreen_LoadTextboxSpaceGFX
	ld de, vTiles2 tile $00
	predef GetAnimatedFrontpic
	hlcoord 0, 0
	ld d, $0
	ld e, ANIM_MON_MENU
	predef LoadMonAnimation
	ld hl, wStatsScreenFlags
	set 6, [hl]
	ret

StatsScreen_GetAnimationParam:
	ld a, [wMonType]
	ld hl, .Jumptable
	rst JumpTable
	ret

.Jumptable:
	dw .PartyMon
	dw .OTPartyMon
	dw .BoxMon
	dw .Tempmon
	dw .Wildmon

.PartyMon:
	ld a, [wCurPartyMon]
	ld hl, wPartyMon1
	ld bc, PARTYMON_STRUCT_LENGTH
	call AddNTimes
	ld b, h
	ld c, l
	jr .CheckEggFaintedFrzSlp

.OTPartyMon:
	xor a
	ret

.BoxMon:
	ld hl, sBoxMons
	ld bc, PARTYMON_STRUCT_LENGTH
	ld a, [wCurPartyMon]
	call AddNTimes
	ld b, h
	ld c, l
	ld a, BANK(sBoxMons)
	call OpenSRAM
	call .CheckEggFaintedFrzSlp
	push af
	call CloseSRAM
	pop af
	ret

.Tempmon:
	ld bc, wTempMonSpecies
	jr .CheckEggFaintedFrzSlp ; utterly pointless

.CheckEggFaintedFrzSlp:
	ld a, [wCurPartySpecies]
	cp EGG
	jr z, .egg
	call CheckFaintedFrzSlp
	jr c, .FaintedFrzSlp
.egg
	xor a
	scf
	ret

.Wildmon:
	ld a, $1
	and a
	ret

.FaintedFrzSlp:
	xor a
	ret

StatsScreen_LoadTextboxSpaceGFX:
	nop
	push hl
	push de
	push bc
	push af
	call DelayFrame
	ldh a, [rVBK]
	push af
	ld a, $1
	ldh [rVBK], a
	ld de, TextboxSpaceGFX
	lb bc, BANK(TextboxSpaceGFX), 1
	ld hl, vTiles2 tile " "
	call Get2bpp
	pop af
	ldh [rVBK], a
	pop af
	pop bc
	pop de
	pop hl
	ret

StatsScreenSpaceGFX: ; unreferenced
INCBIN "gfx/font/space.2bpp"

EggStatsScreen:
	xor a
	ldh [hBGMapMode], a
	ld hl, wCurHPPal
	call SetHPPal
	ld b, SCGB_STATS_SCREEN_HP_PALS
	call GetSGBLayout
	call StatsScreen_PlaceHorizontalDivider
	ld de, EggString
	hlcoord 8, 1
	call PlaceString
	ld de, IDNoString
	hlcoord 8, 3
	call PlaceString
	ld de, OTString
	hlcoord 8, 5
	call PlaceString
	ld de, FiveQMarkString
	hlcoord 11, 3
	call PlaceString
	ld de, FiveQMarkString
	hlcoord 11, 5
	call PlaceString
if DEF(_DEBUG)
	ld de, .PushStartString
	hlcoord 8, 17
	call PlaceString
	jr .placed_push_start

.PushStartString:
	db "▶PUSH START.@"

.placed_push_start
endc
	ld a, [wTempMonHappiness] ; egg status
	ld de, EggSoonString
	cp $6
	jr c, .picked
	ld de, EggCloseString
	cp $b
	jr c, .picked
	ld de, EggMoreTimeString
	cp $29
	jr c, .picked
	ld de, EggALotMoreTimeString
.picked
	hlcoord 1, 9
	call PlaceString
	ld hl, wStatsScreenFlags
	set 5, [hl]
	call SetPalettes ; pals
	call DelayFrame
	hlcoord 0, 0
	call PrepMonFrontpic
	farcall HDMATransferTilemapToWRAMBank3
	call StatsScreen_AnimateEgg

	ld a, [wTempMonHappiness]
	cp 6
	ret nc
	ld de, SFX_2_BOOPS
	call PlaySFX
	ret

EggString:
	db "EGG@"

FiveQMarkString:
	db "?????@"

EggSoonString:
	db   "It's making sounds"
	next "inside. It's going"
	next "to hatch soon!@"

EggCloseString:
	db   "It moves around"
	next "inside sometimes."
	next "It must be close"
	next "to hatching.@"

EggMoreTimeString:
	db   "Wonder what's"
	next "inside? It needs"
	next "more time, though.@"

EggALotMoreTimeString:
	db   "This EGG needs a"
	next "lot more time to"
	next "hatch.@"

StatsScreen_AnimateEgg:
	call StatsScreen_GetAnimationParam
	ret nc
	ld a, [wTempMonHappiness]
	ld e, $7
	cp 6
	jr c, .animate
	ld e, $8
	cp 11
	jr c, .animate
	ret

.animate
	push de
	ld a, $1
	ld [wBoxAlignment], a
	call StatsScreen_LoadTextboxSpaceGFX
	ld de, vTiles2 tile $00
	predef GetAnimatedFrontpic
	pop de
	hlcoord 0, 0
	ld d, $0
	predef LoadMonAnimation
	ld hl, wStatsScreenFlags
	set 6, [hl]
	ret

StatsScreen_LoadPageIndicators:
IF DEF(ORANGE_PAGE)

	hlcoord 11, 5
	ld a, $42 ; " " " "
	call .load_square
	hlcoord 13, 5
	ld a, $36 ; first of 4 small square tiles
	call .load_square
	hlcoord 15, 5
	ld a, $42 ; " " " "
	call .load_square
	hlcoord 17, 5
	ld a, $36 ; " " " "
	call .load_square
	ld a, c
	cp PINK_PAGE
	hlcoord 11, 5
	jr z, .load_highlighted_square_alt
	cp GREEN_PAGE
	hlcoord 13, 5
	jr z, .load_highlighted_square
	cp BLUE_PAGE
	hlcoord 15, 5
	jr z, .load_highlighted_square_alt
	; must be ORANGE_PAGE
	hlcoord 17, 5
	; fallthrough to .load_highlighted_square
ELSE
	hlcoord 13, 5
	ld a, $42 ; first of 4 small square tiles
	call .load_square
	hlcoord 15, 5
	ld a, $36 ; " " " "
	call .load_square
	hlcoord 17, 5
	ld a, $42 ; " " " "
	call .load_square
	ld a, c
	cp PINK_PAGE
	hlcoord 13, 5
	jr z, .load_highlighted_square_alt	
	cp GREEN_PAGE
	hlcoord 15, 5
	jr z, .load_highlighted_square
	; can assume cp BLUE_PAGE will be true, no other choices
	hlcoord 17, 5
ENDC	

.load_highlighted_square	
	ld a, $3a ; first of 4 large square tiles
.load_square
	push bc
	ld [hli], a
	inc a
	ld [hld], a
	ld bc, SCREEN_WIDTH
	add hl, bc
	inc a
	ld [hli], a
	inc a
	ld [hl], a
	pop bc
	ret
.load_highlighted_square_alt
	ld a, $46 ; first of 4 large square tiles, alternate Gray pixels for use of 3rd color slot
	jr .load_square

CopyNickname:
	ld de, wStringBuffer1
	ld bc, MON_NAME_LENGTH
	jr .okay ; utterly pointless
.okay
	ld a, [wMonType]
	cp BOXMON
	jr nz, .partymon
	ld a, BANK(sBoxMonNicknames)
	call OpenSRAM
	push de
	call CopyBytes
	pop de
	call CloseSRAM
	ret

.partymon
	push de
	call CopyBytes
	pop de
	ret

GetNicknamePointer:
	ld a, [wMonType]
	add a
	ld c, a
	ld b, 0
	add hl, bc
	ld a, [hli]
	ld h, [hl]
	ld l, a
	ld a, [wMonType]
	cp TEMPMON
	ret z
	ld a, [wCurPartyMon]
	jp SkipNames

CheckFaintedFrzSlp:
	ld hl, MON_HP
	add hl, bc
	ld a, [hli]
	or [hl]
	jr z, .fainted_frz_slp
	ld hl, MON_STATUS
	add hl, bc
	ld a, [hl]
	and 1 << FRZ | SLP_MASK
	jr nz, .fainted_frz_slp
	and a
	ret

.fainted_frz_slp
	scf
	ret

PrintMonTypeTiles:
	call GetBaseData
	ld a, [wBaseType1]
	ld c, a ; farcall will clobber a for the bank
	farcall GetMonTypeIndex
	ld a, c
	ld hl, TypeLightIconGFX ; from gfx\stats\types_light.png
	ld bc, 4 * LEN_2BPP_TILE ; Type GFX is 4 tiles wide
	call AddNTimes
	ld d, h
	ld e, l
	ld hl, vTiles2 tile $4c
	lb bc, BANK(TypeLightIconGFX), 4 ; Bank in 'c', Number of Tiles in 'c'
	call Request2bpp

; placing the Type1 Tiles (from gfx\stats\types_light.png)
	hlcoord 5, 13
	ld [hl], $4c
	inc hl
	ld [hl], $4d
	inc hl
	ld [hl], $4e
	inc hl
	ld [hl], $4f
	inc hl
	ld a, [wBaseType1]
	ld b, a
	ld a, [wBaseType2]
	cp b
	ret z; Pokemon only has one Type

	; Load Type2 GFX
	; 2nd Type
	ld c, a ; Pokemon's second type
	farcall GetMonTypeIndex
	ld a, c
	ld hl, TypeDarkIconGFX ; from gfx\stats\types_dark.png
	ld bc, 4 * LEN_2BPP_TILE ; Type GFX is 4 Tiles Wide
	call AddNTimes ; type index needs to be in 'a'
	ld d, h
	ld e, l
	ld hl, vTiles2 tile $5c
	lb bc, BANK(TypeDarkIconGFX), 4 ; Bank in 'c', Number of Tiles in 'c'
	call Request2bpp
	
; place Type 2 GFX
	hlcoord 5, 14
	ld [hl], $5c
	inc hl
	ld [hl], $5d
	inc hl
	ld [hl], $5e
	inc hl
	ld [hl], $5f
	ret
