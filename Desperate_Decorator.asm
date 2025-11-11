* = $C000

!source <c64/symbols.asm>
!source <c64/keyboard.asm>
!source <chip/vic_ii.asm>
!source <chip/sid.asm>
!source <chip/cia.asm>

; Global variables
_ROLLER_BASE          = $03B6   ; Current base address of the roller.
_ROLLER_H_OFFSET      = $03B9   ; Current horizontal offset of the roller.

_ROLLER_V_POS_CURR    = $03CA   ; Current vertical position of the roller.
_ROLLER_V_POS         = $03CB   ; Comnputed new vertical position of the roller (discarded if out of bounds).

_ROLLER_SCANLINE_CURR = $03BA   ; Current scanline in text row occupied by the roller.
_ROLLER_SCANLINE      = $03B8   ; Computed new scanline in text row occupied by the roller (discarded if out of bounds).

_DRIP_SPEED           = $03C8   ; Number of iterations dedicated to paint dripping animation before returning control to the player.
_DRIP_TIMER           = $03C9   ; When it equals _DRIP_SPEED it's time to return control to the player.

; Global constants
BITMAP_BASE           = $2000   ; Base address of the hi-res screen.
CINT1                 = $E518   ; Part of VIC-II initialization routine: returns to text screen and default colors.

!zone Init
INIT:
  ldx #$00
  lda #$00

.Loop_Init_Drips:
  sta _DRIP_LENGTH_TABLE,x      ; Set all the 256 drips to zero length.
  inx
  bne .Loop_Init_Drips

  lda #VIC_GREEN                ; Color border in green.
  sta EXTCOL

DRAW_ROOM:
  jsr PREPARE_SCREEN            ; Init the hi-res screen.

  ldx #$00                      ; Set carpet color to blue.
  lda #VIC_BLUE

.Loop_Draw_Carpet:              ; The carpet fills the bottom line of the screen.
  sta VICSCN+(24*40),x
  inx
  cpx #40
  bne .Loop_Draw_Carpet
  rts

!zone Main
; Any location on the screen is always addressed in the same way,
; be it the roller or a drip of paint:
; base address of the row in which the location to be tested is stored in ZP_1
; and horizontal offset in .Y, after which, indirect indexed addressing is used
; to manipulate the byte.

MAIN_LOOP:
  lda _ROLLER_SCANLINE_CURR     ; Going towards the bottom of the screen the base address of a pixel row
  sta _ROLLER_SCANLINE          ; increases by 1 within a text line but jumps by 313 when crossing the boundary
                                ; to the next text line: a scanline counter is necessary to keep track
                                ; of the boundary crossing.


  ldy _ROLLER_H_OFFSET          ; Recover horizontal offset in .Y.

  lda _ROLLER_BASE              ; Recover the low byte of the base address of the current row in which the roller is
  sta ZP_1                      ; and store it in ZP_1.

  lda _ROLLER_V_POS_CURR        ; Recover the current vertical position of the roller.
  sta _ROLLER_V_POS

  lda _ROLLER_BASE+1            ; Recover the high byte of the base address of the current row in which the roller is
  sta ZP_1+1                    ; and store it in ZP_1+1.

  jsr WIPE_PAINT                ; Check if any drip of paint is touching the roller
  lda #$00                      ; and wipe it.
  sta (ZP_1),y

CHECK_UP:
  lda LSTX                      ; Check if the key for moving the roller up is pressed:
  cmp #KBD_SEMICOLON
  bne CHECK_DOWN

  sec                           ; in that case, subtract 1 from the current value of the base address of the roller.
  lda ZP_1
  sbc #$01
  dec _ROLLER_V_POS             ; Also decrement the vertical position counter.
  sta ZP_1
  lda ZP_1+1
  sbc #$00
  sta ZP_1+1

  dec _ROLLER_SCANLINE          ; Decrement the scanline counter:
  lda _ROLLER_SCANLINE          ; if it reaches 0, the boundary between current and previous text line has been crossed
  cmp #$00
  bne CHECK_DOWN

  lda #$08                      ; in which case set it to 8, indicating the last scanline of the previous text line
  sta _ROLLER_SCANLINE

  lda ZP_1                      ; and subtract 312: the C flag is already set by the previous comparison
  sbc #<312                     ; and 1 position has been already subtracted earlier, thus we are actually
  sta ZP_1                      ; subtracting 313 from the base address.
  lda ZP_1+1
  sbc #>312

  cmp #>BITMAP_BASE             ; If we fall out of the screen just play a sound and skip next movement checks.
  bcc PLAY_SFX                  ; Note that in this case ZP_1+1 is left in an inconsistent state, but this is not
                                ; a problem, because we skip also the part in which all previous changes are recorded
  sta ZP_1+1                    ; otherwise update ZP_1+1.

CHECK_DOWN:
  lda LSTX                      ; Now check if the key for moving the roller down is pressed:
  cmp #KBD_SLASH
  bne CHECK_LEFT

  clc                           ; in that case, add 1 to the current value of the base address of the roller.
  lda ZP_1
  adc #$01
  inc _ROLLER_V_POS             ; Also increment the vertical position counter.
  sta ZP_1
  lda ZP_1+1
  adc #$00
  sta ZP_1+1

  inc _ROLLER_SCANLINE          ; Increment the scanline counter:
  lda _ROLLER_SCANLINE          ; if it reaches 9, the boundary between current and next text line has been crossed
  cmp #$09
  bne CHECK_LEFT

  lda #$01                      ; in which case set it to 1, indicating the first scanline of the next text line
  sta _ROLLER_SCANLINE

  lda ZP_1                      ; and add 311: the C flag is already set by the previous comparison
  adc #<311                     ; and 1 position has been already added earlier, thus we are actually
  sta ZP_1                      ; adding 313 to the base address.
  lda ZP_1+1
  adc #>311

  cmp #(>BITMAP_BASE)+$1E       ; If we fall out of the screen just play a sound and skip next movement checks.
  bcs PLAY_SFX                  ; Note that in this case ZP_1+1 is left in an inconsistent state, but this is not
                                ; a problem, because we skip also the part in which all previous changes are recorded
  sta ZP_1+1                    ; otherwise update ZP_1+1.

CHECK_LEFT:
  lda SHFLAG                    ; Now check if the key for moving the roller left is pressed:
  cmp #$02
  bne CHECK_RIGHT

  tya                           ; in that case subtract 8 from the horizontal offset stored in .Y:
  sec                           ; .Y = 0 corresponds to the leftmost position of the roller on the wall.
  sbc #$08
  tay

CHECK_RIGHT:
  lda SHFLAG                    ; Finally check if the key for moving the roller right is pressed:
  cmp #$01
  bne DRAW_ROLLER

  tya                           ; in that case add 8 to the horizontal offset stored in .Y:
  clc                           ; .Y = $F8 corresponds to the rightmost position of the roller on the wall.
  adc #$08
  tay

DRAW_ROLLER:
  lda #$FF                      ; Draw the roller in the new position.
  sta (ZP_1),y

  sty _ROLLER_H_OFFSET          ; Save the updated horizontal offset of the roller.

  lda _ROLLER_V_POS             ; Save the updated vertical position of the roller.
  sta _ROLLER_V_POS_CURR

  lda _ROLLER_SCANLINE          ; Save the updated scanline counter.
  sta _ROLLER_SCANLINE_CURR

  lda ZP_1                      ; Finally save the updated base address of the row in which the roller is.
  sta _ROLLER_BASE
  lda ZP_1+1
  sta _ROLLER_BASE+1

PLAY_SFX:
  lda #$00                      ; After the roller has moved in the new location
  sta _DRIP_TIMER               ; reset the drip timer

  lda #$13                      ; Set SID voice #1 to triangular waveform, gate bit on: start ADS phase
  sta VCREG1                    ; Also synchronize with oscillator #3.

  lda ZP_1+1                    ; Set pitch according to roller position:
  sta FREHI1                    ; the closer to the carpet, the higher the pitch.

  lda #$12                      ; Gate bit off: start Release phase.
  sta VCREG1

  jsr WIPE_PAINT                ; Wipe the new paint touching the roller
  jmp DRIP_PAINT                ; and drip the paint elsewhere.

WIPE_PAINT:
  ldx #$00                      ; Start from leftmost drip.

.Loop_Check_Drips:
  lda _DRIP_H_OFFSET_TABLE,x    ; Compare offset of the scanned drip
  cmp _ROLLER_H_OFFSET          ; with offset of the roller:
  bne .Next_Check_Drips         ; if they don't match pass to next drip.

  lda _DRIP_LENGTH_TABLE,x      ; If they match, check if the length of the drip
  cmp _ROLLER_V_POS_CURR        ; matches the vertical position of the roller:
  bne .Next_Check_Drips         ; if they don't match pass to next drip.

  cmp #$00                      ; Also, if the drip is completely dry pass to the next
  beq .Next_Check_Drips

  sec                           ; If we get there, the roller touches the bottom end of the drip
  sbc #$01                      ; so shorten its length by one: now the drip just brushes against the roller
  sta _DRIP_LENGTH_TABLE,x      ; and next time the drip is checked it will touch again the roller, provided it has not moved.
                                ; Net result is that a stationary roller will prevent any touched drip to keep dripping.

  lda #$21                      ; Change waveform of voice #1 to sawtooth and stop synchronization with oscillator #3
  sta VCREG1

.Next_Check_Drips:
  inx                           ; Increment drip counter and loop back.
  bne .Loop_Check_Drips
  rts

DRIP_PAINT:
  ldx TIMALO                    ; This counter is incremented each CPU cycle.
  jsr COMPUTE_DRIP_BYTE_LOC     ; Compute the location of the bottom end of the drip whose index is in .X
  stx TEMP_1                    ; and place it in ZP_2, then save .X in a temporary location.

  lda _DRIP_TO_BITMASK_TABLE,x  ; Select the appropriate bitmask to set up the pixel corresponding to the drip
  tax                           ; and plot it.
  lda _BITMASK_TABLE,x
  ldx TEMP_1
  ldy #$00
  ora (ZP_2),y
  sta (ZP_2),y

  inc _DRIP_LENGTH_TABLE,x      ; Now that the drip has been plotted, increment its length
  lda _DRIP_LENGTH_TABLE,x      ; and if it reaches the bottom of the screen...
  cmp #199
  bne .Another_Drip
  jmp CINT1                     ; ...it's GAME OVER!!!

.Another_Drip:
  inc _DRIP_TIMER               ; Otherwise, increment the timer counter
  lda _DRIP_SPEED               ; and if it has not reached the threshold
  cmp _DRIP_TIMER               ; keep on dripping!
  bne DRIP_PAINT
  jmp MAIN_LOOP

COMPUTE_DRIP_BYTE_LOC:
  lda _DRIP_LENGTH_TABLE,x      ; To compute the location of the tail end of a drip
  tay                           ; first we must know its length in order to compute the base address
                                ; which will be stored in ZP_2
  lda _DRIP_TAIL_BASE_LO_TABLE,y
  sta ZP_2
  lda _DRIP_TAIL_BASE_HI_TABLE,y
  sta ZP_2+1

  lda _DRIP_H_OFFSET_TABLE,x    ; Once computed the base address we add the offset.
  clc
  adc ZP_2
  sta ZP_2
  lda ZP_2+1
  adc #$00
  sta ZP_2+1

  rts

PREPARE_SCREEN:
  lda #>BITMAP_BASE             ; Hi-res screen starts at $2000
  sta ZP_1+1
  lda #<BITMAP_BASE
  sta ZP_1

.Clear_Bitmap:
  ldy #$00
  lda #$00

.Loop_Clear_Bitmap:
  sta (ZP_1),y                  ; Loop 32 times to clear full screen
  iny
  bne .Loop_Clear_Bitmap
  clc
  lda ZP_1+1
  cmp #$3F
  beq .Dead_Code
  inc ZP_1+1
  bne .Clear_Bitmap

.Dead_Code:
  ldx #$00                      ; This code simply fills $4000-$401E with zeros
  lda #$00                      ; Much likely it's a remnant of previous development

.Loop_Dead_Code:
  sta $4000,x
  inx
  cpx #$3F
  bne .Loop_Dead_Code

.Draw_Wall:
  ldx #$00                      ; The playfield is 32 text columns wide
  lda #VIC_CYAN                 ; It's all drawn in cyan, the side walls will
                                ; be drawn by the BASIC code
.Loop_Draw_Wall:
  sta VICSCN,x
  sta VICSCN+$0100,x
  sta VICSCN+$0200,x
  sta VICSCN+$02E8,x
  inx
  bne .Loop_Draw_Wall

  lda #%00011101                ; Screen memory starts at $0400,bitmap starts at $2000
  sta VMCSB
  lda #%00111011                ; Enable hi-res mode
  sta SCROLY

  rts

!zone Tables
  * = $C700

; The drawing of the drips is driven by a number of tables:
;
; There are 256 drips of paint: the leftmost (drip #0) is at pixel column #32,
; the rightmost (drip #255) is at pixel column 32+255 = #287:
; All the drips start at pixel row #0, therefore the byte containing
; the initial pixel of a drip is given by the following formula:
;
; 8192 + 32 + 8 * (num_drip / 8)
; \__/  \__/  \________________/
;  |     |            |
;  |     |            +------ horizontal offset
;  |     |
;  |     +------ pixel column of drip #0  \
;  |                                      +--- base address of playfield's row #0
;  +------ base address of hi-res screen  /
;
; Drips #0-#7 begin at byte 8224 ($2020), drips #8-#15 begin at byte 8232 ($2028)
; and so on, up to drips #248-#255 beginning at byte 8472 ($2118).
; The formula is not computed directly: rather a 256 bytes table is used:
; entering with num_drip it returns the horizontal offset (H_OFFSET).
;
; Once the horizontal offset is known, the byte containing the tail end
; (or any other point in between, for what it matters) can easily be calculated
; changing the base address according to the desired vertical position.
;
; As known, the address of each row of C64's hires screen is laid out this way:
;
; Row #0:  base address of hi-res screen
; Row #1:  base address + 1
; ...
; Row #7:  base address + 7
; Row #8:  base address + 64
; ...
; Row #15: base address + 71
; Row #16: base address + 128, and so on...
;
; The base address of any vertical position along a drip is given by the following
; formula:
;
; 8192 + 32 + 256 * (drip_vpos / 8) + 64 * (drip_vpos / 8) + (drip_vpos % 8)
; \__/  \__/  \____________________________________________________________/
;  |     |                                  |
;  |     |                                  +------ vertical offset
;  |     |
;  |     +------ pixel column of drip #0  \
;  |                                      +--- base address of playfield's row #0
;  +------ base address of hi-res screen  /
;
; Again, the formula is not computed directly: rather the full base address,
; not just the vertical offset, is computed using two tables, each giving the low
; and the high byte of the result when entering with drip_vpos (the result is greater
; than 255 so we need two tables DRIP_TAIL_BASE_LO and _HI).
;
; Once these quantities are known, any byte on the playfield is accessible:
; to access single pixels two more formulas are needed.
;
; The first formula computes which bit in the bye corresponds to particular drip:
;
; num_drip % 8 --- DRIP_TO_BITMASK
;
; and the second one to compute the OR-mask needed to set that pixel:
;
; 2 ^ (7 - DRIP_TO_BITMASK) --- BITMASK
;
; Two tables are used because only the first is necessary if you need to iterate drips.
;
; Finally, the current length of each drip is stored in another table
; which is filled with $00 at the beginning of a new game (DRIP_LENGTH).

_DRIP_TAIL_BASE_LO_TABLE:
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7
  !byte $20,$21,$22,$23, $24,$25,$26,$27, $60,$61,$62,$63, $64,$65,$66,$67
  !byte $A0,$A1,$A2,$A3, $A4,$A5,$A6,$A7, $E0,$E1,$E2,$E3, $E4,$E5,$E6,$E7

_DRIP_TAIL_BASE_HI_TABLE:
  !byte $20,$20,$20,$20, $20,$20,$20,$20, $21,$21,$21,$21, $21,$21,$21,$21
  !byte $22,$22,$22,$22, $22,$22,$22,$22, $23,$23,$23,$23, $23,$23,$23,$23
  !byte $25,$25,$25,$25, $25,$25,$25,$25, $26,$26,$26,$26, $26,$26,$26,$26
  !byte $27,$27,$27,$27, $27,$27,$27,$27, $28,$28,$28,$28, $28,$28,$28,$28
  !byte $2A,$2A,$2A,$2A, $2A,$2A,$2A,$2A, $2B,$2B,$2B,$2B, $2B,$2B,$2B,$2B
  !byte $2C,$2C,$2C,$2C, $2C,$2C,$2C,$2C, $2D,$2D,$2D,$2D, $2D,$2D,$2D,$2D
  !byte $2F,$2F,$2F,$2F, $2F,$2F,$2F,$2F, $30,$30,$30,$30, $30,$30,$30,$30
  !byte $31,$31,$31,$31, $31,$31,$31,$31, $32,$32,$32,$32, $32,$32,$32,$32
  !byte $34,$34,$34,$34, $34,$34,$34,$34, $35,$35,$35,$35, $35,$35,$35,$35
  !byte $36,$36,$36,$36, $36,$36,$36,$36, $37,$37,$37,$37, $37,$37,$37,$37
  !byte $39,$39,$39,$39, $39,$39,$39,$39, $3A,$3A,$3A,$3A, $3A,$3A,$3A,$3A
  !byte $3B,$3B,$3B,$3B, $3B,$3B,$3B,$3B, $3C,$3C,$3C,$3C, $3C,$3C,$3C,$3C
  !byte $3E,$3E,$3E,$3E, $3E,$3E,$3E,$3E, $3F,$3F,$3F,$3F, $3F,$3F,$3F,$3F
  !byte $40,$40,$40,$40, $40,$40,$40,$40, $41,$41,$41,$41, $41,$41,$41,$41
  !byte $43,$43,$43,$43, $43,$43,$43,$43, $44,$44,$44,$44, $44,$44,$44,$44
  !byte $45,$45,$45,$45, $45,$45,$45,$45, $46,$46,$46,$46, $46,$46,$46,$46

_DRIP_TO_BITMASK_TABLE:
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07
  !byte $00,$01,$02,$03, $04,$05,$06,$07, $00,$01,$02,$03, $04,$05,$06,$07

_DRIP_H_OFFSET_TABLE:
  !byte $00,$00,$00,$00, $00,$00,$00,$00, $08,$08,$08,$08, $08,$08,$08,$08
  !byte $10,$10,$10,$10, $10,$10,$10,$10, $18,$18,$18,$18, $18,$18,$18,$18
  !byte $20,$20,$20,$20, $20,$20,$20,$20, $28,$28,$28,$28, $28,$28,$28,$28
  !byte $30,$30,$30,$30, $30,$30,$30,$30, $38,$38,$38,$38, $38,$38,$38,$38
  !byte $40,$40,$40,$40, $40,$40,$40,$40, $48,$48,$48,$48, $48,$48,$48,$48
  !byte $50,$50,$50,$50, $50,$50,$50,$50, $58,$58,$58,$58, $58,$58,$58,$58
  !byte $60,$60,$60,$60, $60,$60,$60,$60, $68,$68,$68,$68, $68,$68,$68,$68
  !byte $70,$70,$70,$70, $70,$70,$70,$70, $78,$78,$78,$78, $78,$78,$78,$78
  !byte $80,$80,$80,$80, $80,$80,$80,$80, $88,$88,$88,$88, $88,$88,$88,$88
  !byte $90,$90,$90,$90, $90,$90,$90,$90, $98,$98,$98,$98, $98,$98,$98,$98
  !byte $A0,$A0,$A0,$A0, $A0,$A0,$A0,$A0, $A8,$A8,$A8,$A8, $A8,$A8,$A8,$A8
  !byte $B0,$B0,$B0,$B0, $B0,$B0,$B0,$B0, $B8,$B8,$B8,$B8, $B8,$B8,$B8,$B8
  !byte $C0,$C0,$C0,$C0, $C0,$C0,$C0,$C0, $C8,$C8,$C8,$C8, $C8,$C8,$C8,$C8
  !byte $D0,$D0,$D0,$D0, $D0,$D0,$D0,$D0, $D8,$D8,$D8,$D8, $D8,$D8,$D8,$D8
  !byte $E0,$E0,$E0,$E0, $E0,$E0,$E0,$E0, $E8,$E8,$E8,$E8, $E8,$E8,$E8,$E8
  !byte $F0,$F0,$F0,$F0, $F0,$F0,$F0,$F0, $F8,$F8,$F8,$F8, $F8,$F8,$F8,$F8

_DRIP_LENGTH_TABLE:
  !fill 256,0

  * = $03C0
_BITMASK_TABLE:
  !byte %10000000
  !byte %01000000
  !byte %00100000
  !byte %00010000
  !byte %00001000
  !byte %00000100
  !byte %00000010
  !byte %00000001
!zone