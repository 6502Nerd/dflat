;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  KEYBOARD.S
;*	Keyboard driver code. It is very slow to scan so
;*	routines here do a basic scan for any key before finding
;*	the specific key. Still, it has a noticeable impact on
;*	CPU especially in dflat which checks the key after every
;*	keyword is executed.
;*	Rather than working off interrupts these routines just
;*	need to be called as needed. The T1 interrupt keeps
;*	track of keyboard timers for repeat delay and speed.
;*
;**********************************************************

	; ROM code
	code

;****************************************
;* init_keyboard
;* Initialise the keyboard settings
;****************************************
init_keyboard
	lda #KB_REP_DEL
	sta kb_rdel_tim
	lda #KB_REP_TIM
	sta kb_rep_tim
	lda #KB_DEBOUNCE
	sta kb_deb_tim

	; ** check if IJK stick connected **
	php
	sei
	; Save port A
	lda IO_0+DDRA
	pha
	lda IO_0+PRA
	pha

	; Check IJK present
	; set top two bits of porta to output and rest as input
	lda   #0b11000000
	jsr via_strobe_init
	; Select left+right stick
	lda #0b11000000
	sta IO_0+PRAH
	; read stick sense line
	lda IO_0+PRAH
	and #KB_IJK
	eor #KB_IJK
	sta kb_stat
	jsr via_strobe_off

	; Restore port A
	pla
	sta IO_0+PRA
	pla
	sta IO_0+DDRA

	plp
	rts


;****************************************
;* kb_stick
;* Check for fire | down | up | right | left
;*        bit  4     3      2     1       0
;* A = Returns bit mask of keys pressed
;* Y corrupted
;****************************************
kb_stick
	; if IJK connected then read joystick
	lda kb_stat
	and #KB_IJK
	bne kb_stick_ijk

	; Select Row 4 only, all keys on this row
	lda #4+KB_PRB			; Maintain upper nibble of PRB
	sta IO_0+PRB
	lda #SND_REG_IOA		; Select AY Port A for columns
	jsr snd_sel_reg
	lda #0					; Result will be in A
	pha
	ldy #4					; Go through the 5 cols on row 4
kb_stick_pos
	lda kb_stick_mask,y		; Get the column mask
	jsr snd_set_reg			; Activate column
	nop
	nop
	nop
	nop
	lda #KB_SENSE			; Something pressed?
	and IO_0+PRB			; Read Port B
	cmp #KB_SENSE			; C=1 if set else 0
	pla
	rol a					; Get C in to A
	pha
	dey
	bpl kb_stick_pos		; Do all 5 positions
	pla						; Result in A
	rts
kb_stick_ijk
	php
	sei

	; Save port A
	lda IO_0+DDRA
	pha
	lda IO_0+PRA
	pha

	;set top two bits of porta to output and rest as input
	lda   #0b11000000
	jsr via_strobe_init

	;Select Left Joystick
	lda #0b01111111
	sta IO_0+PRA
	;Read back Left Joystick state into A
	lda IO_0+PRA
	; release joystick IO settings
	jsr via_strobe_off
	;Mask out unused bits and invert
	and #0b00011111
	eor #0b00011111
	tay
	; use this as an index into the mapping
	; to get the same bit representation
	; as if using cursor and space keys
	lda kb_ijk_map,y
	tay

	; Restore port A
	pla
	sta IO_0+PRA
	pla
	sta IO_0+DDRA

	plp
	; result in A
	tya
	rts


;****************************************
;* kb_any_key
;* Quick check for any key except shifts & ctrl
;* Carry = 1 means key pressed
;****************************************
kb_any_key
	lda #SND_REG_IOA		; Select Port A of AY
	jsr snd_sel_reg

	ldy #7+KB_PRB			; Start from row 7
kb_any_key_row
	sty IO_0+PRB			; Select row on port B
	; Select all columns except 4
	lda #0b00010000			; Deselect only col 4
	jsr snd_set_reg

	nop
	nop
	nop						; New NOP
	nop
	nop
;	nop						; New NOP

	lda #KB_SENSE			; Something pressed?
	and IO_0+PRB			; Read Port B
	bne kb_any_key_pressed
	dey						; If not then next row
	cpy #KB_PRB-1			; Done rows 0..8?
	bne kb_any_key_row		; Until all rows done
kb_any_key_none
	clc						; C=0 means not pressed
	rts
kb_any_key_pressed
kb_read_got
	sec						; C=1 means pressed
	rts

;****************************************
;* kb_read_raw
;* Read keyboard
;* Y = Keyboard code
;* Carry = 1 means key found, 0 = no keys found
;****************************************
kb_read_raw
	jsr kb_any_key			; Quick check is anything down?
	bcc kb_read_nothing		; Don't bother if not
kb_read_raw_force
	ldx #SND_REG_IOA		; Select Port A of AY
	stx SND_ADBUS			; Put reg # on Port A (sound bus)
	ldx #SND_SELSETADDR		; Get ready to select the reg
	stx SND_MODE			; Latch the reg # on Port A
	ldx #SND_DESELECT		; Deselect AY
	stx SND_MODE
	ldx #0					; Start at column 0
	stx zp_tmp1
kb_check_matrix_col
	ldy #0+KB_PRB			; Start at row 0 (maintain PRB upper nibble)
kb_check_matrix_row
	sty IO_0+PRB			; Select row from Y
	; Get the col value for AY port A
	ldx zp_tmp1
	lda kb_col_mask,x
	; Write it to AY port A
	sta SND_ADBUS			; Put col value on AY bus
	ldx #SND_SELWRITE		; Select mode for writing data
	stx SND_MODE			; Latch reg value on Port A
	ldx #SND_DESELECT		; Deselect AY
	stx SND_MODE

	nop						; Wait 10 cycles before reading sense pin
	nop
	nop

	lda #KB_SENSE			; Bit 3 is the sense
	and IO_0+PRB			; And with Port B

	bne kb_read_raw_got
	; No key for this row/col, next
	iny
	cpy #8+KB_PRB			; Done 8 rows?
	bne kb_check_matrix_row
	; ok check next row
	ldx zp_tmp1
kb_check_skip4
	inx
	cpx #4					; Skip 4?
	beq kb_check_skip4
	stx zp_tmp1
	cpx #8					; Done 8 cols?
	bne kb_check_matrix_col
	; No key was sensed
kb_read_nothing
	ldy #0					; Raw key codes
	clc						; No key sensed flag
	rts
kb_read_raw_got
	;Y=row, zp_temp1=col
	lda zp_tmp1				; Get the column num
	asl a					; Shift in to bits 5,4,3
	asl a
	asl a
	sta zp_tmp1
	tya						; Now or with row number
	and #7					; Only bottom 3 bits!
	ora zp_tmp1
	tay						; Put in to Y
	sec
	rts

;****************************************
;* kb_scan_key
;* Scans for a key, returns zero for no key found
;* Processes caps and shift lock but these don't count as key presses
;* A = Key code
;****************************************
kb_scan_key
	lda kb_deb				; Do not scan keyboard too often
	bne kb_no_scan
	lda kb_deb_tim			; Else reset debounce timer
	sta kb_deb
	jsr kb_read_raw			; Check if a key is sensed
	bcs kb_scan_decode		; go ahead and decode
	; If pressed nothing then reset timers
	lda #255
	sta kb_raw				; Reset raw key settings
	sta kb_last				; And last key
	lda kb_rdel_tim			; Reset repeat timer to initial delay
	sta kb_rep
kb_no_scan
	lda #0
	sec						; Code not valid
	rts						; And done (A=0)
kb_scan_decode
	; If got here then raw key is good
	lda kb_last				; Preload A with last decoded key value
	cpy kb_raw				; Same as last raw key?
	sty kb_raw				; Already save new raw key
	bne kb_process_new		; If is new raw key, look at shift/ctrl/caps
	beq kb_do_repeat		; Else go handle repeating with A containing last
kb_process_new
	lda kb_rdel_tim			; Reset repeat timer to initial delay
	sta kb_rep
	; Now to get a proper key code translated from raw
	; Check for shift and ctrl (not debounced!)
	lda #0b11101111			; Select column 4
	ldx #SND_REG_IOA		; On AY port A
	jsr snd_set

	; check shifted keys
	ldx #4+KB_PRB			; Row 4 (left shift)
	stx IO_0+PRB			; Select row on port B
	nop
	nop
	nop
	nop

	lda IO_0+PRB			; Read Port B

	ldx #7+KB_PRB			; Row 7 (right shift)
	stx IO_0+PRB			; Select row on port B
	nop
	nop
	nop
	nop

	ora IO_0+PRB			; Combine Port B
	ldx kb_table_std,y		; Pre-load standard key code in X
	and #KB_SENSE			; Bit 3 is the sense
	beq kb_read_noshift		; Skip over if no shift
	ldx kb_table_shift,y	; Load up standard key code mapping
kb_read_noshift
	stx kb_code				; Save the mapped keycode
	; check ctrl key
	ldx #2+KB_PRB			; Row 2 (ctrl key)
	stx IO_0+PRB			; Select row on port B
	nop
	nop
	nop
	nop

	lda IO_0+PRB			; Read Port B
	and #KB_SENSE
	beq kb_skip_ctrl
	lda kb_code
	and #0x1f				; Ctrl will result in codes 0 to 31
	sta kb_code				; Override the keycode
	beq kb_brk
	bpl	kb_store_last		; Check repeat (bpl is always true)
kb_skip_ctrl
	lda kb_stat				; Check caps lock
	and #KB_CAPSLK
	beq kb_store_last
	lda kb_code
	cmp #'a'				; If < 'a' then skip
	bcc kb_store_last
	cmp #'z'+1				; If > 'z' then skip
	bcs kb_store_last
	lda kb_code				; Get the actual code
	eor #0x20				; Switch off bit 0x20
	sta kb_code				; Save the capitalised code
	bne kb_store_last		; always
kb_do_repeat
	ldx kb_rep				; Has repeat expired?
	bne	kb_in_repeat		; If not then still in repeat
	ldx kb_rep_tim			; Set repeat speed
	stx kb_rep
kb_store_last
	sta kb_last				; Make last code same as this
	clc						; Code valid
	rts
kb_in_repeat
	lda #0					; Don't emit a keycode
	sec
	rts
kb_brk
	SWBRK DFERR_OK

;****************************************
;* kb_get_key
;* Waits for a key press, C=1 synchronous
;* A = Key code, C=1 means valid
;****************************************
kb_get_key
	txa
	pha
	tya
	pha

kb_get_try
	php
	jsr kb_scan_key
	bcc kb_scan_got_key
	plp						; No key, so check C
	bcs kb_get_try			; Keep looking if C
	sec						; Indicate key not valid

	pla
	tay
	pla
	tax
	lda #0

	rts
kb_scan_got_key
	plp						; Pull stack
	clc						; Indicate key valid

	pla
	tay
	pla
	tax

	lda kb_code

	rts

;****************************************
;* kb_table_std (no shift)
;* Each line is one column
;****************************************
kb_table_std
	db '7' ,'j' ,'m' ,'k' ,' ' ,'u' ,'y' ,'8'
	db 'n' ,'t' ,'6' ,'9' ,',' ,'i' ,'h' ,'l'
	db '5' ,'r' ,'b' ,';' ,'.' ,'o' ,'g' ,'0'
	db 'v' ,'f' ,'4' ,'-' ,0x0b,'p' ,'e' ,'/'
	db 0,0,0,0,0,0,0,0 ; Column 4 is shift and ctrl - no codes
	db '1' ,0x1b,'z' ,0   ,0x08,0x7f,'a' ,0x0d
	db 'x' ,'q' ,'2' ,0x5c,0x0a,']' ,'s' ,0
	db '3' ,'d' ,'c' ,0x27,0x09,'[' ,'w' ,'='

;* kb_table_shift (with shift)
kb_table_shift
	db '&' ,'J' ,'M' ,'K' ,' ' ,'U' ,'Y' ,'*'
	db 'N' ,'T' ,'^' ,'(' ,'<' ,'I' ,'H' ,'L'
	db '%' ,'R' ,'B' ,':' ,'>' ,'O' ,'G' ,')'
	db 'V' ,'F' ,'$' ,'_' ,0x0b,'P' ,'E' ,'?'
	db 0,0,0,0,0,0,0,0 ; Column 4 is shift and ctrl - no codes
	db '!' ,0x1b,'Z' ,0   ,0x08,0x7f,'A' ,0x0d
	db 'X' ,'Q' ,'@' ,'|' ,0x0a,'}' ,'S' ,0
	db '#' ,'D' ,'C' ,0x22,0x09,'{' ,'W' ,'+'

kb_col_mask
	db 0b11111110
	db 0b11111101
	db 0b11111011
	db 0b11110111
	db 0b11101111
	db 0b11011111
	db 0b10111111
	db 0b01111111

kb_stick_mask
	db 0b11011111		; Left 	= Bit 0
	db 0b01111111		; Right = Bit 1
	db 0b11110111		; Up	= Bit 2
	db 0b10111111		; Down	= Bit 3
	db 0b11111110		; Space	= Bit 4

kb_ijk_map
	db 0b00000000		; 00000 = nothing
	db 0b00000010		; 00001 = right
	db 0b00000001		; 00010 = left
	db 0b00000011		; 00011 = left+right
	db 0b00010000		; 00100 = fire
	db 0b00010010		; 00101 = fire+right
	db 0b00010001		; 00110 = fire+left
	db 0b00010011		; 00111 = fire+left+right
	db 0b00001000		; 01000 = down
	db 0b00001010		; 01001 = down+right
	db 0b00001001		; 01010 = down+left
	db 0b00001011		; 01011 = down+left+right
	db 0b00011000		; 01100 = down+fire
	db 0b00011010		; 01101 = down+fire+right
	db 0b00011001		; 01110 = down+fire+left
	db 0b00011011		; 01111 = down+fire+left+right
	db 0b00000100		; 10000 = up
	db 0b00000110		; 10001 = up+right
	db 0b00000101		; 10010 = up+left
	db 0b00000111		; 10011 = up+left+right
	db 0b00010100		; 10100 = up+fire
	db 0b00010110		; 10101 = up+fire+right
	db 0b00010101		; 10110 = up+fire+left
	db 0b00010111		; 10111 = up+fire+left+right
	db 0b00001100		; 11000 = up+down
	db 0b00001110		; 11001 = up+down+right
	db 0b00001101		; 11010 = up+down+left
	db 0b00001111		; 11011 = up+down+left+right
	db 0b00011100		; 11100 = up+down+fire
	db 0b00011110		; 11101 = up+down+fire+right
	db 0b00011101		; 11110 = up+down+fire+left
	db 0b00011111		; 11111 = up+down+fire+left+right
	