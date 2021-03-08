;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  GRAPH.S
;*  This is the graphics module, to handle text and hires
;*  graphics.
;*  For text modes, this module keeps track of where to
;*  next put a character, and also takes care of wrapping
;*  to the next line as well as scrolling the contents up
;*  when the cursor has reached the bottom right.  This
;*  module also enables text input which is echoed to the
;*  screen, to allow interactive input and editing.
;*
;**********************************************************

	; ROM code
	code

mod_sz_graph_s


	include "vdp/font.s"

;****************************************
;* vdp_init_font
;* Initialise fonts from ROM
;* Input : None
;* Output : None
;* Regs affected : All
;****************************************
gr_init_font
	; Get char base from vdp structure
	ldx gr_scrngeom+gr_char
	stx tmp_blo
	ldx gr_scrngeom+gr_char+1
	inx						; ASCII patterns start 1 page later
	stx tmp_bhi

	lda #lo(vdp_font)		; Low byte of fonts source
	sta tmp_clo
	lda #hi(vdp_font)		; High byte of fonts source
	sta tmp_chi
	ldx #3					; Copy 3 pages
	jmp gr_copy_mem


;****************************************
;* vdp_copy_mem
;* Copy memory pages at a time
;* Input :	clo/hi	=	source
;*			blo/hi	=	dest
;*			X		=	pages to copy
;* Output : None
;* Regs affected : All
;****************************************
gr_copy_mem
	ldy #0					; byte within page
	lda tmp_clo				; Low byte of source
	sta tmp_alo
	lda tmp_chi				; High byte of source
	sta tmp_ahi
gr_copy_byte
	tya
	lda (tmp_alo),y			; Get byte from font table
	sta (tmp_blo),y			; Put it to the memory
	iny
	bne gr_copy_byte		; keep going for 1 page
	inc tmp_ahi				; only need to increment high byte of
	inc tmp_bhi				; source and destination ptr
	dex						; page counter
	bne gr_copy_byte		; keep going for X pages
	rts


;****************************************
;* gr_init
;* First initialisation of screen
;****************************************
gr_init
	ldx #VDP_FLASH			; Default cursor flash rate
	stx vdp_curtim
	stx vdp_curcnt
	ldx #0
	stx vdp_curstat

	; Hard reset initial geom values (X=0)
	jsr gr_init_geom

	; Copy font from ROM to char base
	jsr gr_init_font

	; Go in to hires mode to generate tables
	jsr gr_init_hires
	jsr gr_init_hires_tables

	; But start in text mode
	jmp gr_init_screen_txt


;****************************************
;* gr_check_font_copy
;* check font needs to be copied from
;* current location to another
;* A=high byte of page aligned font addr
;* that the font needs to be at
;****************************************
gr_check_font_copy
	; Compare with current high byte
	cmp gr_scrngeom+gr_char+1
	beq gr_check_font_copy_done
	; save A as high byte of destination
	sta tmp_blo+1
	; destination low is 0
	lda #0
	sta tmp_blo

	; Get source from current vdp structure
	lda gr_scrngeom+gr_char+1
	sta tmp_clo+1
	lda gr_scrngeom+gr_char
	sta tmp_clo

	; Copy all 8 pages of font (std + alt) data
	ldx #8
	jsr gr_copy_mem
gr_check_font_copy_done
gr_init_geom_done
	rts


;****************************************
;* gr_init_geom
;* initialise geometry entries from X
;* position from base until -1
;****************************************
gr_init_geom
	; Which geom entry to init = Y
	ldy gr_scrngeom_base,x
	bmi gr_init_geom_done		; Just somewhere with an rts!
	; skip to data and put in A
	inx
	lda gr_scrngeom_base,x
	; save it in Y
	sta gr_scrngeom,y
	; next entry
	inx
	bne gr_init_geom	; Always
	; Hope we don't get here - will crash!

; Geometry initialisation tables
gr_scrngeom_base
	db gr_mode, 0
	db gr_char,   lo(TEXTCHAR)
	db gr_char+1, hi(TEXTCHAR)
	db gr_ink, 0
	db gr_paper, 16+6
	db gr_margin, 2
	db -1
;* Geometry for text/lores
gr_scrngeom_text
	db gr_mode, 0

	db gr_char,   lo(TEXTCHAR)
	db gr_char+1, hi(TEXTCHAR)

	db gr_text_start,   lo(TEXTSCRN)
	db gr_text_start+1, hi(TEXTSCRN)

	db gr_text_size,   lo(40*28)
	db gr_text_size+1, hi(40*28)

	db gr_text_w, 40
	db gr_text_h, 28
	db -1
;* Geometry for hires
gr_scrngeom_hires
	db gr_mode, 1

	db gr_hi_start,   lo(HISCRN)
	db gr_hi_start+1, hi(HISCRN)
	db gr_char,   lo(HICHAR)
	db gr_char+1, hi(HICHAR)

	db gr_text_start,   lo(HITEXT)
	db gr_text_start+1, hi(HITEXT)

	db gr_text_size,   lo(40*3)
	db gr_text_size+1, hi(40*3)

	db gr_hires_x, 0
	db gr_hires_y, 0

	; Only need to initialise text height, width is same as before (iss: really? ;)
	db gr_text_w, 40
	db gr_text_h, 3

	db gr_pixmode, 1
	db gr_pitch, 6
	db -1



;****************************************
;* gr_init_screen_txt
;* initialise the screen in text mode
;****************************************
gr_init_screen_txt
	inc vdp_curoff

	; Check if font copy needed to 0xb400
	lda #hi(TEXTCHAR)
	jsr gr_check_font_copy

	; Initialise geom for text
	ldx #gr_scrngeom_text-gr_scrngeom_base
	jsr gr_init_geom

	jsr gr_spr_init					; SW Sprites are reset

	lda #' '						; Blank is SPACE
	sta vdp_blank
	jsr gr_cls

	dec vdp_curoff

	rts

;****************************************
;* gr_init_hires
;* Input : X = Colour table fill value
;* initialise the screen in hires mode
;****************************************
gr_init_hires
	inc vdp_curoff
	; Check if font copy needed to 0x9800
	lda #hi(HICHAR)
	jsr gr_check_font_copy

	; Initialise geom for hires
	ldx #gr_scrngeom_hires-gr_scrngeom_base
	jsr gr_init_geom

	lda #' '						; Blank is SPACE
	sta vdp_blank
	jsr gr_cls

	; Zero out hires area 0xa000 for 8000 (0x1f40) bytes
	lda #lo(HISCRN)
	sta tmp_alo
	lda #hi(HISCRN)
	sta tmp_ahi
	lda #0x40						; Set bit 6
	ldy #0
	ldx #0
vdp_fill_vram_loop
	sta (tmp_alo),y
	iny
	bne vdp_fill_vram_noinc
	inc tmp_ahi
	inx
vdp_fill_vram_noinc
	cpx #hi(0x1f40)			; Reached page count?
	bne vdp_fill_vram_loop
	cpy #lo(0x1f40)			; Reached byte in page?
	bne vdp_fill_vram_loop

	; Set hires attribute in the magic location
	lda #30			; Hires attribute
	sta 0xbfdf		; Last pos of screen memory

	dec vdp_curoff
	rts

;****************************************
;* gr_init_hires_tables
;* Generate the hires tables
;* Do this whilst in hires mode
;****************************************
gr_init_hires_tables
	; Initialise address pointer to start of high res screen
	lda #lo(HISCRN)
	sta tmp_alo
	lda #hi(HISCRN)
	sta tmp_ahi
	; Starting from row zero, do 200 rows
	ldy #0
gr_init_tab_row
	; Save low and high bytes of screen address in table
	lda tmp_alo
	sta hires_row_low,y
	lda tmp_ahi
	sta hires_row_hi,y
	; add 40 ready for next row
	clc
	lda tmp_alo
	adc #40
	sta tmp_alo
	lda tmp_ahi
	adc #0
	sta tmp_ahi
	; Max 200 rows
	iny
	cpy #200
	bne gr_init_tab_row
	; From column 0, do 240 columns
	; Mask starts at 0x20, when shifts to 0 then
	; the column increments
	lda #0x20
	sta tmp_blo			; Pixel mask
	ldx #0				; Pixel column
	ldy #0				; Byte column
gr_init_tab_col
	; Save current mask and column
	lda tmp_blo
	sta hires_mask,x
	tya
	sta hires_col,x
	; shift mask, if zero then increment col and reset mask
	lsr tmp_blo
	bne gr_init_tab_col_skip
	lda #0x20
	sta tmp_blo
	iny
gr_init_tab_col_skip
	inx
	cpx #240
	bne gr_init_tab_col
	rts



;****************************************
;* gr_cls
;* Clear the text screen
;****************************************
gr_cls
	inc vdp_curoff
	; Set screen address for text mode
	lda gr_scrngeom+gr_text_start
	sta gr_scrngeom+gr_geom_tmp
	lda gr_scrngeom+gr_text_start+1
	sta gr_scrngeom+gr_geom_tmp+1

	ldx gr_scrngeom+gr_text_h		; Count of rows to clear

	; X and Y count bytes to fill
gr_cls_row
	ldy #0							; Count of columns
	lda gr_scrngeom+gr_margin		; Is there a margin?
	beq gr_cls_skip_marg
	lda gr_scrngeom+gr_paper		; Set Paper
	sta (gr_scrngeom+gr_geom_tmp),y
	iny
	lda gr_scrngeom+gr_ink			; Set Ink
	sta (gr_scrngeom+gr_geom_tmp),y
	iny
gr_cls_skip_marg
	lda vdp_blank
	sta vdp_curval					; Under cursor is also blank
gr_cls_col
	sta (gr_scrngeom+gr_geom_tmp),y
	iny
	cpy gr_scrngeom+gr_text_w		; Done all columns?
	bne gr_cls_col
	; Update pointer
	clc
	lda gr_scrngeom+gr_geom_tmp
	adc gr_scrngeom+gr_text_w
	sta gr_scrngeom+gr_geom_tmp
	lda gr_scrngeom+gr_geom_tmp+1
	adc #0
	sta gr_scrngeom+gr_geom_tmp+1
	dex								; 1 row done
	bne gr_cls_row					; Done all rows?

	; set cursror position to top left
	lda vdp_blank
	sta vdp_curval
	ldx gr_scrngeom+gr_margin
	ldy #0
	sec								; Init mode
	jsr gr_set_cur_init

	dec vdp_curoff

	rts

;****************************************
;* gr_getXY_base
;* Get base screen address using Y coord only
;* Input : X = y coord
;* Output :	gr_geom_tmp updated with base
;* Regs affected : A
;****************************************
gr_getXY_base
	; Find y offset using **X register**
	clc
	lda gr_offset_40lo,x
	adc gr_scrngeom+gr_text_start
	sta gr_scrngeom+gr_geom_tmp
	lda gr_offset_40hi,x
	adc gr_scrngeom+gr_text_start+1
	sta gr_scrngeom+gr_geom_tmp+1

	; A = high byte of base address
	rts

;****************************************
;* gr_plot
;* Write a byte in the screen pos
;* Input :	Y,X = coord, A = Byte to put
;*			X = Y COORDINATE
;*			Y = X COORDINATE!!
;* Output : None
;* Regs affected : All
;****************************************
gr_plot
	pha					; Save byte to put
	jsr gr_getXY_base	; base addre
	pla					; Get byte to put
	sta (gr_scrngeom+gr_geom_tmp),y
	rts

;****************************************
;* gr_put
;* Write a byte in the current cursor position
;* Input : A = Byte to put
;* Output : None
;* Regs affected : All
;****************************************
gr_put
	inc vdp_curoff		; Disable cusror
	sta vdp_curval		; Update cursor value
	; Base address plus X offset
	ldy gr_scrngeom+gr_cur_x
	sta (gr_scrngeom+gr_cur_ptr),y
	dec vdp_curoff		; Allow cursor flashing
	rts


;****************************************
;* gr_get
;* Get the byte in the screen pos
;* Input : Y,X = coord (**Y = X coordinate!!)
;* Output : X,Y = address, A = peeked byte
;* Regs affected : All
;****************************************
gr_get
	jsr gr_getXY_base	; Base address
	lda (gr_scrngeom+gr_geom_tmp),y
	rts

;****************************************
;* gr_set_cur
;* Set the cursor position
;* Input : X, Y = position
;* Output : None
;* Regs affected : None
;****************************************
gr_set_cur
	clc
gr_set_cur_init
	inc vdp_curoff				; Disable cursor

	bcs gr_set_cur_skip			; Skip restore if C=1
	stx tmp_alo
	sty tmp_ahi
	; First restore what is under the cursor
	; in case cursor is ON
	sty gr_scrngeom+gr_geom_tmp
	lda vdp_curval
	ldy gr_scrngeom+gr_cur_x
	sta (gr_scrngeom+gr_cur_ptr),y
	ldy gr_scrngeom+gr_geom_tmp
gr_set_cur_skip
	; Save new cursor position
	stx gr_scrngeom+gr_cur_x
	sty gr_scrngeom+gr_cur_y

	; Now calculate the new cursor vram address
	ldx gr_scrngeom+gr_cur_y
	jsr gr_getXY_base
	; Update pointer in tmp, A already has high byte
	sta gr_scrngeom+gr_cur_ptr+1
	lda gr_scrngeom+gr_geom_tmp
	sta gr_scrngeom+gr_cur_ptr
	; Read screen at this position, offset with X coord
	ldy gr_scrngeom+gr_cur_x
	lda (gr_scrngeom+gr_cur_ptr),y
	sta vdp_curval
	; Reset cursor so it's visible
	lda #0
	sta vdp_curstat
	lda #1
	sta vdp_curcnt

	dec vdp_curoff

	rts



;****************************************
;* gr_scroll_up
;* Scroll screen one line up
;****************************************
gr_scroll_up
	inc vdp_curoff

	; Set source in a and dest in b
	; b is first line, source is second
	clc
	lda gr_scrngeom+gr_text_start
	sta tmp_blo
	adc gr_scrngeom+gr_text_w
	sta tmp_alo
	lda gr_scrngeom+gr_text_start+1
	sta tmp_bhi
	adc #0
	sta tmp_ahi

	; Restore what was underneath cursor
	lda vdp_curval
	ldy gr_scrngeom+gr_cur_x
	sta (gr_scrngeom+gr_cur_ptr),y

	; x = lines to scroll (1 less than screen height)
	ldx gr_scrngeom+gr_text_h
	dex

gr_scroll_cpy_ln
	; Start from right hand edge (easier)
	ldy gr_scrngeom+gr_text_w
	dey
gr_scroll_char
	; get char from source and copy to dest
	lda (tmp_alo),y
	sta (tmp_blo),y
	dey
	bpl gr_scroll_char

	; Update source address
	clc
	lda tmp_alo
	adc gr_scrngeom+gr_text_w
	sta tmp_alo
	lda tmp_ahi
	adc #0
	sta tmp_ahi
	; Update destination address
	clc
	lda tmp_blo
	adc gr_scrngeom+gr_text_w
	sta tmp_blo
	lda tmp_bhi
	adc #0
	sta tmp_bhi

	; One line complete
	dex
	bne gr_scroll_cpy_ln

	; Source is pointing at last line
	; Last line needs filling
	ldy #0							; Start from left
	lda gr_scrngeom+gr_margin		; Margin?
	beq gr_scroll_marg				; Skip if not
	lda gr_scrngeom+gr_paper		; Set Paper
	sta (gr_scrngeom+gr_geom_tmp),y
	iny
	lda gr_scrngeom+gr_ink			; Set Ink
	sta (gr_scrngeom+gr_geom_tmp),y
	iny
gr_scroll_marg
	; Needs to be filled with blank
	lda vdp_blank
	sta vdp_curval			; Also this is the cursor value
gr_scroll_erase_ln
	sta (tmp_blo),y
	iny
	cpy gr_scrngeom+gr_text_w
	bne gr_scroll_erase_ln

	dec vdp_curoff

	rts

;****************************************
;* gr_new_ln
;* Carry out a new line
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
gr_new_ln
	; X pos is zero, Y needs to increment
	ldx gr_scrngeom+gr_margin
	ldy gr_scrngeom+gr_cur_y
	iny
	cpy gr_scrngeom+gr_text_h
	bne gr_nl_skip_nl
	; If got here then screen needs to scroll
	; Common routine also used by cursor right
gr_scroll_routine
	dey
	txa
	pha
	tya
	pha
	jsr gr_scroll_up
	pla
	tay
	pla
	tax
gr_nl_skip_nl
	jmp gr_set_cur


;****************************************
;* gr_cur_right
;* Advance cursor position
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
gr_cur_right
	; Load cursor x,y position
	ldx gr_scrngeom+gr_cur_x
	ldy gr_scrngeom+gr_cur_y

	; Move cursor right
	inx
	; Check if reached past edge of line
	cpx gr_scrngeom+gr_text_w
	bne gr_nl_skip_nl
	; If got here then wrap to next line
	ldx gr_scrngeom+gr_margin
	iny
	cpy gr_scrngeom+gr_text_h
	beq gr_scroll_routine		; Common scroll routine
	bne gr_nl_skip_nl			; Common set cursor

;****************************************
;* gr_cur_left
;* Advance cursor left
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
gr_cur_left
	; Load cursor x,y position, load X last to check for 0
	ldy gr_scrngeom+gr_cur_y
	ldx gr_scrngeom+gr_cur_x

	; Decrement screen pointer
	; Move cursor left
	cpx gr_scrngeom+gr_margin	; Already at left margin?
	bne gr_cur_skip_at_left		; No, then just go left
	cpy #0						; Else check if can wrap up
	beq gr_cur_skip_at_tl
	dey
	ldx gr_scrngeom+gr_text_w
gr_cur_skip_at_left
	dex
	jmp gr_set_cur

gr_cur_skip_at_tl
	rts

;****************************************
;* gr_cur_up
;* Advance cursor up
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
gr_cur_up
	; Load cursor x,y position, load Y last to check for zero
	ldx gr_scrngeom+gr_cur_x
	ldy gr_scrngeom+gr_cur_y
	; if y==0 then don't do anything
	beq gr_cur_skip_at_tl	; Just somewhere with an rts!
	dey
	jmp gr_set_cur

;****************************************
;* gr_cur_down
;* Advance cursor down
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
gr_cur_down
	; Load cursor x,y position
	ldx gr_scrngeom+gr_cur_x
	ldy gr_scrngeom+gr_cur_y
	iny
	; If already at  bottom then don't do anything
	cpy gr_scrngeom+gr_text_h
	beq gr_cur_skip_at_tl				; Just somewhere with an rts!
	jsr gr_set_cur

gr_cur_skip_at_bot
	rts


;****************************************
;* gr_del
;* Action del
;* Input : None
;* Output : None
;* Regs affected : None
;****************************************
gr_del
	jsr gr_cur_left
	lda #' '							; Put a space
	jmp gr_put

;****************************************
;* gr_get_key
;* Waits for a key press, C=1 synchronous
;* A = Key code, C=0 means valid code
;****************************************
gr_get_key
	; save X,Y but A is ok to trample
	txa
	pha
	tya
	pha
gr_get_key_2
	php
	jsr kb_get_key
	bcc gr_key_check_key
	plp									; Get async pref
	bcs	gr_get_key_2					; Keep checking if sync
	sec
	bcs gr_key_tidy_up					; Finish if not
gr_key_check_key						; Check the key pressed
	cmp #UTF_ACK						; Copy key pressed?
	bne gr_key_skip_copy
	lda vdp_curval						; If yes the get char under cursor
	bcs gr_key_got_key					; Always branches (n=1, c=1)
gr_key_skip_copy
	cmp #CTRL_CAPS
	bne gr_key_got_key
	lda kb_stat							; Toggle caps bit
	eor #KB_CAPSLK
	sta kb_stat
	plp									; Get back synchronous pref
	bcs gr_get_key_2					; And check again for sync
	bcc gr_key_no_key					; Else no key
gr_key_got_key
	plp
	clc									; Ensure C=0 for valid key
gr_key_tidy_up
	; restore X,Y but don't lose A
	sta tmp_d
	pla
	tay
	pla
	tax
	lda tmp_d
	rts
gr_key_no_key
	plp
	sec									; Ensure C=1 for invalid key
	bcs gr_key_tidy_up					; Always branches

;****************************************
;* gr_put_byte
;* Put a byte out
;* Input : A = Byte to put
;* Output : None
;* Regs affected : None
;****************************************
gr_put_byte
	_pushAXY
	jsr gr_put_byte_low
	_pullAXY
gr_no_special
	rts

gr_put_byte_low
	cmp #UTF_DEL			; Del key
	beq gr_process_special
	cmp #32					; Special char?
	bcs gr_printable		; >=32 == carry clear
gr_process_special
	; Else find special behaviour to do
	ldx #-1
	sta tmp_alo
gr_special_loop
	inx
	lda gr_special_ch,x
	beq gr_no_special		; Somewhere with an rts!
	cmp tmp_alo
	bne gr_special_loop
	lda gr_special_fn_lo,x
	sta tmp_alo
	lda gr_special_fn_hi,x
	sta tmp_ahi
	jmp (tmp_alo)

	;	Normal caracter processing here.
gr_printable
	; Place in current position and move right
	jsr gr_put
	jmp gr_cur_right

gr_special_ch
	db UTF_CR
	db UTF_DEL
	db CRSR_LEFT
	db CRSR_RIGHT
	db CRSR_UP
	db CRSR_DOWN
	db UTF_FF
	db UTF_BEL
	db 0

gr_special_fn_lo
	db lo(gr_new_ln)
	db lo(gr_del)
	db lo(gr_cur_left)
	db lo(gr_cur_right)
	db lo(gr_cur_up)
	db lo(gr_cur_down)
	db lo(gr_cls)
	db lo(init_snd)

gr_special_fn_hi
	db hi(gr_new_ln)
	db hi(gr_del)
	db hi(gr_cur_left)
	db hi(gr_cur_right)
	db hi(gr_cur_up)
	db hi(gr_cur_down)
	db hi(gr_cls)
	db hi(init_snd)

; Special command to print to the screen
; Y,A=Message, zero terminated
gr_print_msg
	sty num_tmp
	sta num_tmp+1
	ldy #0
gr_print_msg_loop
	lda (num_tmp),y
	beq gr_print_msg_done
	jsr gr_put_byte
	iny
	bne gr_print_msg_loop
gr_print_msg_done
	lda num_tmp+1
	ldy num_tmp
	rts

;******* HIRES STUFF *****

;****************************************
;* gr_fill
;* Fill bytes X,Y coordinates with char code A
;* Input : X,Y = coord, A = Char code
;* Output : None
;* Regs affected : None
;****************************************

;****************************************
;* gr_hchar
;* Plot character to hires X,Y coordinates with char code A
;* Input : X,Y = coord, A = Char code
;* Output : None
;* Regs affected : None
;****************************************
gr_hchar
	; Multiply char code by 8
	; and add to char font base
	; tmp_clo contains base address
	asl a
	rol tmp_chi
	asl a
	rol tmp_chi
	asl a
	rol tmp_chi
	clc
	adc gr_scrngeom+gr_char
	sta tmp_clo
	lda tmp_chi
	and #7
	adc gr_scrngeom+gr_char+1
	sta tmp_chi

	; Set up destination position
	jsr gr_point_setup
	; tmp_alo contains address including column offset
	clc
	tya
	adc tmp_alo
	sta tmp_alo
	lda tmp_ahi
	adc #0
	sta tmp_ahi

	lda tmp_blo					; Get the mask
	ldx #7
gr_hchar_mask					; Calculate how many shifts to tmp_blo
	dex
	lsr a
	bne gr_hchar_mask
	stx tmp_blo					; number between 1 and 6 : shift n-1 times

	; copy font bytes and shift the required number of times
	; go from bottom to top as data gets stored on the stack!
	ldy #7
gr_hchar_getfont
	lda (tmp_clo),y
	sta ztmp_24
	lda #0
	sta ztmp_24+1

	; shift right number of times
	ldx tmp_blo
gr_hchar_rot1bit
	dex
	beq gr_hchar_rot1bit_nx
	lsr ztmp_24					; Rotate left hand side
	lda ztmp_24+1				; Rotate right hand side
	bcc gr_hchar_rot1bit_bcc
	ora #0x40					; account for 6 bits per byte
gr_hchar_rot1bit_bcc
	lsr a
	sta ztmp_24+1
	bpl gr_hchar_rot1bit		; Always as lsr sets N=0
gr_hchar_rot1bit_nx
	lda ztmp_24+1				; Get RHS
	pha							; Push RHS on to stack
	lda ztmp_24					; Get LHS
	pha							; Push that too - LH gets pulled first
	dey							; Bottom to to lines
	bpl gr_hchar_getfont

	; Now copy shift source to destination, accounting for pixmode
	ldx #0
gr_hchar_copyline
	ldy gr_scrngeom+gr_pixmode	; Mode determines how to modify
	beq gr_hchar_copyline_0
	cpy #2
	beq gr_hchar_copyline_2

	; Mode = 1 : OR
	ldy #0						; Get lh side source
	pla
	ora (tmp_alo),y
	sta (tmp_alo),y
	iny							; Get rh side source
	pla
	ora (tmp_alo),y
	sta (tmp_alo),y
	jmp gr_hchar_copyline_nx
gr_hchar_copyline_2
	; Mode = 2 : EOR
	ldy #0						; Get lh side source
	pla
	eor (tmp_alo),y
	sta (tmp_alo),y
	iny							; Get rh side source
	pla
	eor (tmp_alo),y
	sta (tmp_alo),y
	jmp gr_hchar_copyline_nx
gr_hchar_copyline_0
	; Mode = 0 : erase
	ldy #0						; Get lh side source
	pla
	sta ztmp_24
	ora (tmp_alo),y
	eor ztmp_24
	sta (tmp_alo),y
	iny							; Get rh side source
	pla
	sta ztmp_24
	ora (tmp_alo),y
	eor ztmp_24
	sta (tmp_alo),y
gr_hchar_copyline_nx
	clc							; Next address
	lda tmp_alo
	adc #40
	sta tmp_alo
	lda tmp_alo+1
	adc #0
	sta tmp_alo+1
	inx
	cpx #8
	bne gr_hchar_copyline
	rts							; Done after 8 lines

;****************************************
;* gr_point_setup
;* Calculate information about a pixel location
;* Input : X,Y = coord
;* Output : None
;* Regs affected :
;* tmp_alo,hi contains the row base address
;* tmp_blo contains the mask index
;* tmp_bhi contains the column offset in to row
;* Y is same as tmp_bhi
;* X is same as tmp_blo
;****************************************
gr_point_setup
	; Get row address
	lda hires_row_low,y
	sta tmp_alo
	lda hires_row_hi,y
	sta tmp_ahi
	; Get the pixel mask
	lda hires_mask,x
	sta tmp_blo
	; Get the column offset to Y
	ldy hires_col,x
	rts

gr_set_hires_cur
	stx gr_scrngeom+gr_hires_x
	sty gr_scrngeom+gr_hires_y
	rts

;* Get pixel value at X,Y in to A
gr_pixel
	jsr gr_point_setup				; Set up mask and addresses, Y=column, X=rem
	lda (tmp_alo),y					; Get screen byte
	and tmp_blo						; Check if pixel coincides with mask
	rts

;* Plot a point based on X,Y coordinates
gr_point
	cpx #240						; Check bounds
	bcs gr_point_done
	cpy #200
	bcs gr_point_done

	;** FOR SPEED COPYING THE POINT SETUP ROUTINE
	; Get row address
	lda hires_row_low,y
	sta tmp_alo
	lda hires_row_hi,y
	sta tmp_ahi
	; Get the pixel mask
	lda hires_mask,x
	sta tmp_blo
	; Get the column offset to Y
	ldy hires_col,x

;	jsr gr_point_setup				; Set up mask and addresses, Y=column, X=rem
;* Plot a point based on tmp_alo base, Y offset and X index mask
	lda (tmp_alo),y					; Get screen byte
	ldx gr_scrngeom+gr_pixmode		; Look at the mode
	cpx #2							; If eor mode then go and write
	beq gr_point_eor
	ora tmp_blo						; Or with MASK
	cpx #0							; But if not then eor
	bne gr_point_write
gr_point_eor
	eor tmp_blo						; EOR with MASK
gr_point_write
	sta (tmp_alo),y
gr_point_done
	rts


;****************************************
;* gr_circle
;* Draw a circle centre x0,y0, radius r
;* Input :	num_a   = x0
;*			num_a+1 = y0
;*			num_a+2 = r
;* Output : None
;* Regs affected : None
;****************************************
gr_circle
; Local definitions of temp space to make
; the rest of the code easier to read
grc_x0 	= (num_a)
grc_y0 	= (num_a+1)
grc_r 	= (num_a+2)
grc_x 	= (num_a+3)
grc_y	= (num_b+1)
grc_d	= (num_b+2)

	;x = radius
	lda grc_r
	sta grc_x
	;decision = 1 - x
	lda #0
	;y = 0
	sta grc_y
	clc			; A=0 so CLC subtracts 1 :-)
	sbc grc_x
	sta grc_d
gr_circle_plot
	;while(x >= y)
	lda grc_x
	cmp grc_y
	bcc gr_circle_done
	;plot 8 points on current x,y
	jsr gr_circle_points
	;y++
	inc grc_y
	;if d<=0
	lda grc_d
	beq gr_circle_d_lte0
	bmi gr_circle_d_lte0
	;else
	;x--
	dec grc_x
	;decision += 2 * (y - x) + 1
	lda grc_y
	sec
	sbc grc_x
	asl a
	clc
	adc #1
	adc grc_d
	sta grc_d
	jmp gr_circle_plot
gr_circle_d_lte0
	;decision += 2 * y + 1
	lda grc_y
	asl a
	clc
	adc #1
	adc grc_d
	sta grc_d
	jmp gr_circle_plot
gr_circle_done
	rts
gr_circle_points
; Local names of temp storage
; to make code easier to read
	;DrawPixel( x + x0,  yh + y0);
	lda grc_x
	clc
	adc grc_x0
	tax
	lda grc_y
	clc
	adc grc_y0
	tay
	jsr gr_point
	;DrawPixel( y + x0,  xh + y0);
	lda grc_y
	clc
	adc grc_x0
	tax
	lda grc_x
	clc
	adc grc_y0
	tay
	jsr gr_point
	;DrawPixel(-x + x0,  yh + y0);
	lda grc_x0
	sec
	sbc grc_x
	tax
	lda grc_y
	clc
	adc grc_y0
	tay
	jsr gr_point
	;DrawPixel(-y + x0,  xh + y0);
	lda grc_x0
	sec
	sbc grc_y
	tax
	lda grc_x
	clc
	adc grc_y0
	tay
	jsr gr_point
	;DrawPixel(-x + x0, -yh + y0);
	lda grc_x0
	sec
	sbc grc_x
	tax
	lda grc_y0
	sec
	sbc grc_y
	tay
	jsr gr_point
	;DrawPixel(-y + x0, -xh + y0);
	lda grc_x0
	sec
	sbc grc_y
	tax
	lda grc_y0
	sec
	sbc grc_x
	tay
	jsr gr_point
	;DrawPixel( x + x0, -yh + y0);
	lda grc_x
	clc
	adc grc_x0
	tax
	lda grc_y0
	sec
	sbc grc_y
	tay
	jsr gr_point
	;DrawPixel( y + x0, -xh + y0);
	lda grc_y
	clc
	adc grc_x0
	tax
	lda grc_y0
	sec
	sbc grc_x
	tay
	jmp gr_point



;****************************************
;* gr_line
;* Draw a line from x0,y0 -> x1,y1
;* Input :	num_a   = x0
;*			num_a+1 = y0
;*			num_a+2 = x1
;*			num_a+3 = y1
;* Output : None
;* Regs affected : None
;****************************************
gr_line

; Local definitions of temp space to make
; the rest of the code easier to read
grl_x0 	= (num_a)
grl_y0 	= (num_a+1)
grl_x1 	= (num_a+2)
grl_y1 	= (num_a+3)
grl_dx	= (ztmp_24+4)
grl_dy	= (ztmp_24+5)
grl_xyyx= (ztmp_24+6)
grl_2dx	= (ztmp_24+7)			; Word
grl_2dy	= (ztmp_24+9)			; Word
grl_2dxy= (ztmp_24+11)			; Word
grl_sinx= (ztmp_24+13)
grl_p	= (ztmp_24+14)			; Word
grl_siny= (ztmp_24+16)

	; Start from hires cursor position
	; New cursor pos = end of line pos
	lda gr_scrngeom+gr_hires_x
	sta grl_x0
	lda gr_scrngeom+gr_hires_y
	sta grl_y0
	ldx grl_x1
	ldy grl_y1
	jsr gr_set_hires_cur

	lda #0
	sta grl_xyyx				; Assume normal xy axis

	; check if abs(dy)>abs(dx) if so need to swap xy
	; num_b = abs(x), num_b+1 = abs(dy)
	sec
	lda grl_x1
	sbc grl_x0
	bcs gr_line_skip_dx_neg
	eor #0xff
	adc #1
gr_line_skip_dx_neg
	sta grl_dx
	sec
	lda grl_y1
	sbc grl_y0
	bcs gr_line_skip_dy_neg
	eor #0xff
	adc #1
gr_line_skip_dy_neg
	sta grl_dy
	cmp grl_dx
	bcc gr_line_skip_xy_swap
	; swap xy axes and also dx and dy
	lda grl_x0					; swap x0 and y0
	ldx grl_y0
	sta grl_y0
	stx grl_x0
	lda grl_x1					; swap x1 and y1
	ldx grl_y1
	sta grl_y1
	stx grl_x1
	lda grl_dx					; swap dy and dx
	ldx grl_dy
	sta grl_dy
	stx grl_dx
	inc grl_xyyx				; set flag to Not Z to know about axis change

gr_line_skip_xy_swap
	; assume going from left to right and top to bottom
	lda #1
	ldy #255					; -1
	sta grl_sinx
	sta grl_siny

	; check going right to left
	lda grl_x0
	cmp grl_x1
	bcc gr_line_skip_left
	sty grl_sinx				; make -1
gr_line_skip_left
	; check going bottom to top
	lda grl_y0
	cmp grl_y1
	bcc gr_line_skip_y_up
	sty grl_siny				; make -1

gr_line_skip_y_up
	lda grl_dx
	asl a
	sta grl_2dx					; 2*dx (word)
	lda #0
	sta grl_2dx+1
	rol grl_2dx+1

	lda grl_dy
	asl a
	sta grl_2dy					; 2*dy (word)
	lda #0
	sta grl_2dy+1
	rol grl_2dy+1

;    p=2*dy-dx;					; p (word)
	sec
	lda grl_2dy
	sbc grl_dx
	sta grl_p
	lda grl_2dy+1
	sbc #0
	sta grl_p+1

;   2*(dy-dx)					; num_tmp+2 = 2*(dy-dx)
	sec
	lda grl_2dy
	sbc grl_2dx
	sta grl_2dxy
	lda grl_2dy+1
	sbc grl_2dx+1
	sta grl_2dxy+1

gr_line_pixel
	ldx grl_x0
	ldy grl_y0
	lda grl_xyyx				; Swapped?
	beq gr_line_yx_skip
	ldx grl_y0
	ldy grl_x0
gr_line_yx_skip
	jsr gr_point				; Plot point x,y

	lda grl_x0					; Check if done
	cmp grl_x1
	beq gr_line_done

	; Increment x always
	clc
	lda grl_x0
	adc grl_sinx
	sta grl_x0

	; check sign of p
	lda grl_p+1
	bmi gr_line_neg_p

	; if p >=0

	; y=y+increment
	clc
	lda grl_y0
	adc grl_siny
	sta grl_y0

	; p=p+2*dy-2*dx
	_addZPWord grl_p,grl_2dxy
	jmp gr_line_pixel

gr_line_neg_p
	; if p < 0
	; p=p+2*dy
	_addZPWord grl_p,grl_2dy

	jmp gr_line_pixel
gr_line_done
	rts


; Line drawing pseudo code
;    while(x<x1)
;    {
;        putpixel(x,y);
;        if(p>=0)
;        {
;            y=y+1;
;            p=p+2*dy-2*dx;
;        }
;        else
;        {
;            p=p+2*dy;
;        }
;        x=x+1;
;    }


;* These tables are to speed up calculating the
;* offset for plot commands
gr_offset_40lo
	db lo(000*40), lo(001*40), lo(002*40), lo(003*40), lo(004*40)
	db lo(005*40), lo(006*40), lo(007*40), lo(008*40), lo(009*40)
	db lo(010*40), lo(011*40), lo(012*40), lo(013*40), lo(014*40)
	db lo(015*40), lo(016*40), lo(017*40), lo(018*40), lo(019*40)
	db lo(020*40), lo(021*40), lo(022*40), lo(023*40), lo(024*40)
	db lo(025*40), lo(026*40), lo(027*40)
gr_offset_40hi
	db hi(000*40), hi(001*40), hi(002*40), hi(003*40), hi(004*40)
	db hi(005*40), hi(006*40), hi(007*40), hi(008*40), hi(009*40)
	db hi(010*40), hi(011*40), hi(012*40), hi(013*40), hi(014*40)
	db hi(015*40), hi(016*40), hi(017*40), hi(018*40), hi(019*40)
	db hi(020*40), hi(021*40), hi(022*40), hi(023*40), hi(024*40)
	db hi(025*40), hi(026*40), hi(027*40)


;* Character based sprites for text mode only
mod_sz_sprite_s
;* Initialisation
gr_spr_init
	ldx #31							; Start at last sprite
gr_spr_init_loop
	lda #-1							; Put -1 in the x coords
	sta spr_curX,x
	sta spr_newX,x
	dex								; Next sprite
	bpl gr_spr_init_loop			; Until all 32 sprites initialised
	rts


;* Erase all active sprites
gr_spr_erase
	; First restore background from sprites
	; that are active and new pos is different from current
	ldx #31							; Start at last sprite
gr_spr_erase_loop
	lda spr_curX,x					; Is sprite active?
	bmi gr_spr_erase_next
	sta tmp_alo						; x pos saved for later
gr_spr_erase_do
	lda spr_bgnd,x					; Get the background
	jsr gr_spr_put					; And restore it
gr_spr_erase_next
	dex
	bpl gr_spr_erase_loop
	rts

;* Get ready for new position
gr_spr_new
	ldx #31							; Start at last sprite
gr_spr_new_loop
	lda spr_newX,x					; Is the new position active?
	bmi gr_spr_new_next
	sta tmp_alo						; x pos saved for later
gr_spr_new_pos
	lda spr_newY,x					; Get new Y
	sta spr_curY,x					; Update new->cur Y
	; Calculate screen address
	; save as part of sprite data and in zp area
	tay
	clc
	lda gr_offset_40lo,y
	adc #lo(TEXTSCRN)
	sta gr_scrngeom+gr_geom_tmp
	sta spr_baseadrl,x
	lda gr_offset_40hi,y
	adc #hi(TEXTSCRN)
	sta gr_scrngeom+gr_geom_tmp+1
	sta spr_baseadrh,x
	ldy tmp_alo						; Get x pos back in to Y reg
	tya
	sta spr_curX,x					; Update new->cur X
	lda (gr_scrngeom+gr_geom_tmp),y	; Get background
	sta spr_bgnd,x					; And save this
gr_spr_new_next
	dex
	bpl gr_spr_new_loop
	rts


; Draw all active sprites
; Active sprites are always drawn - 0 = lowest priority
gr_spr_draw
	ldx #31							; Start at last sprite
gr_spr_draw_loop
	lda spr_newX,x					; Is sprite active?
	sta tmp_alo
	bmi gr_spr_draw_next
	lda spr_chr,x					; Get the sprite char
	jsr gr_spr_put
gr_spr_draw_next
	dex
	bpl gr_spr_draw_loop
	rts


;* Common routine to put A to screen address
;* Used for erase and draw of sprites
gr_spr_put
	pha
	lda spr_baseadrl,x		; Get the screen pointer
	sta gr_scrngeom+gr_geom_tmp
	lda spr_baseadrh,x
	sta gr_scrngeom+gr_geom_tmp+1
	ldy tmp_alo						; Y reg is in tmp_alo
	pla								; Get back the char to
	sta (gr_scrngeom+gr_geom_tmp),y	; put on to screen
	rts


;* Update spr A with char X
gr_spr_char
	tay
	txa
	sta spr_chr,y
	rts

;* Locate what is at the location of sprite A, returned in A
;* C=1 if sprite it not active
gr_spr_hit
	tax
	lda spr_curX,x
	cmp #0x80						; If A>=0x80 then C=1
	lda spr_bgnd,x
	rts

;* Update spr A with coords X,Y
gr_spr_pos
	sty tmp_alo
	tay
	txa								; X coord still in X reg
	sta spr_newX,y
	lda tmp_alo						; Y coord from temp
	sta spr_newY,y
	rts

;* Update all sprite positions from df_tmpptra, df_tmpptrb
gr_spr_multi_pos
	ldy #62
	ldx #31
gr_spr_multi_loop
	lda (df_tmpptra),y
	sta spr_newX,x
	lda (df_tmpptrb),y
	sta spr_newY,x
	dey
	dey
	dex
	bpl gr_spr_multi_loop
	rts

mod_sz_sprite_e
mod_sz_graph_e

; Old version of point calculator - in case I need it again!
;	; Calculate destination address
;	lda #0
;	sta tmp_ahi
;
;	tya				; Row number in A
;	; Multiply 8
;	asl a
;	rol tmp_ahi
;	asl a
;	rol tmp_ahi
;	asl a
;	rol tmp_ahi
;	sta tmp_alo
;
;	; Multiply 32
;	; Use partial result from m8
;	lda tmp_ahi
;	sta tmp_bhi
;	lda tmp_alo
;	; Just two more rotates to get m32
;	asl a
;	rol tmp_bhi
;	asl a
;	rol tmp_bhi
;	sta tmp_blo
;	; Add m8 and m32 for m40
;	clc
;	lda tmp_alo
;	adc tmp_blo
;	sta tmp_alo
;	lda tmp_ahi
;	adc tmp_bhi
;	; Add high byte of screen address
;	adc gr_scrngeom+vdp_scrn+1
;	sta tmp_ahi
;	; Dest offset in tmp_alo
;
;	; pixel x coord in to A
;	; We dvide by 48 doing trial subtracts
;	; This leaves Y with a number 0..5
;	; And A has the remainder 0..47
;	; Use these to look up byte column and mask
;	; We don't do more than 5 trial subtracts
;	; plus the table size is much smaller
;	; Comprimise of space vs speed
;	txa
;	ldy #0				; Start at segment 0
;	sec
;gr_point_d48
;	sbc #48
;	iny
;	bcs gr_point_d48				; Keep going until underflow
;
;	dey								; Adjust segment count
;	adc #48							; Get remainder in A
;	tax								; Put remainder in to X (0..47)
;	lda gr_col_seg,y				; Get the start column of segment in Y
;	clc
;	adc gr_col_offset,x				; Add segment offset using remainder
;	tay								; We have the byte column in Y
;	sty tmp_bhi						; Save in tmp, also still in Y
;	stx tmp_blo						; Save remainder 0..47
;	rts

;gr_point_mask
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;	db 0x20,0x10,0x08,0x04,0x02,0x01
;gr_col_offset
;	db 0,0,0,0,0,0
;	db 1,1,1,1,1,1
;	db 2,2,2,2,2,2
;	db 3,3,3,3,3,3
;	db 4,4,4,4,4,4
;	db 5,5,5,5,5,5
;	db 6,6,6,6,6,6
;	db 7,7,7,7,7,7
;gr_col_seg
;	db 0,8,16,24,32
