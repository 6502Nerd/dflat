;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  VAR.S
;*  This module handles all the variable management in dflat.
;*  When a new variable is detected during tokenisation, it
;*  is added to the variable tables.  Any subsequent used of
;*  that variable is tokenised as an index in to the variable
;*  table.  There are two variable tables:
;*  Variable name table (VNT) keeps track of variable names
;*  Variable value table (VVT) maintains variable properties
;*  including type, dimension (if array) and of course the
;*  actual values.  For an array, the value is a pointer to
;*  memory grabbed using the 'malloc' function (see stack.s).
;*  This approach to variable managemet is directly from the
;*  Atari 8 bit.  The disadvantage is that during a big edit
;*  session you may end up having a much larger variable
;*  table than you need.  Why?  Well because say you enter
;*  a as a new variable, but then later change it to b.
;*  In this case a remains in the variable tables - dflat
;*  only ever adds to the table!  However it is easily
;*  solved - when you save and then reload from new a
;*  program, the variable table is built up as the program
;*  is loaded.
;*  The VNT grows DOWN from the top of free memory, with
;*  the VVT growing UP from just below the VNT.
;*	This means that the VVT for a variable will always be
;*	found in a fixed place in the VVT table and memory.
;*	For the VNT (names) it grows from vvstart at low memory.
;*
;**********************************************************


	; ROM code
	code  

mod_sz_var_s


;****************************************
;* Find a variable
;* CC if found, A has index
;****************************************
df_var_find
	; start at the beginning of the vnt table
	; vars are stored in reverse order
	_cpyZPWord df_vntstrt,df_tmpptrb
	; start at end of VVT
	_cpyZPWord df_vvtend,df_tmpptra
df_var_match_vnt
	; If reached top of VVT then not found
	lda df_tmpptra
	cmp df_vvtstrt
	bne df_var_match_vnt_do
	lda df_tmpptra+1
	cmp df_vvtstrt+1
	beq df_var_find_no_vnt
df_var_match_vnt_do
	; match each char in buffer with vnt
	ldy df_linoff
	ldx #0
df_var_match_vnt_sym
	lda df_linbuff,y
	cmp (df_tmpptrb,x)
	bne df_var_vnt_sym_nomatch
	; if single char match then increment
	; source and search
	iny
	_incZPWord df_tmpptrb
	; if more chars in vnt entry then continue
	lda (df_tmpptrb,x)
	bne df_var_match_vnt_sym
	; if no more chars in vnt entry but
	; but chars in buffer then try next vnt
	lda df_linbuff,y
	; check for valid alpha-numeric
	jsr df_tk_isalphanum
	; if there is a valid alpha-num then no match
	bcs df_var_vnt_sym_nomatch
	; else check type
df_var_check_type
	; if not alpha-num then check for type
	; string or int postfix didn't match with VNT
;	cmp #'%'
;	beq df_var_vnt_sym_nomatch
	cmp #'$'
	; ok, all good got a match
	bne df_var_find_true
df_var_vnt_sym_nomatch
	; find the zero terminator
	lda (df_tmpptrb,x)
	beq	df_var_vnt_entry_end
	_incZPWord df_tmpptrb
	jmp df_var_vnt_sym_nomatch
df_var_vnt_entry_end
	; skip over zero terminator
	_incZPWord df_tmpptrb
	; update vvt address pointer
	clc
	lda df_tmpptra
	adc #8
	sta df_tmpptra
	_bcc 2
	inc df_tmpptra+1
	bne df_var_match_vnt		; Always - high byte is not zero

	; if at end of vnt then no matches found
df_var_find_no_vnt
	lda #0
	tax
	sec
	rts

df_var_find_true
	; Get slot address into x,a
	ldx df_tmpptra
	lda df_tmpptra+1
	
	; Consume characters found in source
	sty df_linoff
	clc
	rts

	
;****************************************
;* Insert a variable name in to vnt
;* X = number of bytes to make room
;* Space allocated is;
;*	8 bytes for the new VVT entry
;*  X bytes for the new VNT entry
;* df_vntstrt is new var name entry
;* df_vvtend is new vvt entry
;****************************************
df_var_insert_space
	; *** REMEMBER TO DO A SPACE CHECK EVENTUALLY! ***

	; adjust down the VNT start by VVT size (for extra VVT entry)
	; put it in tmpptrb
	sec
	lda df_vntstrt
	sbc #DFVVT_SZ
	sta df_vntstrt
	lda df_vntstrt+1
	sbc #0
	sta df_vntstrt+1
	
	; adjust VNT end by VVT size (for extra VVT entry)
	; and put it in tmpptrc
	sec
	lda df_vntend
	sbc #DFVVT_SZ
	sta df_vntend
	lda df_vntend+1
	sbc #0
	sta df_vntend+1

	; Set copy point to new start of VNT
	_cpyZPWord df_vntstrt,df_tmpptra

	; Now move all bytes from old VNT (higher up) to new VNT position
df_var_move_byte
	; When pointer = vntend then done
	lda df_tmpptra
	cmp df_vntend
	bne df_var_move_byte_do
	lda df_tmpptra+1
	cmp df_vntend+1
	beq df_var_move_byte_fin
df_var_move_byte_do
	; First the source byte VVT size higher in memory
	ldy #DFVVT_SZ
	lda (df_tmpptra),y
	; And copy to new position lower in memory
	ldy #0
	sta (df_tmpptra),y
	; Increment memory pointer
	_incZPWord df_tmpptra
	jmp df_var_move_byte		; Always as C is not touched (WRONG!)
df_var_move_byte_fin
	
	; Now subtract X bytes from VNT start to accommodate new var name
	stx tmp_d
	sec
	lda df_vntstrt
	sbc tmp_d
	sta df_vntstrt
	lda df_vntstrt+1
	sbc #0
	sta df_vntstrt+1

	; VVT end is the same as VNT end
	_cpyZPWord df_vntend,df_vvtend

	; Copy done increment variable count
	inc df_varcnt
	clc
	rts
	
	
;****************************************
;* Analyse variable name
;* Return type in A
;* NOT C = It is a variable
;* C = It is not a variable
;* X = Length including pre-fixes
;* Y = Offset to next char after var name
;****************************************
df_var_analyse
	; Default type is INT
	lda #DFVVT_INT
	sta df_tmpptra
	
	; start at the current buffer position
	ldy df_linoff
	
	; check for PROC prefix
	lda df_linbuff,y
	cmp #'_'
	; if it is not proc then get the name
	bne df_var_not_proc
	; else set type to PROC
	lda #DFVVT_PROC
	sta df_tmpptra
	; skip over prefix
	iny
df_var_not_proc
	; found the actual number of alpha nums
	ldx #0xff
	; go back on pos on index as loop always does iny
	dey
df_var_type_countlen
	; count alpha nums
	iny
	inx
	lda df_linbuff,y
	; first char has to be alpha, rest can be alpha-numeric
	cpx #0
	bne df_var_type_countlen_alphanum
	jsr df_tk_isalpha
	bcs df_var_type_countlen
	bcc df_var_type_countlen_done
df_var_type_countlen_alphanum
	jsr df_tk_isalphanum
	bcs df_var_type_countlen
df_var_type_countlen_done
	cpx #0
	bne df_var_analyse_chk_post
	; if zero alphanums error but not fatal
df_var_analyse_err
	; If already prefix of PROC then fatal error
	lda df_tmpptra
	cmp #DFVVT_PROC
	beq df_var_analyse_fatal_err
	sec
	rts
df_var_analyse_fatal_err
	SWBRK DFERR_SYNTAX
df_var_analyse_chk_post
	; first see if the char is $
	; but cannot already have PROC prefix
;	cmp #'%'
;	bne df_var_analyse_chk_dollar
;	ldx df_tmpptra
;	cpx #DFVVT_PROC
;	beq df_var_analyse_fatal_err
	; Set to INT type although it is the default already
;	lda #DFVVT_INT
;	sta df_tmpptra
	; advance the buffer index
;	iny
;	bne df_var_analyse_chk_arry
df_var_analyse_chk_dollar
	; now see if the char is $
	; but cannot already have PROC prefix
	cmp #'$'
	bne df_var_analyse_chk_arry
	ldx df_tmpptra
	cpx #DFVVT_PROC
	beq df_var_analyse_fatal_err
	; Set to STRING type
	lda #DFVVT_STR
	sta df_tmpptra
	; advance the buffer index
	iny
df_var_analyse_chk_arry
	; Check for array type vs PROC
	lda df_linbuff,y
	cmp #'['
	bne df_var_not_arry
	; array and proc type not compatible
	lda df_tmpptra
	cmp #DFVVT_PROC
	beq df_var_analyse_fatal_err
	ora #DFVVT_ARRY
	sta df_tmpptra
df_var_not_arry
	; Ok got everything
	; calculate length from y
	; y is next char after var name
	tya
	sec
	sbc df_linoff			; where we started
	; put len in X
	tax
	; put type in A
	lda df_tmpptra
	clc
	rts

;****************************************
;* Find or create a variable
;* If found then type needs to match mask
;* Not a fatal error because could be part
;* of a trial of different parsing options
;****************************************
df_var_findcreate
	; save mask
	pha
	jsr df_var_find
	bcs df_var_findcreate_create
	; restore mask to check for 0 then push
	pla
	pha
	; don't check mask if zero
	beq df_var_findcreate_found
	; restore mask
	pla
	ldy #DFVVT_TYPE
	and (df_tmpptra),y
	; but if mask is non zero then this must be non zero too
	beq df_var_findcreate_err
	pha		; Dummy push to match the pla
df_var_findcreate_found
	; discard mask
	pla	
	; put slot address in x,a
	ldx df_tmpptra
	lda df_tmpptra+1
	clc
	rts
	
	;* If no existing variable found, add one to VNT and VVT
df_var_findcreate_create
	; find type (A) and length (X)
	jsr df_var_analyse
	; keep A temporarily
	sta df_tmpptra
	; if not a variable then return with C=1
	bcs df_var_findcreate_errp
	; check if mask needs to be applied
	pla
	beq df_var_analyse_ok
	; else mask and check
	and df_tmpptra
	; mask match is ok
	bne df_var_analyse_ok
	; else return not found
	beq df_var_findcreate_err
df_var_findcreate_errp
	pla
df_var_findcreate_err
	sec
	rts

df_var_analyse_ok
	; extra space for zero terminator
	inx
	; save data in reverse order to when needed
	; by the initialise section x,a (var name sz, type)
	; insert space of X bytes
	lda df_tmpptra
	pha
	txa
	pha
	
	jsr df_var_insert_space
	bcc df_var_initialise_var
	; error inserting space
	SWBRK DFERR_OK
	
df_var_initialise_var
	pla
	sta df_tmpptrc				; var name size + 1

	pla							; type
	ldy #DFVVT_TYPE
	sta (df_vvtend),y			; put type in vvt slot
	lda #0						; zero the rest
	ldy #7
df_var_zero_vnt
	sta (df_vvtend),y
	dey
	bne df_var_zero_vnt			; Don't zero out the type

	dec df_tmpptrc				; 1 less to copy variable name

	ldx df_linoff				; Start at var name beginning, Y already 0
	; copy variable name to vnt slot
df_var_findcreate_copy
	lda df_linbuff,x
	sta (df_vntstrt),y
	inx
	iny
	dec df_tmpptrc
	bne df_var_findcreate_copy
	; put in zero terminator
	lda #0
	sta (df_vntstrt),y

	stx df_linoff				; Update line pointer

	; Return address of slot in X,A
	ldx df_vvtend
	lda df_vvtend+1
	
	clc
	rts
	
mod_sz_var_e
