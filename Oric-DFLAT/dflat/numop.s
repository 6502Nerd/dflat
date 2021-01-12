;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  NUMOP.S
;*	Dflat number AND string operators.
;*  Uses the operator stack to get parameters, leaving the
;*  result on the operator stack.
;*
;**********************************************************

	; ROM code
	code  

; common pushint code
df_rt_putintres
	ldx df_tmpptra
	lda df_tmpptra+1
	jmp df_ost_pushInt

; add two numbers
df_rt_add
	jsr df_rt_get2Ints
	_addZPWord df_tmpptra,df_tmpptrb
	jmp df_rt_putintres
	
; subtract
df_rt_sub
	jsr df_rt_get2Ints
	_subZPWord df_tmpptra,df_tmpptrb
	jmp df_rt_putintres

; multiply
df_rt_mult
	jsr df_rt_get2Ints
	_cpyZPWord df_tmpptra,num_a
	_cpyZPWord df_tmpptrb,num_b
	jsr int_mult
	_cpyZPWord num_a,df_tmpptra
	jmp df_rt_putintres

; divide
df_rt_div
	jsr df_rt_get2Ints
	_cpyZPWord df_tmpptra,num_a
	_cpyZPWord df_tmpptrb,num_b
	jsr int_div
	_cpyZPWord num_a,df_tmpptra
	jmp df_rt_putintres

; mod
df_rt_mod
	jsr df_rt_get2Ints
	_cpyZPWord df_tmpptra,num_a
	_cpyZPWord df_tmpptrb,num_b
	jsr int_div
	_cpyZPWord num_x,df_tmpptra
	jmp df_rt_putintres

; shift left
df_rt_asl
	jsr df_rt_get2Ints
	; use low byte only for # of shifts
	ldx df_tmpptrb
	inx
df_rt_aslbit
	dex
	beq df_rt_asldone
	asl df_tmpptra
	rol df_tmpptra+1
	jmp df_rt_aslbit
df_rt_asldone
	jmp df_rt_putintres

; shift right
df_rt_lsr
	jsr df_rt_get2Ints
	; use low byte only for # of shifts
	ldx df_tmpptrb
	inx
df_rt_lsrbit
	dex
	beq df_rt_lsrdone
	lsr df_tmpptra+1
	ror df_tmpptra
	jmp df_rt_lsrbit
df_rt_lsrdone
	jmp df_rt_putintres

; logical and
df_rt_and
	jsr df_rt_get2Ints
	lda df_tmpptra
	and df_tmpptrb
	tax
	lda df_tmpptra+1
	and df_tmpptrb+1
	jmp df_ost_pushInt
	
; logical or
df_rt_or
	jsr df_rt_get2Ints
	lda df_tmpptra
	ora df_tmpptrb
	tax
	lda df_tmpptra+1
	ora df_tmpptrb+1
	jmp df_ost_pushInt

; logical eor
df_rt_eor
	jsr df_rt_get2Ints
	lda df_tmpptra
	eor df_tmpptrb
	tax
	lda df_tmpptra+1
	eor df_tmpptrb+1
	jmp df_ost_pushInt

; a==b common function
df_rt_aequb
	lda df_tmpptra
	cmp df_tmpptrb
	bne df_rt_aequbFin
	lda df_tmpptra+1
	cmp df_tmpptrb+1
df_rt_aequbFin
	rts

; a-b common function
df_rt_asubb
	sec
	lda df_tmpptra
	sbc df_tmpptrb
	lda df_tmpptra+1
	sbc df_tmpptrb+1
	bvc df_rt_asubbFin
	eor #0x80
df_rt_asubbFin
	rts

; b-a common function
df_rt_bsuba
	sec
	lda df_tmpptrb
	sbc df_tmpptra
	lda df_tmpptrb+1
	sbc df_tmpptra+1
	bvc df_rt_bsubaFin
	eor #0x80
df_rt_bsubaFin
	rts

; common routine push true
df_rt_true
	ldx #0xff
	txa
	jmp df_ost_pushInt

; common routine push false
df_rt_false
	ldx #0x00
	txa
	jmp df_ost_pushInt

; a < b == (a-b) < 0
df_rt_lt
	jsr df_rt_get2Ints
	jsr df_rt_asubb
	bmi df_rt_true
	bpl df_rt_false

; a <=b == !(b-a > 0)
df_rt_lte
	jsr df_rt_get2Ints
df_rt_lte_calc
	jsr df_rt_bsuba
	bmi df_rt_false
	bpl df_rt_true

; a > b == (b-a) < 0
df_rt_gt
	jsr df_rt_get2Ints
	jsr df_rt_bsuba
	bmi df_rt_true
	bpl df_rt_false

; a >= b == (a-b >=0)
df_rt_gte
	jsr df_rt_get2Ints
df_rt_gte_calc
	jsr df_rt_asubb
	bpl df_rt_true
	bmi df_rt_false

; a == b
df_rt_eq
	jsr df_rt_get2Ints
	jsr df_rt_aequb
	bne df_rt_false
	beq df_rt_true

; a <> b
df_rt_ne
	jsr df_rt_get2Ints
	jsr df_rt_aequb
	beq df_rt_false
	bne df_rt_true

; Common usage of boolean operators for num and string

df_rt_comlt
	; First find the data type on the stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_slt
	; If not string then assume number
	beq df_rt_lt

df_rt_comgt
	; First find the data type on the stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_sgt
	; If not string then assume number
	beq df_rt_gt

df_rt_comeq
	; First find the data type on the stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_seq
	; If not string then assume number
	beq df_rt_eq

df_rt_comlte
	; First find the data type on the stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_slte
	; If not string then assume number
	beq df_rt_lte
	
df_rt_comgte
	; First find the data type on the stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_sgte
	; If not string then assume number
	beq df_rt_gte

df_rt_comne
	; First find the data type on the stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_sne
	; If not string then assume number
	beq df_rt_ne


;********** STRING OPS **********

; string less than or equal
df_rt_slte
	jsr df_rt_str_comp
	bmi df_rt_str_comp_true
	beq df_rt_str_comp_true
df_rt_str_comp_false
	jmp df_rt_false
df_rt_str_comp_true
	jmp df_rt_true	

; string greater then or equal	
df_rt_sgte
	jsr df_rt_str_comp
	bpl df_rt_str_comp_true
	bmi df_rt_str_comp_false

; string not equal
df_rt_sne
	jsr df_rt_str_comp
	bne df_rt_str_comp_true
	beq df_rt_str_comp_false

; string less than
df_rt_slt
	jsr df_rt_str_comp
	bmi df_rt_str_comp_true
	bpl df_rt_str_comp_false

; string greater than
df_rt_sgt
	jsr df_rt_str_comp
	bmi df_rt_str_comp_false
	beq df_rt_str_comp_false
	bne df_rt_str_comp_true

; string equal
df_rt_seq
	jsr df_rt_str_comp
	beq df_rt_str_comp_true
	bne df_rt_str_comp_false

; common string comparator
; N=1 means <
; Z=0 means >
; Z=1 means ==
df_rt_str_comp
	jsr df_rt_get2Strs
	ldy #0
df_rt_str_comp_byte
	lda (df_tmpptra),y
	tax							; Save op1 char in X
	cmp (df_tmpptrb),y
	; if c=0 then <
	bcc df_rt_str_comp_lt
	; if c=1 and nz then >
	bne df_rt_str_comp_gt
	; if here then both strings still the same
	txa							; What was op1 char
	; if char is zero then end
	beq df_rt_str_comp_eq
	; else do next char
	iny
	bne df_rt_str_comp_byte ; Always - relying on Y is never 0
df_rt_str_comp_lt
	lda #0xff
	rts
df_rt_str_comp_gt
	lda #0x01
	rts
df_rt_str_comp_eq
	lda #0x00
	rts


