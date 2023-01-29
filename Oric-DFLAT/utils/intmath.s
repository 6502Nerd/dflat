;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  INTMATH.S
;*  Core module for integer maths supported by dflat
;*  Now supports signed integers in 2s complement form
;*  Uses the intmath registers: num_a, num_b, num_x, num_tmp
;*  Most inputs are through num_a and num_b, with result in
;*  num_a
;*  Operations: add, sub, swap, 8 bit mult, mult, divide
;*
;**********************************************************

	; ROM code
	code

;****************************************
;* Add : A + B result in A
;****************************************
;int_add
;	clc
;	lda num_a
;	adc num_b
;	sta num_a
;	lda num_a+1
;	adc num_b+1
;	sta num_a+1
;	rts
	
;****************************************
;* Sub : A - B result in A
;****************************************
;int_sub
;	sec
;	lda num_a
;	sbc num_b
;	sta num_a
;	lda num_a+1
;	sbc num_b+1
;	sta num_a+1
;	rts
	
;****************************************
;* Swp : A <-> B 
;****************************************
;int_swp
;	lda num_a
;	ldx num_b
;	sta num_b
;	stx num_a
;	lda num_a+1
;	ldx num_b+1
;	sta num_b+1
;	stx num_a+1
;	rts
	
;****************************************
;* Mult : A * B result in A
;* B assumed to be an 8 bit quantity 
;****************************************
int_fast_mult
	_cpyZPWord num_a,num_tmp
	lda #0
	sta num_a
	sta num_a+1
	ldy #8
int_fast_mult_cycle
	lsr num_b
	bcc int_fast_mult_next
	clc
	lda num_a
	adc num_tmp
	sta num_a
	lda num_a+1
	adc num_tmp+1
	sta num_a+1
int_fast_mult_next
	asl num_tmp
	rol num_tmp+1
	dey
	bne int_fast_mult_cycle
	rts
	
;****************************************
;* Mult : A * B result in A
;****************************************
int_mult
	_cpyZPWord num_a,num_tmp
	lda #0
	sta num_a
	sta num_a+1
	ldy #16
int_mult_cycle
	lsr num_b+1
	ror num_b
	bcc int_mult_next
	clc
	lda num_a
	adc num_tmp
	sta num_a
	lda num_a+1
	adc num_tmp+1
	sta num_a+1
int_mult_next
	asl num_tmp
	rol num_tmp+1
	dey
	bne int_mult_cycle
	rts

;****************************************
;* Div : A / B result in A, remainder X
;****************************************
int_div
	; adjust signs
	lda #0
	sta num_tmp				; Assume all +ve
	lda num_a+1				; Check A hi
	bpl int_div_skip_negA
	inc num_tmp				; Record sign flip
	sec						; 2's complement A
	lda #0					; by 0-A
	sbc num_a				; A is now +ve
	sta num_a
	lda #0
	sbc num_a+1
	sta num_a+1	
int_div_skip_negA
	lda num_b+1				; Check B hi
	bpl int_div_skip_negB
	inc num_tmp				; Record sign flip
	sec						; 2's complement B
	lda #0					; by 0-B
	sbc num_b				; B is now +ve
	sta num_b
	lda #0
	sbc num_b+1
	sta num_b+1
int_div_skip_negB			; num_tmp bit 0=1 for result flip
	; x is the remainder
	lda #0
	sta num_x
	sta num_x+1
	; 16 bit division
	ldy #16
int_div_cycle
	; shift a left 1 bit
	asl num_a
	rol num_a+1
	; shift in to remainder
	rol num_x
	rol num_x+1
	; try and subtract b from remainder
	sec
	lda num_x
	sbc num_b
	tax
	lda num_x+1
	sbc num_b+1
	bcc int_div_skip
	; so b did fit in to remainder, save it
	stx num_x
	sta num_x+1
	inc num_a
int_div_skip
	; carry on for 16 bits
	dey
	bne int_div_cycle
	; result in a, remainder in x
	; check num_tmp bit 0
	lda num_tmp
	and #1
	beq int_div_noflip
	sec						; 2's complement A
	lda #0					; by 0-A
	sbc num_a				; A is now +ve
	sta num_a
	lda #0
	sbc num_a+1
	sta num_a+1	
int_div_noflip
	rts
	
