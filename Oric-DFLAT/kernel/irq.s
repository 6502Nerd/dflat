	
;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  IRQ.S
;*	This is the IRQ handler
;*	There is only one regular interrupt source which is T1
;*	T1 timeout on a 50Hz cycle, whose main job is to
;*	flash the cursor and decrement some timers.
;*	But there are handlers for BRK, user and VIA sources.
;*	By default the user and VIA do nothing, but can be
;*	redirected to user routines.
;*
;**********************************************************

;* Obviously this can only be done with
;* interrupts disabled!
init_irq
	; Core IRQ handler
	lda #lo(irq)
	sta vec_irq
	lda #hi(irq)
	sta vec_irq+1
	
	; Core BRK handler
	lda #lo(irq_brk)
	sta vec_brk
	lda #hi(irq_brk)
	sta vec_brk+1

	; User handlers VIA0 interrupts
	lda #lo(null_handler)
	sta vec_usercia0
	lda #hi(null_handler)
	sta vec_usercia0+1

	rts


;* Calls the master IRQ handler - from the ROM
call_irq_master
	jmp (vec_irq)
	
;* Calls the BRK handler
call_irq_brk
	jmp (vec_brk)

;* Call the user CIA0 handler
call_irq_usercia0
	jmp (vec_usercia0)

;* null interrupt
null_irq
	rti

;* null handler
null_handler
	rts
	

;* Master IRQ handler
irq
	; Don't use _pushAXY as it uses a temp location!
	pha
	txa
	pha
	tya
	pha

	cld						; Just in case!
	clc						; Standard behaviour

	; Check if IRQ or BRK
	; load P from stack in to A
	tsx
	lda 0x104,x
	; BRK bit set?
	and #0x10
	bne call_irq_brk
	
	;* Primary interrupt is timer 1
	lda IO_0 + IFR
	and #0x40				; Bit 6 = Timer 1 interrupt
	beq irq_fin				; If nothing then end

	; Service the timer 1 interrupt
	sta IO_0 + IFR			; Clear the interrupt
	jsr int_vdp_handler

irq_fin
	_pullAXY
	rti
	
;* Handle BRK
irq_brk
	; Handle BRK
	; Get PCL,H minus 2 gives the BRK instruction address
	sec
	lda 0x0105,x
	sbc #2
	sta df_brkpc
	lda 0x0106,x
	sbc #0
	sta df_brkpc+1
	; Get the byte pointed to by old PC
	; which is 1 on from the BRK
	ldy #1
	lda (df_brkpc),y
	sta df_brkval
	sta errno
	; now update the return address
	lda df_pc
	sta 0x105,x
	lda df_pc+1
	sta 0x106,x
	
	_pullAXY
	; Save the registers in temp area
	sta num_a
	stx num_a+1
	sty num_a+2
	; when RTI occurs:
	;  will return to error handler
	;  df_brkval will contain signature
	rti
	
	
;****************************************
;* int_vdp_handler
;* VDP interrupt handler
;****************************************
int_vdp_handler
	jsr update_timers	; If it is then update system timers (kernel routine)
	lda vdp_curoff		; Is cursor enabled?
	bne int_vdp_fin		; Skip if disabled

	dec vdp_curcnt		; Decrement countdown
	bne int_vdp_fin		; If not expired, do nothing
	lda vdp_curtim		; Reset cursor countdown
	sta vdp_curcnt
	lda vdp_curstat		; Get the flash status
	eor #0x80			; Invert top bit
	sta vdp_curstat
	eor vdp_curval		; EOR with whats under cursor
	; Use cursor address, write to screen
	; ptr is base, offset with X coord in Y register
	ldy gr_scrngeom+gr_cur_x
	sta (gr_scrngeom+gr_cur_ptr),y

int_vdp_fin	
	rts


;****************************************
;* update_timers
;* Update 24 bit timer and debounce counters
;****************************************
update_timers
	inc vdp_cnt
	bne inc_kb_timers
	inc vdp_cnt_hi
	bne inc_kb_timers
	inc vdp_cnt_hi2
inc_kb_timers
	ldx kb_deb			; Is debounce 0?
	beq skip_kb_deb
	dec kb_deb
skip_kb_deb
	ldx kb_rep			; Is repeat timer 0?
	beq skip_kb_rep
	dec kb_rep
skip_kb_rep
	rts
	
