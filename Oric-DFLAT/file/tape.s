;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  TAPE.S
;*	These routines allow for tape input/output
;*	The tape format is not compatible with a regular Oric
;*	but is a block format as follows;
;*	Some zero bits (64) start a block.
;*	Then two bytes of data representing the block number.
;*	Then 256 bytes of block data.
;*	Each byte above consists of two '1' start bits and
;*	one '0' stop bit.
;*	After each block, 1 second of 4800Hz (seen as zero bits)
;*	is transmitted to allow the CPU to deal with the block
;*	when subsequently loading.
;*	A bit is 4800Hz half cycle then 4800Hz half cycle for a
;*	'1' or 2400Hz half cycle for a '0', except for end of
;*	block which is one second of 2400Hz.
;*
;**********************************************************

	; ROM code
	code

mod_sz_tape_s

tp_init
	jmp init_via0_tape

tp_release
	rts
	
; tp_put_delay
;* X = number of zero bits to insert
tp_put_delay
	clc
	jsr tp_write_bit
	dex
	bne tp_put_delay
	rts

; tp_block_gap
; Send 2400 bits of zero ~ 1.5 seconds
; add 0x101 due to how counters decrement
; For binary mode it is just 1 bit gap
tp_block_gap
	ldy tp_delay+1
	ldx tp_delay
tp_block_gap_1
	jsr tp_put_delay
	dey
	bne tp_block_gap_1
	rts	

;* tp_write_byte
;* Write a byte to tape which looks like this
;* 11xxxxxxxx0
tt_putbyte_pc				; ** FOR ORICUTRON EMULATOR **
tp_write_byte
	pha
	php
	sec					; 1 for start bit
	jsr tp_write_bit
	sec					; 1 for start bit
	jsr tp_write_bit
	ldy #8				; 8 bits of data
tp_write_byte_bit
	ror a
	jsr tp_write_bit
	dey
	bne tp_write_byte_bit
	clc					; 0 for stop bit
	jsr tp_write_bit
	plp
	bcc tp_write_byte_nodelay
	txa
	ldx #32					; Delays needed
	jsr tp_put_delay
	tax
tp_write_byte_nodelay
	pla
tt_putbyte_end_pc			; ** FOR ORICUTRON EMULATOR **
	rts
	

;* tp_write_bit
;* Write a bit in C to tape which looks like this
;* Half cycle of 4800Hz, then
;*		half cycle of 4800Hz for a 1
;*		half cycle of 2400Hz for a 0
tp_write_bit
	pha
	txa
	pha
	php						; Save the bit to be stored

	lda #TAPE_RATE			; Half cycle first of 4800Hz
	ldx #0
	sta IO_0+T1LL
	stx IO_0+T1LH			; This starts the timer
	lda IO_0+T1CL			; Clear any interrupt flag
tp_write_bit_chk1
	bit IO_0+IFR			; Check IRF for bit 6
	bvc tp_write_bit_chk1	; V=0? Keep checking
	lda IO_0+T1CL			; Clear interrupt flag
	
	plp						; Get the bit to be stored
	lda #TAPE_RATE			; Assume putting out a 1 in 2nd half
	bcs tp_write_bit_t1		; A zero is half the rate (2400Hz)
	asl	a					; TAPE_RATE * 2 need to inc X
	inx
tp_write_bit_t1
	sta IO_0+T1LL
	stx IO_0+T1LH			; This starts the timer
	lda IO_0+T1CL			; Clear any interrupt flag
tp_write_bit_chk2
	bit IO_0+IFR			; Check IRF for bit 6
	bvc tp_write_bit_chk2	; V=0? Keep checking
	lda IO_0+T1CL			; Clear interrupt flag
	pla
	tax
	pla
	rts



;* tp_read_byte
;* Read a byte from tape, always expects one '1' start bit
tt_readbyte_pc				; ** FOR ORICUTRON EMULATOR **
tp_read_byte
tp_read_byte_st
	jsr tp_read_bit			; Expecting a 1
	bcc tp_read_byte_st
	jsr tp_read_bit			; Should be another 1
	bcc tp_read_byte_st
	; Ok got 2 start
	
	ldy #8				; Get 8 bits
tp_read_byte_bits
	jsr tp_read_bit
	ror a
	dey
	bne tp_read_byte_bits
	; Byte is in A, stop bit is not waited for..
tt_readbyte_end_pc			; ** FOR ORICUTRON EMULATOR **
	rts

;* tp_read_bit
;* Get a CB1 transition and measure the time
;* The measurement indicates a 1 or 0 received
tp_read_bit
	pha
	lda IO_0+PRB			; Clear CB1 interrupt flag by reading B
tp_read_bit_cb1
tt_getsync_loop_pc			; ** FOR ORICUTRON EMULATOR **
	lda IO_0+IFR
	and #IFR_CB1
	beq tp_read_bit_cb1		; Keep checking until CB1 active

	lda IO_0+T2CH			; Get the high byte of T2 counter
	pha
	lda #0xff				; Reset T2 counter high byte
	sta IO_0+T2CH			; Reset high byte of T2 counter
	pla						; What was previous counter?
;	beq tp_io_error			; If down to zero, then some issue?
	cmp #0xfe				; C=1 if T2 >= 0xfe else C=0
	pla
	rts						; Carry contains the bit received
tp_io_error
	SWBRK DFERR_BREAK		; DEFINE A NEW ERROR!

mod_sz_tape_e

