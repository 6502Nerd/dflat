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

tp_open_msg
	db	"Loading:",0
tp_save_msg
	db	"Saving:",0
tp_back4
	db	8,8,8,8,0
	
;* tp_init
;* Basic initialisation for tape handling
tp_init
	lda #0
	sta tp_flag
	sta tp_idx
	clc
	rts

;* Common function to open tape
tp_open_common				; Used by open write too
	sei						; Interrupts disabled from here..
	jsr init_via0_tape		; Get ready to output (tape ON)


	ldx #0
	stx tp_idx				; Initialise block
	stx tp_block
	stx tp_block+1
	inx
	stx tp_flag				; assume 1 = Read mode
	rts

;* tp_open_read
;* Open tape settings

fd_cload_getname_pc			; ** FOR ORICUTRON EMULATOR **

tp_open_bread
tp_open_read
	ldy #lo(tp_open_msg)
	lda #hi(tp_open_msg)
	jsr gr_print_msg
	
	jsr tp_open_common

tp_get_fname_start
	jsr tp_read_byte
	cmp #'$'				; Find the $ symbol
	bne tp_get_fname_start
	
	ldx #9					; 9 byte header - skip
tp_get_header
	jsr tp_read_byte
	dex
	bne tp_get_header
	
	; X= zero here, now get null terminated filename
tp_get_fname
	jsr tp_read_byte
	sta df_linbuff,x
	inx
	cmp #0
	bne tp_get_fname

	ldy #lo(df_linbuff)
	lda #hi(df_linbuff)
	jsr gr_print_msg
	lda #' '
	jsr gr_put_byte

	clc
	rts

tt_getsync_pc				; ** FOR ORICUTRON EMULATOR **
tt_getsync_end_pc			; ** FOR ORICUTRON EMULATOR **

	nop
	

;* tp_close
;* Close tape settings
tp_close
	lda tp_flag
	cmp #2					; Write mode?
	bne tp_close_no_flush	; If not no need to flush
	jsr tp_put_block		; Flush the current block
tp_close_no_flush
	lda #0x0d				; Line feed
	jsr gr_put_byte
	
	jsr init_via0			; Back to normal
	cli
	lda #0					; Zero tape flag
	sta tp_flag

tt_csave_end_pc				; ** FOR ORICUTRON EMULATOR **

	clc
	rts

;* tp_open_write
;* Open tape settings
tp_open_bwrite
	ldy #hi(0x101)
	ldx #lo(0x101)
	bne tp_open_write_start
tp_open_write
	ldy #hi(6000+0x101)
	ldx #lo(6000+0x101)

fd_csave_getname_pc			; ** FOR ORICUTRON EMULATOR **

tp_open_write_start
	sty tp_delay+1
	stx tp_delay
	ldy #lo(tp_save_msg)
	lda #hi(tp_save_msg)
	jsr gr_print_msg
	ldy #lo(df_linbuff)
	lda #hi(df_linbuff)
	jsr gr_print_msg
	lda #' '
	jsr gr_put_byte
	
	jsr tp_open_common		; Same as reading but
	inc tp_flag				; Make flag = 2 for write mode

tt_writeleader_pc			; ** FOR ORICUTRON EMULATOR **
tt_writeleader_end_pc		; ** FOR ORICUTRON EMULATOR **
	ldx #128				; 128 low bits
	jsr tp_put_delay

	lda #'$'				; Start of header symbol
	sec
	jsr tp_write_byte
	
	ldx #9					; Header, for future expansion
	lda #0xff
tp_write_header	
	sec
	jsr tp_write_byte
	dex
	bne tp_write_header
	
	ldx #0
tp_write_fname
	lda df_linbuff,x		; Put the filename
	sec
	jsr tp_write_byte
	inx
	cmp #0
	bne tp_write_fname
	
	clc
	rts


;* tp_put_byte
;* Put to byte to tape - if got a block then save block

tp_put_byte
	pha
	sta tmp_d
	txa
	pha
	tya
	pha
	lda tmp_d
	
	ldy tp_idx				; Save byte to buffer
	sta tp_buf,y
	iny
	sty tp_idx
	bne tp_put_byte_done
	jsr tp_put_block		; Flush block to tape
	clc						; Increment block #
	lda tp_block
	adc #1
	sta tp_block
	lda tp_block+1
	adc #0
	sta tp_block+1
tp_put_byte_done
	pla
	tay
	pla
	tax
	pla
	clc
	rts

; tp_put_delay
;* X = number of zero bits to insert
tp_put_delay
	clc
	jsr tp_write_bit
	dex
	bne tp_put_delay
	rts

;* tp_put_block
;* flush a block to tape
tp_put_block
	jsr tp_print_block
		
	ldx #128				; 128 low bits
	jsr tp_put_delay

	lda tp_block			; Save block number low then high
	clc
	jsr tp_write_byte
	lda tp_block+1
	sec
	jsr tp_write_byte

	ldx #0
tp_put_block_bytes
	lda tp_buf,x			; Then write out all bytes in block
	clc
	jsr tp_write_byte
	inx
	bne tp_put_block_bytes	; Always 256 bytes
	
	jsr tp_block_gap		; Wait a while, still outputting 0 bits

	rts

; tp_print_block
tp_print_block
	lda tp_block+1			; Print block number hi byte
	jsr str_a_to_x
	jsr gr_put_byte
	txa
	jsr gr_put_byte
	lda tp_block			; Print block number lo byte
	jsr str_a_to_x
	jsr gr_put_byte
	txa
	jsr gr_put_byte
	lda #hi(tp_back4)		; Go back 4 characters to print next block
	ldy #lo(tp_back4)
	jsr gr_print_msg
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



;* tp_get_byte
;* Get a byte from buffer - load blocks as needed
tp_get_byte
	txa
	pha
	tya
	pha
	
	ldy tp_idx				; Index in to buffer
	bne tp_get_byte_buf		; No need to load from tape
	jsr tp_get_block		; Else load a block from tape
	ldy #0					; 0 index is now valid
tp_get_byte_buf
	lda tp_buf,y			; Get from memory buffer
	iny
	sty tp_idx				; Never zero on leaving
	sta tmp_d
	pla
	tay
	pla
	tax
	lda tmp_d
	clc
	rts						; Except when buffer done

;* tp_get_block
;* Load a block in to memory
tp_get_block

	jsr tp_read_byte		; Get block number
	sta tp_block
	jsr tp_read_byte
	sta tp_block+1
	
	jsr tp_print_block
	
	ldy #0
	sty tp_idx
tp_get_block_bytes
	jsr tp_read_byte		; Fill buffer
	ldy tp_idx
	sta tp_buf,y
	iny
	sty tp_idx
	bne tp_get_block_bytes	; Always 256 bytes

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


