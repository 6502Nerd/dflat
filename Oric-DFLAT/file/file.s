;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  FILE.S
;*	These routines allow for input/output and are used
;*	by both the tape and parallel devices.
;*	The format is based on a tape format.
;*	The tape format is not compatible with a regular Oric
;*	but is a block format as follows;
;*	Some zero bits (64) start a block.
;*	Then two bytes of data representing the block number.
;*	Then 256 bytes of block data.
;*	Each byte above consists of two '1' start bits and
;*	one '0' stop bit.
;*
;**********************************************************

	; ROM code
	code

mod_sz_file_s

;* Vector table for tape or sd card device
tpsd_vectors
;* Tape vectors
	dw	tp_init
	dw	tp_read_byte
	dw	tp_write_byte
	dw	tp_put_delay
	dw	tp_block_gap
	dw	tp_release
;* SD Card vectors
	dw	sd_init
	dw	sd_read_byte
	dw	sd_write_byte
	dw	sd_put_delay
	dw	sd_block_gap
	dw	sd_release

;* Jump through vectors for device specific calls
f_init
	jmp (f_storage_vec+f_init_vec)
f_read_byte
	jmp (f_storage_vec+f_read_byte_vec)
f_write_byte
	jmp (f_storage_vec+f_write_byte_vec)
f_put_delay
	jmp (f_storage_vec+f_put_delay_vec)
f_block_gap
	jmp (f_storage_vec+f_block_gap_vec)
f_release
	jmp (f_storage_vec+f_release_vec)
	
f_open_msg
	db	"Loading:",0
f_save_msg
	db	"Saving:",0
f_back4
	db	8,8,8,8,0

f_print_open
	ldy #lo(f_open_msg)
	lda #hi(f_open_msg)
	jmp gr_print_msg

;* Common function to open tape
;* df_linbuff contains the filename without device name
;* A contains device (0=tape, 1=sdcard)
f_open_common				; Used by open write also
	tay						; Move A to Y
	beq f_skip_sd			; Skip SD card if 0
	ldy #f_storage_table	; Else Y index into sd card table
f_skip_sd	
	ldx #0					; Start at beginning of table
f_init_vectors
	lda tpsd_vectors,y		; Get vector byte
	sta f_storage_vec,x		; Save to ram table
	iny
	inx
	cpx #f_storage_table	; Keep going until whole table copied
	bne f_init_vectors

	sei						; Interrupts disabled from here..
	ldx #0
	stx tp_idx				; Initialise block
	stx tp_block
	stx tp_block+1
	
	jsr f_init				; Initialise VIA etc.
	rts

;* tp_open_read

fd_cload_getname_pc			; ** FOR ORICUTRON EMULATOR **

f_open_bread
f_open_read
	pha						; Don't lose device number
	lda #1					; Read mode
	sta tp_flag
	jsr f_print_open
	pla
	jsr f_open_common

f_get_fname_start
	jsr f_read_byte
	cmp #'$'				; Find the $ symbol
	bne f_get_fname_start
	
	ldx #9					; 9 byte header - skip
f_get_header
	jsr f_read_byte
	dex
	bne f_get_header
	
	; X= zero here, now get null terminated filename
f_get_fname
	jsr f_read_byte
	sta df_linbuff,x
	inx
	cmp #0
	bne f_get_fname

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
f_close
	lda tp_flag
	cmp #2					; Write mode?
	bne f_close_no_flush	; If not no need to flush
	jsr f_put_block			; Flush the current block
f_close_no_flush
	lda #0x0d				; Line feed
	jsr gr_put_byte
	jsr f_release			; Device specific resource release
	jsr init_via0			; Back to normal
	cli
	lda #0					; Zero tape flag
	sta tp_flag

tt_csave_end_pc				; ** FOR ORICUTRON EMULATOR **

	clc
	rts

;* tp_open_write
;* Open tape settings
f_open_bwrite
	ldy #hi(0x101)
	ldx #lo(0x101)
	bne f_open_write_start
f_open_write
	ldy #hi(6000+0x101)
	ldx #lo(6000+0x101)

fd_csave_getname_pc			; ** FOR ORICUTRON EMULATOR **

f_open_write_start
	pha						; Don't lose device number
	lda #2					; Write mode
	sta tp_flag
	sty tp_delay+1
	stx tp_delay
	ldy #lo(f_save_msg)
	lda #hi(f_save_msg)
	jsr gr_print_msg
	ldy #lo(df_linbuff)
	lda #hi(df_linbuff)
	jsr gr_print_msg
	lda #' '
	jsr gr_put_byte
	pla	
	jsr f_open_common		; Same as reading but tp_mode=2

tt_writeleader_pc			; ** FOR ORICUTRON EMULATOR **
tt_writeleader_end_pc		; ** FOR ORICUTRON EMULATOR **
	ldx #128				; 128 low bits
	jsr f_put_delay

	lda #'$'				; Start of header symbol
	sec
	jsr f_write_byte
	
	ldx #9					; Header, for future expansion
	lda #0xff
f_write_header	
	sec
	jsr f_write_byte
	dex
	bne f_write_header
	
	ldx #0
f_write_fname
	lda df_linbuff,x		; Put the filename
	sec
	jsr f_write_byte
	inx
	cmp #0
	bne f_write_fname
	
	clc
	rts


;* f_put_byte
;* Put to byte to tape - if got a block then save block

f_put_byte
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
	bne f_put_byte_done
	jsr f_put_block			; Flush block to tape
	clc						; Increment block #
	lda tp_block
	adc #1
	sta tp_block
	lda tp_block+1
	adc #0
	sta tp_block+1
f_put_byte_done
	pla
	tay
	pla
	tax
	pla
	clc
	rts

;* tp_put_block
;* flush a block to tape
f_put_block
	jsr f_print_block
		
	ldx #128				; 128 low bits
	jsr f_put_delay

	lda tp_block			; Save block number low then high
	clc
	jsr f_write_byte
	lda tp_block+1
	sec
	jsr f_write_byte

	ldx #0
f_put_block_bytes
	lda tp_buf,x			; Then write out all bytes in block
	clc
	jsr f_write_byte
	inx
	bne f_put_block_bytes	; Always 256 bytes
	
	jmp f_block_gap		; Wait a while, still outputting 0 bits

;	rts

; tp_print_block
f_print_block
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
	lda #hi(f_back4)		; Go back 4 characters to print next block
	ldy #lo(f_back4)
	jmp gr_print_msg
;	rts


;* t_get_byte
;* Get a byte from buffer - load blocks as needed
f_get_byte
	txa
	pha
	tya
	pha
	
	ldy tp_idx				; Index in to buffer
	bne f_get_byte_buf		; No need to load from tape
	jsr f_get_block			; Else load a block from tape
	ldy #0					; 0 index is now valid
f_get_byte_buf
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

;* f_get_block
;* Load a block in to memory
f_get_block

	jsr f_read_byte		; Get block number
	sta tp_block
	jsr f_read_byte
	sta tp_block+1
	
	jsr f_print_block
	
	ldy #0
	sty tp_idx
f_get_block_bytes
	jsr f_read_byte		; Fill buffer
	ldy tp_idx
	sta tp_buf,y
	iny
	sty tp_idx
	bne f_get_block_bytes	; Always 256 bytes

	rts
	

mod_sz_file_e

