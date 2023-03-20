;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  IO.S
;*  General IO module.  Allows different IO devices to be
;*  utilised transparently by the rest of the code.
;*  Other code should use "io_" commands so that they
;*  do not need to know what specific device is providing
;*  input and output capabilities.
;*	On startup, the kernel defaults to keyboard and screen
;*  examines the BBC DIP switch to decide whether to
;*	but when saving and loading points to tape routines.
;*
;**********************************************************

	; ROM code
	code

mod_sz_io_s

;****************************************
;* io_init
;* Initialise the default device and make active
;* No keys pressed = serial (default)
;* F0 pressed = KB/VDP
;* F1 pressed = Serial
;* Keyboard and screen or serial port
;* Output : None
;* Regs affected : P, A
;****************************************
io_init
	lda #1				; Default = KB/VDP
	sta io_default
	jmp io_active_device; Activate the device


;****************************************
;* io_set_default, io_active_device
;* Activate device based on default or A
;* Input : A = Device number
;* Output : None
;* Regs affected : P, A
;****************************************
io_set_default			; Entry point for default
	lda io_default
io_active_device		; Entry point for A set
	tax					; X=buf size index
	asl	a				; x16 the Block number
	asl a
	asl a
	asl a
	tay					; Y=index in to device table
	lda io_buf_sz,x
	sta buf_sz
	ldx #0
	; Copy device settings to io block
io_copy_data
	lda io_devices,y
	sta io_block,x
	iny
	inx
	cpx #16
	bne io_copy_data
	
	lda #lo(scratch)	; Initialise IO buffer and size
	sta buf_lo
	lda #hi(scratch)
	sta buf_hi
	lda #UTF_CR			; Line terminator is CR
	sta buf_ef
	rts

;****************************************
;* io_get_ch
;* Get a char (wait forever or just check)
;* Input : C = 1 for synchronous, 0 for async
;* Output : A = Byte code, C = 1 means A is invalid
;* Regs affected : P, A
;****************************************
io_get_ch
	jmp (io_block+io_get_byte)
	

;****************************************
;* io_put_ch
;* Put a char
;* Input : A = char
;* Regs affected : P, A
;****************************************
io_put_ch
	jmp (io_block+io_put_byte)
	
;****************************************
;* io_open_read
;* Open for reading
;* Input : X,A = pointer to filename (zero terminated)
;* Output : C=0 success
;* Regs affected : All
;****************************************
io_open_read
	jmp (io_block+io_open_r)
	
;****************************************
;* io_open_write
;* Open for reading
;* Input : X,A = pointer to filename (zero terminated)
;* Output : C=0 success
;* Regs affected : All
;****************************************
io_open_write
	jmp (io_block+io_open_w)

;****************************************
;* io_close
;* Close a file
;* Input : 
;* Output : C=0 success
;* Regs affected : All
;****************************************
io_close
	jmp (io_block+io_close_f)
	
;****************************************
;* io_delete
;* Delete a file
;* Input : 
;* Output : C=0 success
;* Regs affected : All
;****************************************
io_delete
	jmp (io_block+io_del_f)

;****************************************
;* io_open_ext1
;* Extended function 1
;* Input : 
;* Output : C=0 success
;* Regs affected : All
;****************************************
io_open_ext1
	jmp (io_block+io_ext1)
	
;****************************************
;* io_open_ext2
;* Extended function 2
;* Input : 
;* Output : C=0 success
;* Regs affected : All
;****************************************
io_open_ext2
	jmp (io_block+io_ext2)
	
;****************************************
;* io_read_line
;* Read a line, terminated by terminating char or max buffer length
;* Input : buf_(lo/hi/sz/ef) : Address, Max size, end marker, C = 1 means echo
;* Output : Y = Line length C = Buffer limit reached
;* Regs affected : None
;****************************************
io_read_line
	pha

	php					; Save echo state
	
	ldy #0x00			; Starting at first byte
io_get_line_byte
	sec					; Getting bytes synchronously
	jsr io_get_ch		; Get a byte
	bcs io_get_line_done; Got nothing then finish
	plp					; Get echo state
	php					; Instantly save it back
	bcc io_skip_echo	; Carry not set = don't echo
	cmp #UTF_DEL		; Delete?
	bne io_do_echo
	cpy #0				; Already at beginning?
	beq io_skip_echo	; Don't echo delete
	dey					; Else decrement length
io_do_echo
	jsr io_put_ch		; Echo it
io_skip_echo
	cmp #UTF_SPECIAL	; Special character?
	bcc io_skip_special	; Skip if so (don't add to buffer)
	cmp #UTF_DEL		; Don't proces DEL either
	beq io_skip_special
	sta (buf_lo),y		; Save it
	iny					; Increase length
io_skip_special
	cmp buf_ef			; Is it the terminating char?
	beq io_get_line_done	; If yes then done
	cpy buf_sz			; Reached the buffer max size?
	bne io_get_line_byte	; No, get another byte
	plp					; Remember to pull echo state off stack
	sec					; Yes, set carry flag
	pla
	rts					; And done
io_get_line_done
	lda #0
	sta (buf_lo),y		; Terminate with 0
	plp					; Remember to pull echo state off stack
	clc					; Clear carry flag
	pla
	rts					; Fin

;****************************************
;* io_print_line
;* Print a line (when data is not already in serial buffer)
;* Input : X = Address Lo, A = Address Hi
;* Output : Y=number chars output
;* Regs affected : All
;****************************************
io_print_line
	pha

	stx tmp_clo					; Store the string pointer
	sta tmp_chi					; lo and hi
	ldy #0						; Start at the beginning!
io_print_line_byte
	lda (tmp_clo),y				; Copy byte to
	beq io_print_done			; If zero then done - print
	jsr io_put_ch				; Transmit
	iny
	bne io_print_line_byte		; Carry on until zero found or Y wraps
io_print_done
	pla
	rts


;*** Null operation just clc and return ***
io_null_op
	clc
	rts
	
;* IO buffer sizes
io_buf_sz
	db 255					; Device 0 = Tapeoe SDCard
	db 127					; Device 1 = keyboard/screen

;* IO devices defined here
io_devices
;* Device zero is the tape/sdcard system
;* t: is for tape, s: is for sdcard in the filename
;* the file routines parse the filename to fine t: or s:
;* at the file protocol level both t: and s: appear the same
;* hence device0 can handle both
;* This is a block based device
io_device0					; Tape device, input = Tape, output = Tape
	dw	f_get_byte			; io_get_ch
	dw	f_put_byte			; io_put_ch
	dw	f_open_read			; io_open_r
	dw	f_open_write		; io_open_w
	dw	f_close				; io_close_f
	dw	io_null_op			; io_del_f
	dw	f_open_bread		; io_ext1 - open for binary read
	dw	f_open_bwrite		; io_ext2 - open for binary write
;* Device one is keyboard / screen
;* only offers get and put
;* This is a char based device
io_device1					; Default device, input = screen editor, output = screen editor
	dw	gr_get_key			; io_get_ch
	dw	gr_put_byte			; io_put_ch
	dw	io_null_op			; io_open_r
	dw	io_null_op			; io_open_w
	dw	io_null_op			; io_close_f
	dw	io_null_op			; io_del_f
	dw	io_null_op			; io_ext1
	dw	io_null_op			; io_ext2
mod_sz_io_e

