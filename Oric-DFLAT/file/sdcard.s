;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  SDCARD.S
;*	These routines allow for SD card input/output
;*	The file format is similar to the dflat tape
;*	format but is *NOT* a block format
;*
;*  Controlled through port A (printer port) as follows;
;*		PA0		Clock			Out		Controlled by Oric
;*		PA1		D0				In/Out	To/from Oric protocol dependent
;*		PA2		D1				In/Out	To/from Oric protocol dependent
;*		PA3		D2				In/Out	To/from Oric protocol dependent
;*		PA4		D3				In/Out	To/from Oric protocol dependent
;*		PA5		Ready (ACK)		In		Set by Arduino (1=ACK)
;*		PA6		SD Select		Out		Set low by Oric
;*		PA7		SD Select		Out		Set low by Oric
;*		PB4		Select (STB)	Out		Set low by Oric
;*
;* Protocol is that PB4,6,7 must be low for SD card device
;* to respond. If PB6,7 are non zero, then it selects
;* one of the joysticks.
;* Protocol;
;* Write:
;* 0) Oric: Set PB4,6,7 low - D0-D3 as output
;* 1) Oric: Set D0-D3 as output (to send)
;* 2) Oric: Send write command byte
;* 3) Oric: Send additional bytes (e.g. filename)
;* 4) Oric: Set D0-D3 as output
;* 5) Oric: Send data bytes
;* 6) Oric: Set PB4,6,7 = high, D0-D3 as input
;* Read:
;* 0) Oric: Set PB4,6,7 low - D0-D3 as output
;* 1) Oric: Set D0-D3 as output (to send)
;* 2) Oric: Send write command byte
;* 3) Oric: Send additional bytes (e.g. filename)
;* 4) Oric: Set D0-D3 as input
;* 5) Oric: Receive data bytes
;* 6) Oric: Set PB4,6,7 = high, D0-D3 as input
;*
;* Commands
;* 0x00 : Load <filename><NUL>
;* 0x01 : Save <filename><NUL>
;**********************************************************

	; ROM code
	code

mod_sz_sd_s

SD_BIT_DELAY	=	2

;* Delay based on X register
sd_delay
	pha
	txa
	ldx #SD_BIT_DELAY
sd_delay_loop
	dex
	bne sd_delay_loop
	tax
	pla
	rts

; Release the Arduino
; Toggles the clock line a few times with STB high
; This causes the Arduino to reset
sd_release
	jsr via_strobe_off
;	ldy #10
;	lda IO_0+PRA;
;sd_reset_toggle
;	eor #1
;	sta IO_0+PRA;
;	jsr sd_delay
;	dey
;	bne sd_reset_toggle
	jsr sd_write_byte
	jsr sd_write_byte
	jmp init_via0


;* Select the Arduino
sd_select
	pha

	;set ddra to output mode initially
	lda #0b11011111
	jsr via_strobe_init

	;deselect joystick (and zero all other bits)
	lda #0b00000000
	sta IO_0+PRA

	jsr sd_delay
	pla
	rts


;* Wait for Arduino to be ready
;* Timeout results in an exception (BRK)
sd_ready
	ldx #10
	ldy #0
sd_notready
	iny							; Decrement X,Y timeout
	bne sd_ready_skipx
	dex
	beq sd_timeout				; If X is zero then timeout error
sd_ready_skipx
	lda IO_0+PRA				; Get port A value
	and #0b00100000				; Check bit 5
	beq sd_notready
	rts
sd_timeout
	SWBRK DFERR_FNAME


; Write nibble in bit 1..4 plus clock high
sd_write_nibble
	ldy #0b00000000
	sty IO_0+PRA				; Clock low, data low
	jsr sd_delay
	ora IO_0+PRA				; Set bits to transfer for nibble
	ora #1						; Clock high
	sta IO_0+PRA				; Send it to Port A
	jmp sd_delay


;* Write a byte in A to the Arduino
sd_write_byte
	sta tmp_d
	pha
	txa
	pha
	tya
	pha

	; Make sure arduino is ready
	jsr sd_ready	
	jsr sd_delay

	; Set DDRA to output from Oric
	lda #0b11011111
	sta IO_0+DDRA

	; Write low nibble
	lda tmp_d					; Get the byte to transmit
	and #0x0f					; Mask off lower nibble
	asl a 						; Shift from bit 0-3 to bit 1-4
	jsr sd_write_nibble

	; Write high nibble
	lda tmp_d					; Get the byte to transmit
	and #0xf0					; Mask off upper nibble
	lsr a						; Shift down to bit 1-4
	lsr a
	lsr a
	jsr sd_write_nibble

	pla
	tay
	pla
	tax

	pla
	rts


;* Read nibble from Arduino plus clock high
sd_read_nibble
	ldy #0b00000000
	sty IO_0+PRA				; Set clock low, data low
	jsr sd_delay				

	ora #1						; Clock bit high
	sta IO_0+PRA				; Store it

	lda IO_0+PRA				; Get bits 1..4
	and #0b00011110				; Mask these bits

	jmp sd_delay


sd_read_byte
	txa
	pha
	tya
	pha

	; Make sure arduino is ready
	jsr sd_ready	
	jsr sd_delay

	; Set DDRA to input to Oric
	lda #0b11000001
	sta IO_0+DDRA

	lda IO_0+PRA				; Get port A value
	and #0b11100000				; Ensure clock and data is low
	tay							; Y is clock zero and data zero

	; Read low nibble
	jsr sd_read_nibble
	lsr a						; Shift from bit 1 to bit 0
	and #0x0f					; Mask off lower nibble
	sta tmp_d					; Save in temp

	; Read high nibble
	jsr sd_read_nibble
	asl a						; Shift from bit 1 to bit 4
	asl a
	asl a
	and #0xf0					; Mask off upper nibble
	ora tmp_d					; Merge with lower nibble
	sta tmp_d

	pla
	tay
	pla
	tax

	lda tmp_d
	rts
	
;* Initialise SD card
;* Use TP flag to determine the command to send to Arduino
;* Send this command (read or write) plus the filename
sd_init
	ldy tp_flag				; 1=read, 2=write
	dey
	tya						; 0=read, 1=write for the Arduino
sd_init_a					; Init with A preset to command byte
	jsr sd_select
;* Send command and filename
sd_commandfname
	jsr sd_write_byte		; Send command byte (0=read, 1=write,3=delete)
	ldy #0					; Start of df_linbuff
sd_commandfname_ch
	lda df_linbuff,y		; Get filename char
	jsr sd_write_byte		; Send it as command frame
	iny						; Next char
	cmp #0					; Unless zero terminator
	bne sd_commandfname_ch
;* No delay for SD card
;* No block gap for SD card
sd_put_delay
sd_block_gap
	rts


;* Delete file from SD card
sd_delete
	lda #3					; Send Delete command
	jsr sd_init_a			; And send filename to delete
	jsr sd_ready			; Check SD card is ready else error
	jmp sd_release			; Then done

;* Ouput directory listing
sd_dir
	jsr sd_select
	lda #4					; Send directory command
	jsr sd_write_byte
	jsr sd_delay
sd_dir_char					; Keep getting bytes from Arduino and print to screen
	jsr sd_delay
	jsr sd_read_byte
	jsr gr_put_byte
	tax
	bne sd_dir_char			; Until zero terminator found
	jmp sd_release			; Then done
	


mod_sz_sd_e

