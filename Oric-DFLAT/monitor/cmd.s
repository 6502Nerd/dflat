;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  CMD.S
;*	A really simple monitor that needs to be improved and
;*	optimised.  Only has these commands;
;*		dxxxx			Dump memory location xxxx
;*						Press enter for more else stop
;*		sxxxx [yy]		Set memory location xxxx with hex
;*						bytes, respond with next location
;*		q				Quit
;*	Now you see why this needs optimising!
;*
;**********************************************************

cmd_lo					= df_currdat
cmd_hi					= (df_currdat+1)

	; ROM code
	code  

_mod_sz_cmd_s

command_line

cmd_ready
	_println_low msg_ready

	sec							; Set carry flag = echo characters
	jsr io_read_line			; Get a command line
	ldy #0
	lda (buf_lo),y
	cmp #'q'
	bne cmd_check_cmd
	rts
cmd_check_cmd
	jsr cmd_parse				; Find command and execute
	bcs cmd_error				; Carry set = error condition
	jmp cmd_ready

cmd_error
	_println_low msg_errmsg
	jmp cmd_ready


;****************************************
;* cmd_parse
;* Parse the command line in the io buffer
;* Regs affected : 
;****************************************
cmd_parse
	iny						; Ready for next char
	
cmd_check_d
	cmp #'d'				; Check for d
	bne cmd_check_s
	jmp cmd_dumpmem
cmd_check_s
	cmp #'s'				; Check for s
	bne cmd_error
	jmp cmd_setmem

cmd_not_found
	lda #CMD_ERR_NOTFOUND
	sta errno
	sec
	rts
	
;****************************************
;* cmd_setmem
;* Set the memory at address AAAA to byte string
;* Input : buflo, bufhi
;* Output : y = start of first parm byte
;*          x = index to routine pointer
;* Regs affected : A
;****************************************
cmd_setmem
	jsr cmd_parse_word
	bcs cmd_setmem_err
cmd_setmem_byte
	jsr cmd_parse_byte
	bcs cmd_setmem_err
	jsr cmd_poke				; Poke A in to cmd_lo, hi
	jsr cmd_incmem
	jsr cmd_parse_next_parm		; Try and find another parm
	bcc cmd_setmem_byte			; Process if found, else fin
	lda cmd_hi
	jsr utilPrintA
	lda cmd_lo
	jsr utilPrintA
	jsr utilPrintCRLF
	clc
	rts
cmd_setmem_err
	sec
	rts


;****************************************
;* cmd_dumpmem
;* Dump memory at address AAAA
;* Input : buflo, bufhi
;* Output : y = start of first parm byte
;* Regs affected : A
;****************************************
cmd_dumpmem
	jsr cmd_parse_word			; Get address to dump
	bcc cmd_dumpmem_block
	rts							; C is set
cmd_dumpmem_block
	lda cmd_hi					; Show the address
	jsr utilPrintA
	lda cmd_lo
	jsr utilPrintA
	lda #' '
	jsr io_put_ch
	
	ldy #8						; 8 Bytes per line
cmd_dumpmem_byte
	jsr cmd_peek
	jsr utilPrintA
	jsr utilPrintSPC
	jsr cmd_incmem
	dey
	bne cmd_dumpmem_byte
cmd_dumpmemASCII
	sec							; Move pointer back to start
	lda cmd_lo
	sbc #8
	sta cmd_lo
	lda cmd_hi
	sbc #0
	sta cmd_hi
	
	jsr utilPrintSPC
	ldy #8						; 8 Bytes per line
cmd_dumpmem_ascii
	ldx #'.'					; Non-printable char
	jsr cmd_peek
	cmp #' '					; <32 is unprintable
	bcs cmd_dump_skip_ctrl
	txa							; Replace with dot
cmd_dump_skip_ctrl
	cmp #UTF_DEL				; >= DEL is unprintable
	bcc cmd_dump_skip_del
	txa							; Replace with dot
cmd_dump_skip_del	
	jsr io_put_ch
	jsr cmd_incmem
	dey		
	bne cmd_dumpmem_ascii		; Show 8 bytes
	sec
	jsr io_get_ch				; Wait for key press
	cmp #UTF_CR		
	bne cmd_dumpmemFin			; Any key but enter finishes dump
;	jsr io_put_ch				; Do new line
	jmp cmd_dumpmem_block
cmd_dumpmemFin
	lda #UTF_CR
	jsr io_put_ch
	clc
	rts
	
	
;****************************************
;* cmd_incmem
;* Increment pointer
;* Input : cmd_lo, cmd_hi
;* Output : cmd_lo, cmd_hi
;* Regs affected : 
;****************************************
cmd_incmem
	_incZPWord cmd_lo
	rts
	
;****************************************
;* cmd_peek
;* Read byte
;* Input : cmd_lo, cmd_hi
;* Output : A
;* Regs affected : 
;****************************************
cmd_peek
	stx tmp_d
	ldx #0
	lda (cmd_lo,x)
	ldx tmp_d
	rts
	
;****************************************
;* cmd_poke
;* Read byte
;* Input : cmd_lo, cmd_hi, A
;* Output : A
;* Regs affected : 
;****************************************
cmd_poke
	stx tmp_d
	ldx #0
	sta (cmd_lo,x)
	ldx tmp_d
	rts

;****************************************
;* cmd_parse_byte
;* Find 2 char hex byte
;* Input : buflo, bufhi, y offset
;* Output : y = char after hex byte, A = value
;* Regs affected : A,Y
;****************************************
cmd_parse_byte
	jsr cmd_parse_next_parm	; Find the next parameter
	bcs cmd_parse_byte_err
	lda (buf_lo),y			; Get hi nibble of high byte
	beq cmd_parse_byte_err	; If no char then error with C set
	pha						; Save on stack
	iny
	lda (buf_lo),y			; Get lo nibble of high byte
	beq cmd_parse_byte_errp	; If no char then error with C set
	tax						; Lo nibble goes to X
	pla						; Restore hi nibble
	jsr str_x_to_a			; Convert from hex to A
	bcs cmd_parse_byte_err	; If error then stop
	iny						; Point to next char
	clc
	rts						; A contains the byte
cmd_parse_byte_errp
	pla
cmd_parse_byte_err
	tax
	rts

;****************************************
;* cmd_parse_word
;* Find 4 char hex word
;* Input : buflo, bufhi, y offset
;* Output : y = char after hex byte, A = hi, X = lo value
;* Regs affected : A,X,Y
;****************************************
cmd_parse_word
	jsr cmd_parse_byte			; Get hi byte of word
	bcs cmd_word_err
	sta cmd_hi					; Save hi byte of word
	jsr cmd_parse_byte			; Get lo byte of word
	bcs cmd_word_err
	sta cmd_lo
	clc
	rts
cmd_word_err
	sec
	rts

;****************************************
;* cmd_parse_next_parm
;* Find next non-white space
;* Input : buflo, bufhi, y offset
;* Output : y = start of first parm byte
;* Regs affected : A
;****************************************
cmd_parse_next_parm
	dey
cmd_find_parm
	iny
	lda (buf_lo),y
	cmp #0
	beq cmd_next_parm_err	; If z then no parms, fin with C set
	cmp #' '				; Ignore space
	beq cmd_find_parm
	clc						; else ok, C is cleared
cmd_next_parm_err
	rts

	
msg_ready				db ">\x0"
msg_errmsg				db "?\xd\x0"
_mod_sz_cmd_e
