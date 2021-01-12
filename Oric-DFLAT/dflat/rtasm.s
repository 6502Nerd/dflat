;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  RTASM.S
;*  Module that implements the runtime execution of the
;*  assembler.
;*
;**********************************************************

	; ROM code
	code  

mod_sz_rtasm_s

df_rt_asm_assemble
	; check if >=0x80 (directive or mnemonic)
	ldy df_exeoff
	lda (df_currlin),y
	cmp #0x80
	bcc df_rt_asm_do_label
	jsr df_rt_asm_command
	jsr df_rt_asm_printCR
	jmp df_rt_asm_assemble_done
df_rt_asm_do_label
	jsr df_rt_asm_label
df_rt_asm_assemble_done
	; any clean up here
	rts

; Jump to mnemonic or 
; directive with index in A ignoring MSB
df_rt_asm_command
	and #0x7f
	pha
	tax
	; Point to Xth symtable entry of addr modes and op codes
	jsr df_rt_asm_skip_to_sym

	; Get first addressing mode (ignoring offset byte)
	ldy #1
	lda (df_symtab),y
	; is it a directive?
	cmp #AM_DIR
	; if directive
	beq df_rt_asm_dir
	; Else must be a normal mnemonic
	jmp df_rt_asm_mnem

; Jump to the appropriate directive
df_rt_asm_dir
	; Get the index and *2
	; to get jump vector
	pla
	asl a
	tax
	; jump over directive code
	inc df_exeoff
	; jump to directive handler
	lda df_rt_asm_tokenjmp,x
	sta df_tmpptra
	lda df_rt_asm_tokenjmp+1,x
	sta df_tmpptra+1
	jmp (df_tmpptra)

; Process label
; Set the label to the PC
df_rt_asm_label
	; Get the address of the variable in A,X
	jsr df_rt_generate_lvar
	stx df_asmoprnd
	sta df_asmoprnd+1
	; Assign to value of PC
	ldy #0
	lda df_asmpc
	sta (df_asmoprnd),y
	lda df_asmpc+1
	iny
	sta (df_asmoprnd),y
	rts	

;* Set the code origin
df_rt_asm_org
	; Get the address from expression
	jsr df_rt_asm_get_operand
	; Put this in to PC
	lda df_asmoprnd
	sta df_asmpc
	lda df_asmoprnd+1
	sta df_asmpc+1
	lda #3
	sta df_asmlen
	jsr df_rt_asm_printPC
	rts

; Set the assembly option
; Bit 0 set = Print code to console
; But 1 set = Write code to memory
; Be sure to run 2 non-write passes
; before a write pass
; First pass may result in ZP used a lot
; Second pass resolves forward refs to ABS
; Final write pass then stores the code
df_rt_asm_opt
	; Get the value from expression
	jsr df_rt_asm_get_operand
	; Put this in to OPT
	lda df_asmoprnd
	sta df_asmopt
	rts

; dw and db handled here
; keep going through comma separated
; list and write bytes or words
; string expansion also handled
df_rt_asm_db
	lda #2
	sta df_asmlen
	bne df_rt_asm_db_start	; Relies on A<>0
df_rt_asm_dw
	lda #3
	sta df_asmlen
df_rt_asm_db_start
	jsr df_rt_asm_printPC
	jsr df_rt_asm_printSPC
df_rt_asm_data
	jsr df_rt_neval		; Evaluate expression
	jsr df_ost_peekType	; What is the type
	cmp #0x80			; If string
	bcs df_rt_asm_string
	jsr df_ost_popInt	; else get number in A,X
	stx df_asmoprnd		; store low value
	sta df_asmoprnd+1	; store high value
	jsr df_rt_asm_data_write
	jmp df_rt_asm_data_next
df_rt_asm_string
	jsr df_ost_popPtr	; pop string pointer
	stx df_tmpptra		; save pointer to tmpa
	sta df_tmpptra+1
df_rt_asm_string_ch
	ldy #0
	lda (df_tmpptra),y	; Get string char, 0=done
	beq df_rt_asm_data_next
	sta df_asmoprnd		; Save as operand
	stx df_asmoprnd+1	; High is always zero
	jsr df_rt_asm_data_write
	_incZPWord df_tmpptra
	jmp df_rt_asm_string_ch
df_rt_asm_data_next
	jsr df_rt_eos		; End?
	bcs df_rt_asm_data_done
	inc df_exeoff		; Jump over ','
	jmp df_rt_asm_data
df_rt_asm_data_done
	rts	

; Write one db or dw value
; Check for value too big for db
; Advance PC by df_asmlen
df_rt_asm_data_write
	jsr df_rt_asm_printOPR
df_rt_asm_data_write_skip1
	lda #2
	and df_asmopt
	beq df_rt_asm_data_write_skip2
	lda df_asmoprnd
	ldy #0
	sta (df_asmpc),y
	iny
	lda #2
	cmp df_asmlen
	beq df_rt_asm_data_write_hi
	lda df_asmoprnd+1
	bne df_rt_asm_data_write_err
df_rt_asm_data_write_hi
	lda df_asmoprnd+1
	sta (df_asmpc),y
df_rt_asm_data_write_skip2
	lda df_asmlen
	sec					; Add 1 less!
	sbc #1
	clc
	adc df_asmpc
	sta df_asmpc
	lda df_asmpc+1
	adc #0
	sta df_asmpc+1
	rts
df_rt_asm_data_write_err	
	SWBRK DFERR_QUANTITY
	
	
df_rt_asm_ds
	jsr df_rt_asm_printPC
	jsr df_rt_asm_printSPC
	; Get the address from expression
	jsr df_rt_asm_get_operand
	; ADD this in to PC
	clc
	lda df_asmoprnd
	adc df_asmpc
	sta df_asmpc
	lda df_asmoprnd+1
	adc df_asmpc+1
	sta df_asmpc+1
	; len=1 for printing
	lda #3
	sta df_asmlen
	jsr df_rt_asm_printOPR
	rts


; Process a normal assembler mnemonic
df_rt_asm_mnem
	pla				; Throw away previous temp variable
	; jump over mnemonic code
	inc df_exeoff
	; work out the addressing mode and get operand
	jsr df_rt_asm_addrmode

df_rt_asm_mnem_try
	lda df_asmadmd					; With the addressing mode
	jsr df_rt_asm_find_addr_mode	; Find it for this instruction
	cmp #AM_NONE					; Exists?
	bne	df_rt_asm_mnem_chk			; If does then check it
df_rt_asm_check_alt
	ldx df_asmadmd					; Use ad mode as an index
	lda df_asm_altaddrmode,x		; Else get alternate
	cmp #AM_NONE					; Is there an alternate?
	beq df_rt_asm_mnem_err			; If no then error
	jsr df_rt_asm_find_addr_mode	; Check does mode exist for 
	cmp #AM_NONE					; this instruction
	beq df_rt_asm_mnem_err			; if no then error
	cmp df_asmadmd					; Same as the original?
	beq df_rt_asm_mnem_done			; if so then done
df_rt_asm_mnem_chk
	sta df_asmadmd					; This is the final mode
	tax								; Get the length
	lda df_asm_length,x
	ldx df_asmoprnd+1				; Is operand hi non-zero?
	beq df_rt_asm_mnem_done			; If zero then done
	cmp #3							; Else must be len 3?
	bne df_rt_asm_check_alt			; Try an alternate
df_rt_asm_mnem_done
	ldx df_asmadmd					; Get the final mode index
	lda df_asm_length,x				; Get the length
	sta df_asmlen
	lda df_asmadmd					; Get the final mode
	jsr df_rt_asm_find_addr_mode	; Get the Y index of ad mode
	; Ok now get the opcode
	iny
	lda (df_symtab),y
	sta df_asmopcde
	; Now have all information to assemble
	jsr df_rt_asm_encode
	
	rts
df_rt_asm_mnem_err
	SWBRK DFERR_SYNTAX
	
; Take assembler data and encode it
; depending on the current option
; Option 0 = No write
; Option 1 = No write, Print
; Option 2 = Write
; Option 3 = Write, Print
df_rt_asm_encode
	; If relative then need to calculate offset
	lda df_asmadmd
	cmp #AM_REL
	bne df_rt_asm_encode_skiprel
	; If high byte is 0 then do nothing
	lda df_asmoprnd+1
	beq df_rt_asm_encode_skiprel
	; else calculate distance from PC
	; first take 2 off operand
	sec
	lda df_asmoprnd
	sbc #2
	sta df_asmoprnd
	lda df_asmoprnd+1
	sbc #0
	sta df_asmoprnd+1
	; now calculate current operand-PC
	sec
	lda df_asmoprnd
	sbc df_asmpc
	sta df_asmoprnd
	lda df_asmoprnd+1
	sbc df_asmpc+1
	; put 0 in high operand storage
	; but A contains result of subtraction
	; so check that for out of range
	ldy #0
	sty df_asmoprnd+1
	; detect too far; high byte is either 0 or 255
	; else it's an error
	cmp #0
	beq df_rt_asm_encode_relpos
	cmp #0xff
	bne df_rt_asm_encode_relfar
	; if high is ff then low must be same -ve
	lda df_asmoprnd
	bmi df_rt_asm_encode_skiprel
	; else error
	bpl df_rt_asm_encode_relfar
df_rt_asm_encode_relpos
	; if high is 00 then low must be same +ve
	lda df_asmoprnd
	bpl df_rt_asm_encode_skiprel
	; else error
df_rt_asm_encode_relfar
	; set to non-zero value
	lda #0xff
	sta df_asmoprnd+1
df_rt_asm_encode_skiprel
	; Only write the code if bit 1=1
	lda #0x02
	and df_asmopt
	beq df_rt_asm_encode_print
	ldy #0
	lda df_asmopcde
	sta (df_asmpc),y
	lda df_asmlen
	cmp #1						; No operand
	beq df_rt_asm_encode_print
	cmp #3						; Word operand
	beq df_rt_asm_encode_writeword
	; byte operand, high byte must be zero
	lda df_asmoprnd+1
	beq df_rt_asm_encode_writebyte
	SWBRK DFERR_QUANTITY
df_rt_asm_encode_writebyte
	iny 
	lda df_asmoprnd
	sta (df_asmpc),y
	jmp df_rt_asm_encode_print
df_rt_asm_encode_writeword
	iny 
	lda df_asmoprnd
	sta (df_asmpc),y
	iny 
	lda df_asmoprnd+1
	sta (df_asmpc),y	
df_rt_asm_encode_print
	jsr df_rt_asm_print_mmen
	; advance pc
	clc
	lda df_asmpc
	adc df_asmlen
	sta df_asmpc
	lda df_asmpc+1
	adc #0
	sta df_asmpc+1
	clc
	rts


; Print the full 1,2,3 byte instruction
; depends on df_asmopt
df_rt_asm_print_mmen
	jsr df_rt_asm_printPC
	jsr df_rt_asm_printSPC
	jsr df_rt_asm_printOPC
	jsr df_rt_asm_printSPC
	jsr df_rt_asm_printOPR
df_rt_asm_print_rts			; Hopefull subs can get here
	rts

df_rt_asm_printPC
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts
	; Print current PC
	lda df_asmpc+1
	jsr utilPrintA
	lda df_asmpc
	jsr utilPrintA
	rts

df_rt_asm_printOPC
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts
	lda df_asmopcde
	jsr utilPrintA
	rts

df_rt_asm_printSPC
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts
	jsr utilPrintSPC
	rts
	
	
df_rt_asm_printOPR
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts	
	lda df_asmlen				; check how may operand bytes
	cmp #1						; if only opcode, done
	beq df_rt_asm_print_rts	
	cmp #2
	beq df_rt_asm_printOPR_1	; if only 2 do low byte only
	lda df_asmoprnd+1
	jsr utilPrintA
df_rt_asm_printOPR_1
	lda df_asmoprnd
	jsr utilPrintA
	rts

df_rt_asm_printCR
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts
	jsr utilPrintCRLF
	rts

df_rt_asm_printCH
	sta tmp_d
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts
	lda tmp_d
	jmp io_put_ch
	rts

; Print an entire line, but save df_exeoff
df_rt_asm_printline
	; Check the option bit 0 (Print)
	lda #0x01
	and df_asmopt
	beq df_rt_asm_print_rts
	lda df_exeoff
	pha
	; use df_tmpptra
	_cpyZPWord df_currlin, df_tmpptra
	jsr df_rt_list_line_only
	pla
	sta df_exeoff
	rts


; Calculate the addressing mode
; Populate the operand as needed
; A and df_asmadmd contain Addressing Mode
df_rt_asm_addrmode	
	; Store nothing in address mode
	lda #AM_NONE
	sta df_asmadmd
	; zero out the operand
	lda #0
	sta df_asmoprnd
	sta df_asmoprnd+1
	; jump over whitespace
	jsr df_rt_skip_ws
	jsr df_rt_eos			; End of statement?
	bcs df_rt_asm_AM_IMP
	; Check what it is
	cmp #'#'				; Immediate?
	beq df_rt_asm_AM_IMM
	cmp #'('				; Indirect something?
	beq df_rt_asm_AM_INDIRECT
	; Must be ABS,ZP or REL
	bne df_rt_asm_ABSREL
; Process IMP/ACC
df_rt_asm_AM_IMP
	lda #AM_IMP
	sta df_asmadmd
	rts
; Process ABS or REL
df_rt_asm_ABSREL	
	; Evaluate operand
	jsr df_rt_asm_get_operand
	jsr df_rt_eos			; End of statement?
	bcs df_rt_asm_AM_ABS
	iny						; Jump over ","
	lda (df_currlin),y		; Load X or Y
	iny						; Jump over index reg
	sta df_exeoff			; Save exe offset
	cmp #'x'				; Indirect X?
	beq df_rt_asm_AM_ABSX
; Process ABSY
df_rt_asm_AM_ABSY
	lda #AM_ZPY				; Go for smallest modes
	sta df_asmadmd
	rts
; Process what looks like pure ABS, could be REL
df_rt_asm_AM_ABS
	ldy #1					; Check first addressing mode
	lda (df_symtab),y
	cmp #AM_REL
	beq df_rt_asm_AM_REL
	lda #AM_ZP				; Go for smallest modes
df_rt_asm_AM_REL
	sta df_asmadmd
	rts
; Process ABSX
df_rt_asm_AM_ABSX
	lda #AM_ZPX				; Go for smallest modes
	sta df_asmadmd
	rts
; Process Immediate
df_rt_asm_AM_IMM
	; skip over #
	inc df_exeoff
	; Get operand
	jsr df_rt_asm_get_operand
	; Mark as immediate
	lda #AM_IMM
	sta df_asmadmd
	rts
; Process indirect
df_rt_asm_AM_INDIRECT
	; skip over (
	inc df_exeoff
	; Calculate the operand
	jsr df_rt_asm_get_operand
	; what is next char?
	ldy df_exeoff
	lda (df_currlin),y
	cmp #')'			; Could be IND or INDY
	beq df_rt_asm_AM_ZPINDORY
	; else must be INDX
	; skip 'x)'
	iny
	iny
	sty df_exeoff
	lda #AM_ZPINDX
	sta df_asmadmd
	rts
; Pure indirect mode found
df_rt_asm_AM_ZPINDORY
	lda #AM_ZPIND			; Assume ZPIND
	iny
	sty df_exeoff
	; if at end of line/statement then pure indirect
	jsr df_rt_eos			; End of statement?
	bcs df_rt_asm_AM_ZPIND
	; else must be INDY
	lda #AM_ZPINDY
	; skip ',Y'
	iny
	iny
	sty df_exeoff
df_rt_asm_AM_ZPIND
	sta df_asmadmd
	rts

; Get and save the operand
; df_asmoprnd contains the result
df_rt_asm_get_operand
	; evaluate
	jsr df_rt_neval
	; Get the parameter from stack
	jsr df_ost_popInt
	; put in the operand
	stx df_asmoprnd
	sta df_asmoprnd+1
	rts
	
; From current symtab entry, find addressing
; mode in A.  AM_NONE=Not found, Y=index
df_rt_asm_find_addr_mode
	; save A in tmp
	sta tmp_d
	; Start first entry (1) - 2 = 0xff
	ldy #0xff
	ldx #0
df_rt_asm_find_addr_mode_loop
	; next entry
	iny
	iny
	; if current index > num entries then error
	tya
	sec
	sbc (df_symtab,x)
	bcs df_rt_asm_find_addr_mode_err
	; is symtab addressing mode what we want?
	lda (df_symtab),y
	cmp tmp_d
	bne df_rt_asm_find_addr_mode_loop
	; done A=mode, y=index in to df_symtab
	rts
df_rt_asm_find_addr_mode_err
	lda #AM_NONE
	rts


; Skip X amount of symbols in table
; A contains how many addr mode and op code bytes
; df_symtab points to offset byte
df_rt_asm_skip_to_sym
	inx				; so done when X=0
	; Start at token symbols beginning
	lda #lo(df_asm_tokensyms)
	sta df_symtab
	lda #hi(df_asm_tokensyms)
	sta df_symtab+1
	lda #0
	sta df_symoff
	ldy #0
df_rt_asm_skip_to_sym_next
	; Get symtable char
	lda (df_symtab),y
	; if < ' ' then jumped over symbol chars
	cmp #' '
	bcc df_rt_asm_skip_to_sym_end
	;else next smy tab char
	_incZPWord df_symtab
	jmp df_rt_asm_skip_to_sym_next
	; Found end of symbol
df_rt_asm_skip_to_sym_end
	;if done then return
	dex
	beq df_rt_asm_skip_to_sym_done
	;jump over addr and op code by A bytes
	sec
	adc df_symtab
	sta df_symtab
	lda df_symtab+1
	adc #0
	sta df_symtab+1
	jmp df_rt_asm_skip_to_sym_next
	
df_rt_asm_skip_to_sym_done
	rts

mod_sz_rtasm_e

