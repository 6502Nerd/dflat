;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  ASM.S
;*	Module that implements tokenisation of the assembler.
;* 	The assembler is an extension to dflat, almost a
;*	almost a language within a language, as it has its
;*	own symbol tables to allow adding new keywords without
;*	running out of space in the main symtol table and also
;*	for normal 6502 asembler syntax which is at odds with
;*	regular dflat.
;*	Hence, this module has to do its own additional parsing
;*	to tokenise the assembly.
;*
;**********************************************************

	; ROM code
	code  

mod_sz_tkasm_s

	; if didn't find regular keywords then try assenbler
df_tk_asm_parse_command
	; skip the white space after the dot
	jsr df_tk_skip_ws
	; find the assembler symbol
	jsr df_tk_asm_matchtok
	; if not found then must be a label assignment
	bcs df_tk_asm_parse_command_symbol
	; Get the assembler symbol and put the token
	lda df_symoff
	; Set the MSB
	ora #0x80
	jsr df_tk_put_tok
	; check the first addressing mode code
	; remember to jump over the offset byte
	ldy #1
	lda (df_symtab),y
	; Directive?
	cmp #AM_DIR
	bne df_tk_asm_mnemonic
	; if directive then process it
	lda df_symoff
	jsr df_tk_asm_exec_parser
	bcs df_tk_asm_parse_command_err
	; [1] ignore white space but keep it
	jsr df_tk_skip_ws
	; No error in parsing this command
	clc
df_tk_asm_parse_command_err
	rts
df_tk_asm_mnemonic
	; for all nmemonics, work out the addressing mode
	jsr df_tk_asm_addr_mode
	bcs df_tk_asm_parse_command_err
	rts
df_tk_asm_parse_command_symbol
	; No mask
	lda #0
	jmp df_tk_var


;****************************************
;* df_tk_asm_matchtok
;* Try and find a token match against the table df_asm_tokensyms
;* Input:
;*			Current df_linbuff and df_linoff
;* Return: 	CC = No Error, CS = Error
;*			df_linoff points to next char if CC else unchanged
;****************************************
df_tk_asm_matchtok
	; Start at token symbols beginning
	lda #lo(df_asm_tokensyms)
	sta df_symtab
	lda #hi(df_asm_tokensyms)
	sta df_symtab+1
	lda #0
	sta df_symoff
	ldx #0
df_tk_asm_checknexttok
	; From the line buffer current pointer
	; Check for a token match
	ldy df_linoff
df_tk_asm_checktokch
	; Get symtable char
	lda (df_symtab,x)
	; if less than ascii ' ' then reached end of
	; this symbol and everything matched so found!
	cmp #' '
	bcc df_tk_asm_symfound
	; Else compare with current line buffer char
	cmp df_linbuff,y
	; If chars not match then this symbol fails
	bne df_tk_asm_symnomatch
	; else more chars to match
	; so increment line buffer pointers
	_incZPWord df_symtab
	iny
	jmp df_tk_asm_checktokch
df_tk_asm_symnomatch
	; Increment symbol counter to next symbol
	inc df_symoff
df_tk_asm_symnextentry
	lda (df_symtab,x)
	; End of symbol is < ' '
	cmp #' '
	bcc  df_tk_asm_foundsymend
	_incZPWord df_symtab
	jmp df_tk_asm_symnextentry
df_tk_asm_foundsymend
	; Now at the offset to jump over addressing
	; mode and opcode values. Add offset to ptr
	; remember than C is clear and A has offset
	; set C so always jump over the offset
	sec
	adc df_symtab
	sta df_symtab
	lda df_symtab+1
	adc #0
	sta df_symtab+1
	; If next char is not zero then
	; try and match with line buffer
	lda (df_symtab,x)
	bne df_tk_asm_checknexttok
	; else symbol table exhausted
	; so no match found
	; Zero symbol counter
	sta df_symoff		; Relies on A=0
	; Set C to indicate error (no match)
	sec
	rts
df_tk_asm_symfound
	; Full match with keyword in symtable but
	; check the next buffer char is not alphanum
	; as this could be part of a symbol
	; Point to next buffer char in any case
	; y already points to char after keyword
	lda df_linbuff,y
	; if it is an alphanum then
	jsr df_tk_isalphanum
	; do not count as a match
	bcs df_tk_asm_symnomatch
	; Save line buffer pointer (points to next char)
	; Clear C to indicate success (match)
	sty df_linoff
	lda df_symoff
	; df_symtab points to the offset
df_tk_asm_addr_mode_ok
	clc
	rts

;****************************************
;* df_tk_asm_exec_parser
;* Execute parse routine for this statement
;* Input: a is the token found
;* Return: CC = Parsed ok, CS = Error
;****************************************
df_tk_asm_exec_parser
	asl a
	tax
	lda df_tk_asm_tokenjmp,x
	sta df_tmpptra
	lda df_tk_asm_tokenjmp+1,x
	sta df_tmpptra+1
	jmp (df_tmpptra)


;****************************************
;* df_tk_asm_addr_mode
;* Tokenise the addressing mode
;* Input: a is the token found
;* Return: CC = Parsed ok, CS = Error
;****************************************
df_tk_asm_addr_mode
	jsr df_tk_skip_ws		;Skip whitespace
	cmp #0					;End of line?
	beq df_tk_asm_addr_mode_ok
	cmp #':'				;End of statement?
	beq df_tk_asm_addr_mode_ok
	lda #'#'				; Check for immediate
	jsr df_tk_expect_tok
	bcs df_tk_asm_addr_mode_1
	jmp df_tk_imm		; Process immediate
df_tk_asm_addr_mode_1
	lda #'('				; One of indirect modes?
	jsr df_tk_expect_tok
	bcs df_tk_asm_addr_mode_2
	jmp df_tk_indirect
df_tk_asm_addr_mode_2
	; here just a regular address
	; Just one expression expected
	jsr df_tk_expression
	; Is there a comma after the expression
	lda #','
	jsr df_tk_expect_tok
	bcs df_tk_asm_addr_mode_ok
	; must be x or y
	lda #'x'
	jsr df_tk_expect_tok
	bcc df_tk_asm_addr_mode_ok
	; got to here, must be y
	lda #'y'
	jmp df_tk_expect_tok_err
	
	
;* Tokenise immediate addressing mode	
df_tk_imm
	; Just one expression expected
	jmp df_tk_expression
	
;* Tokenise indirect
df_tk_indirect
	; Just one expression expected
	jsr df_tk_expression
	; Is there a comma after the expression
	lda #','
	jsr df_tk_expect_tok
	; if not then might be indirect Y
	bcs df_tk_indirect_y
	; Else must have be indirect "x)"
	lda #'x'
	jsr df_tk_expect_tok_err
	lda #')'
	jmp df_tk_expect_tok_err
	
df_tk_indirect_y
	; Definitely has a close bracket
	lda #')'
	jsr df_tk_expect_tok_err
	; Is there a comma after the expression
	lda #','
	jsr df_tk_expect_tok
	bcs df_tk_indirect_done
	; if comma then must be indirect y
	lda #'y'
	jmp df_tk_expect_tok_err

df_tk_indirect_done
	clc
	rts
	

	


;**************************************************
;*           TOKENISATION SUBROUTINES
;**************************************************


;* HANDLE DIRECTIVES

;* All tokenisation is part of regular toksub routines




























mod_sz_tkasm_e

