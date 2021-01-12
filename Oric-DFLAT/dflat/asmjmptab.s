;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  ASMJUMPTAB.S
;*  Runtime token jump table for assembler.
;*  dflat uses four key tables to tokenise and run programs:
;*  - df_tokensyms    - table of token symbols
;*  - df_tk_tokentype - table of token types
;*  - df_tk_tokenjmp  - table of tokenising routines
;*  - df_rt_tokenjmp  - table of runtime routines
;*  The key is the token symbols.  When a line is entered
;*  in to the raw (untokenised) buffer, df_tokensyms is
;*  used to identify tokens.  The position of the found
;*  token is used to then look up type and jump vectors
;*  in the other tables.
;*
;**********************************************************

	; ROM code
	code  

; Tokeniser jump table
; Only directives needed as all opcodes are handled
; through a single routine
df_tk_asm_tokenjmp
	dw	df_tk_asm_org
	dw	df_tk_asm_opt
	dw	df_tk_asm_db
	dw	df_tk_asm_dw
	dw	df_tk_asm_ds

df_rt_asm_tokenjmp
	dw	df_rt_asm_org
	dw	df_rt_asm_opt
	dw	df_rt_asm_db
	dw	df_rt_asm_dw
	dw	df_rt_asm_ds
