;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  DFLAT.S
;*  This is the main controller code file for dflat.
;*  This file includes all the required dflat source files
;*  needed:
;*  - error.s is the error handling module
;*  - var.s is the variable handling module
;*  - tokenise.s is the tokenisation module
;*  - progedit.s is the program editing module
;*  - runtime.s is the runtime module
;*  - stack.s is the stack handling module
;*  The above modules include further source files as
;*  needed.
;*
;**********************************************************

	; ROM code
	code

mod_sz_dflat_s

;	include "dflat/error.s"  ** included in the main bank
	include "dflat/var.s"
	include "dflat/tokenise.s"
	include "dflat/progedit.s"
	include "dflat/runtime.s"
	include "dflat/stack.s"

;****************************************
;* df_init
;* Initialise dflat language settings
;****************************************
df_init
	; Initialise top of memory to default
	; This can be overridden by himem command
	lda #lo(DF_MEMTOP)
	sta df_memtop
	lda #hi(DF_MEMTOP)
	sta df_memtop+1

	; Init program space
	jsr df_clear

	; Initialise assembler
;	jsr asm_init

	rts


;****************************************
;* df_clear
;* Initialise program space
;****************************************
df_clear
	; Start of program space
	lda #lo(DF_PROGSTART)
	sta df_prgstrt
	sta df_prgend
	lda #hi(DF_PROGSTART)
	sta df_prgstrt+1
	sta df_prgend+1
	; Terminal value in prog space
	lda #0
	ldy #0
	sta (df_prgstrt),y
	; No variables - zero the count
	sta df_varcnt

	; Variable value table
	; Grows down from mem top
	lda df_memtop
	sta df_vvtstrt
	sta df_vvtend
	lda df_memtop+1
	sta df_vvtstrt+1
	sta df_vvtend+1

	; Variable name table
	; Grows down from mem top
	lda df_vvtstrt
	sta df_vntstrt
	sta df_vntend
	lda df_vvtstrt+1
	sta df_vntstrt+1
	sta df_vntend+1

	; String accumulator
	lda #lo(df_raw)
	sta df_sevalptr
	lda #hi(df_raw)
	sta df_sevalptr+1

	rts

mod_sz_dflat_e
