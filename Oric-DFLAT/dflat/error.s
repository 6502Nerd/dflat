;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  ERROR.S
;*  Error handling module.
;*  Whan an error is thrown using BRK, this module handles
;*  displaying the error plus any associated line number
;*  if it was running a program.  It then resets necessary
;*  settings and takes the system back to program edit
;*  mode.  The message uses the general IO handler, thus
;*  output must be set to the right place else for example
;*	the error message will be written to tape!
;*
;**********************************************************

	; ROM code
	code  
	include "dflat\error.i"
	
	
; Error message table, each msg null terminated
df_tk_errortab
	db	"Ok", 0
	db	"Syntax", 0
	db	"Type", 0
	db	"Re-dim", 0
	db	"No repeat", 0
	db	"No defn", 0
	db	"Parm", 0
	db	"Ended", 0
	db	"No endif", 0
	db	"No if", 0
	db	"No for", 0
	db	"Not found", 0
	db	"Too long", 0
	db	"Break", 0
	db	"No data", 0
	db	"No while", 0
	db	"No line", 0
	db	"No return ", 0
	db	"Aborted", 0
	db	"Bounds", 0
	db	"No org", 0
	db	0

df_tk_error_inline
	db	" in line ", 0
df_tk_error_atpos
	db	" pos ", 0
df_tk_error_error
	db	" error", 0

;****************************************
;* df_trap_error
;* Show an error message
;* errno is error number
;* currlin = Line number
;* exeoff = offset
;* at the end jump to program editor
;****************************************
df_trap_error
	; reset SP
	ldx df_sp
	txs
	; set IO back to normal
	jsr init_via0
	jsr io_set_default
	cli
	
	lda #lo(df_tk_errortab)
	sta df_tmpptra
	lda #hi(df_tk_errortab)
	sta df_tmpptra+1
	ldx errno				; 0 or >=128 goes to monitor
	beq df_trap_go_monitor
	bmi df_trap_go_monitor
	bpl df_trap_normal
df_trap_go_monitor
	jmp df_trap_monitor
df_trap_normal
	ldy #0
df_show_err_find
	cpx #0
	beq df_show_err_found
	; If on a zero, then error table exhausted
	; so drop in to the monitor
	lda (df_tmpptra),y
	beq df_trap_monitor
df_show_err_skip
	_incZPWord df_tmpptra
	lda (df_tmpptra),y
	bne df_show_err_skip
	_incZPWord df_tmpptra
	dex
	jmp df_show_err_find
df_show_err_found
	ldx df_tmpptra
	lda df_tmpptra+1
	jsr io_print_line
	ldx #lo(df_tk_error_error)
	lda #hi(df_tk_error_error)
	jsr io_print_line
	; if line number <> 0 then print it
	ldy #DFTK_LINNUM
	lda (df_currlin),y
	tax
	iny
	lda (df_currlin),y
	cmp #0x00
	bne df_show_err_linnum
	cpx #0x00
	bne df_show_err_linnum
	beq df_show_err_fin
df_show_err_linnum
	_println df_tk_error_inline
	clc
	jsr print_a_to_d
df_show_err_fin
	ldy df_exeoff
	beq df_show_err_done
	_println df_tk_error_atpos
	tya
	tax
	lda #0
	clc
	jsr print_a_to_d	
df_show_err_done
	lda #UTF_CR
	jsr io_put_ch
	clc
	; back to editor
	jmp df_pg_dflat


; For unknown errors, jump to monitor
df_trap_monitor
	; Print PC
	_println_low df_msg_pc
	lda df_brkpc+1
	jsr utilPrintA
	lda df_brkpc
	jsr utilPrintA
	jsr utilPrintSPC

	; Print A
	_println_low df_msg_acc
	lda num_a
	jsr utilPrintA
	jsr utilPrintSPC

	; Print X
	_println_low df_msg_xreg
	lda num_a+1
	jsr utilPrintA
	jsr utilPrintSPC

	; Print Y
	_println_low df_msg_yreg
	lda num_a+2
	jsr utilPrintA
	jsr utilPrintCRLF
	
	jsr df_rt_monitor
	; back to editor
	jmp df_pg_dflat

df_msg_pc
	db "PC:\x0"
df_msg_acc
	db "A:\x0"
df_msg_xreg
	db "X:\x0"
df_msg_yreg
	db "Y:\x0"
