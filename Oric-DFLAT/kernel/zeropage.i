;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  ZEROPAGE.I
;*  This module name is misleading it is not only zero page
;*  allocations, but also page 2, 3, 4, 5, 6, 7 and beyond
;*  memstart is a handy label that indicates the first
;*  location that we can store dflat programs from.
;*  Zero page is a valuable asset as the 6502 can access
;*  this page one cycle quicker than the rest of memory
;*  and infact some addressing modes can only use ZP.
;*  Due to the value of zero page, a lot of system and
;*  dflat variables are put here.  But we don't have the
;*  luxury for single use variables - so you will also
;*  see a lot of temporary sounding names which are
;*  have multiple uses across the code base.
;*
;**********************************************************

	; Zero page declarations
	bss
	org 0x0000

tmp_bank1	ds	1		; Temp storage ONLY FOR USE BY BANK SWITCHING
tmp_bank2	ds	1		; Temp storage ONLY FOR USE BY BANK SWITCHING

; Interrupt routine addresses
vec_irq		 ds	2		; Master IRQ handler
vec_brk		 ds	2		; Master BRK handler
vec_usercia0 ds	2		; Where to jump for CIA0 interrupt

; VDP parameters
vdp_cnt		ds	1		; VDP interrupt counter
vdp_cnt_hi 	ds	1		; VDP counter high
vdp_cnt_hi2	ds	1		; VDP counter high 2
vdp_curtim	ds	1		; Cursor blink speed
vdp_curcnt	ds	1		; Cursor blink countdown
vdp_curoff	ds	1		; Cursor off (0 = On)
vdp_curstat	ds	1		; Cursor status
vdp_curval	ds	1		; Cursor value on screen
vdp_blank	ds	1		; Screen blank value normally 32

; Screen geometry
gr_scrngeom	ds	gr_screen

;Keyboard parameters
kb_raw  	ds	1		; Raw keyboard code
kb_last		ds	1		; Code of last key
kb_code 	ds	1		; Converted keyboard code
kb_stat		ds	1		; Keyboard status for caps and shift lock
kb_deb		ds	1		; Debounce timer
kb_deb_tim  ds	1		; Default debounce delay
kb_rep		ds	1		; Keyboard repeat speed timer
kb_rep_tim 	ds	1		; Default repeat speed
kb_rdel_tim	ds	1		; Default repeat delay (until starts repeating)

tmp_alo 	ds	1		; VDP addresses lo
tmp_ahi 	ds	1		; VDP addresses hi
tmp_blo 	ds	1		; Temp address lo
tmp_bhi		ds	1		; Temp address hi
tmp_clo		ds	1		; Temp address lo
tmp_chi		ds	1		; Temp address hi
tmp_d		ds	1		; Temp storage d

; Raw input/output parameters
buf_lo		ds	1		; Line buffer address low
buf_hi		ds	1		; Line buffer address high
buf_sz		ds	1		; Buffer size
buf_ef		ds	1		; End file / line marker


; ** Integer function storage **
ztmp_24					; Start of 24 byte scratch area (all of integer temp storage)
num_a		ds	4		; 4 byte primary accumulator
num_b		ds	4		; 4 byte secondary accumulator
num_x		ds	4		; 4 byte x register
num_tmp		ds	4		; 4 byte temp space
num_buf		ds	8		; 8 byte string buffer

; ** Tape function storage **
tp_block	ds	2		; Block number (int)
tp_idx		ds	1		; Current buffer index
tp_flag		ds	1		; zero = closed, 1=read, 2=write
tp_delay	ds	2		; Interblock delay

;
; **** INTERPRETER ZERO PAGE ****
;
dflat_zp_s
dflat_zp_save_s			; ZP save dflat from here
df_checkkey	ds	1		; Key check interval counter
df_checkmsk	ds	1		; Mask for check key
errno		ds	1		; General error condition status
df_immed	ds	1		; Immediate mode (0 = not immediate)
df_sp		ds	1		; Stack pointer after error to restore to
df_pc		ds	2		; PC after error to return to
df_brkpc	ds	2		; PC pushed on the stack for BRK
df_brkval	ds	1		; Byte after BRK instruction
df_prgstrt	ds	2		; Start of program code
df_prgend	ds	2		; End of program code
df_vntstrt	ds	2		; Variable name table start
df_vntend	ds	2		; Variable name table end
df_vvtstrt	ds	2		; Variable value table start
df_vvtend	ds	2		; Variable value table end
df_varcnt	ds	1		; Variable counter
df_starstrt	ds	2		; String and array table start
df_starend	ds	2		; String and array table end
df_rtstop	ds	1		; Runtime stack pointer
df_parmtop	ds	1		; Top of parameter stack (grows up)
df_strbuff	ds	1		; String expression buffer
df_stridx	ds	1		; Top of string buffer (grows down)
df_sevalptr	ds	2		; Pointer to next free char in string eval

df_linoff	ds	1		; Offset in to line buffer
df_tokoff	ds	1		; Offset in to tokenised buffer
df_eolidx	ds	1		; End of line index (i.e length)
df_nxtstidx	ds	1		; Offset to the next statement offset
df_curstidx	ds	1		; Offset to the start of currently executing statement
df_symtab	ds	2		; Pointer to next free symtab entry
df_symoff	ds	1		; Offset in to token table
df_symini	ds	2		; Start of symtab
df_currlin	ds	2		; Execution current line pointer
df_exeoff	ds	1		; Execution line buffer offset
df_nextlin	ds	2		; Next line to execute
df_procmode	ds	1		; Only used during tokenisation
df_procargs	ds	1		; Only used during tokenisation
df_procloc	ds	1		; Counts the number of local parameters
df_procptr	ds	2		; Pointer to proc vvt slot
df_lineptr	ds	2		; Pointer to line during searches
df_lineidx	ds	1		; Pointer to line index during searches
df_ifnest	ds	1		; Global nested if counter
df_currdat	ds	2		; Data current line pointer
df_datoff	ds	1		; Data line buffer offset
df_rnd		ds	2		; Random number seed

df_asmpc	ds	2		; Assembler program counter
df_asmopt	ds	1		; Assembler current option
df_asmadmd	ds	1		; Addressing mode
df_asmopcde	ds	1		; Current opcode
df_asmoprnd	ds	2		; Current operand
df_asmlen	ds	1		; Instruction length

dflat_zp_save_e			; Save up to this place

; Temp space for dflat
df_tmpptra	ds	2		; Temp pointer a
df_tmpptrb	ds	2		; Temp pointer b
df_tmpptrc	ds	2		; Temp pointer c
df_tmpptrd	ds	2		; Temp pointer d
df_tmpptre	ds	2		; Temp pointer e

dflat_zp_e

zp_tmp1		ds	1		; General zero page temporary
zp_tmp2		ds	1		; General zero page temporary
zp_tmp3		ds	1		; General zero page temporary
zp_tmp4		ds	1		; General zero page temporary

;***** END OF ZERO PAGE *****
_end_zero_page

;***** Page 1 is CPU stack ****
	org 0x0100
_cpu_stack
			ds	256		; All of page 1

;***** Page 2 is tape buffer *****
	org 0x0200
tp_buf		ds	256		; Serial input / output line buffer (tape system)

;***** 3 is IO *****
	org 0x0300			; IO mapped to Page 3 on Oric
io_address	ds	256

	org 0x0400			; Page 4 = dflat space
fd_getname_addr			; ** FOR ORICUTRON EMULATOR **
df_linbuff
df_raw		ds	128		; untokenised input line
df_tokbuff
df_tok		ds 	128		; tokenised output line

	org 0x0500			; Page 5 = fixed space for interpreter stack
df_rtstck				; operator stack grow up, runtime grows down
df_rtspace	ds	256


;***** NON-ZERO PAGE VARIABLES *****

; Acticve IO device settings
io_default	ds	1		; The default device number
io_block	ds	io_struct


; Dflat top of memory+1 - initialised at boot time but can be changed by user
df_memtop	ds	2

; Scratch area e.g. string and numeric expression evaluation, screen scrolling etc.
scratch		ds	256

;***** THIS IS THE START OF FREE SPACE for DFLAT **
mem_start

