;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  STACK.S
;*  This module implements the dflat software stacks.  dflat
;*  needs a few stacks:
;*  - 6502 stack for expression parsing and reentrancy
;*  - dflat runtime stack for things like for/next loops
;*  - dflat parameter stack for passing parameters etc.
;*  The two software stacks are implemented in the same page
;*  with the operator stack growing up from 0 and the
;*  runtime stack growing down from 255.  Hopefully they
;*  don't meet as there are no checks for this at the moment
;*  purely because I want speed over friendliness, although
;*  I may come to regret this!
;*  I have also added the only runtime memory allocation
;*  needed by dflat here.  Humourously I have called it 'malloc'
;*  which is taken from the C language, but it's only to
;*  grab some memory after end the of dflat program code for
;*  arrays - which of course cannot have storage allocated at
;*  tokenisation time (e.g. because I dimension an array with
;*  a size from a variable).
;*
;**********************************************************

	; ROM code
	code  


;****************************************
;* Push a byte on to runtime stack
;* A = byte
;****************************************
df_rst_pushByte
	ldy df_rtstop
	sta df_rtstck,y
	dey
	sty df_rtstop
	rts

;****************************************
;* Pop a byte off runtime stack
;* A = byte
;****************************************
df_rst_popByte
	ldy df_rtstop
	iny
	lda df_rtstck,y
	sty df_rtstop
	rts

;****************************************
;* Peek a byte off runtime stack
;* A = byte
;****************************************
df_rst_peekByte
	ldy df_rtstop
	; Look at what is below top of stack
	; below means the next address up as
	; this stack grows downwards like 6502
	lda df_rtstck+1,y
	rts
	
;****************************************
;* Push a word on to runtime stack
;* X,A = word lo,hi
;****************************************
df_rst_pushWord
	ldy df_rtstop
	sta df_rtstck,y
	dey
	txa
	sta df_rtstck,y
	dey
	sty df_rtstop
	rts

;****************************************
;* Pop a word off runtime stack
;* X,A = word lo,hi
;****************************************
df_rst_popWord
	ldy df_rtstop
	iny
	ldx df_rtstck,y
	iny
	lda df_rtstck,y
	sty df_rtstop
	rts

;****************************************
;* Push a parameter on to parm stack
;* X,A - int
;* Y - type
;****************************************
df_ost_pushParmX
	sty tmp_d		; Save Type
	ldy df_parmtop	; Get stack index
	; push high byte first (A)
	sta df_rtstck,y
	iny
	; push low byte next (X)
	txa
	sta df_rtstck,y
	iny
	lda tmp_d		; get Type
	sta df_rtstck,y
	iny
	; save new top of stack
	sty df_parmtop
	rts

;****************************************
;* Push an int on to parm stack
;* X,A - int
;****************************************
df_ost_pushInt
	ldy #DFST_INT
	bne df_ost_pushParmX		; ALWAYS!

;****************************************
;* Push A on to parm stack
;* X,A - int
;****************************************
df_ost_pushIntA
	ldy #DFST_INT
	tax
	lda #0
	beq df_ost_pushParmX		; ALWAYS!

;****************************************
;* Push a string pointer on to parm stack
;* X,A - int
;****************************************
df_ost_pushStr
	ldy #DFST_STR
	bne df_ost_pushParmX		; ALWAYS!

;****************************************
;* Push a general pointer on to parm stack
;* X,A - int
;****************************************
df_ost_pushPtr
	ldy #DFST_PTR
	bne df_ost_pushParmX		; ALWAYS!
	
;****************************************
;* Pop parameter from the stack
;* X,A - int
;* Y - type expected
;****************************************
df_ost_popParmX
	tya
	ldy df_parmtop
	; pull type first
	dey
	and df_rtstck,y
	beq df_st_typemismatcherr
	; pull low byte first
	dey
	ldx df_rtstck,y
	; pull high byte next
	dey
	lda df_rtstck,y
	; save new top of stack
	sty df_parmtop
	clc
	rts
df_st_typemismatcherr
	SWBRK DFERR_TYPEMISM

;****************************************
;* Return type on top of stack
;* A - type
;****************************************
df_ost_peekType
	ldy df_parmtop
	lda df_rtstck-1,y
	rts

;****************************************
;* Pop an int off parm stack
;* X,A - int
;****************************************
df_ost_popInt
	ldy #DFST_INT
	bne df_ost_popParmX			; ALWAYS!

;****************************************
;* Pop a string pointer off parm stack
;* X,A - int
;****************************************
df_ost_popStr
	ldy #DFST_STR
	bne df_ost_popParmX			; ALWAYS!
	
;****************************************
;* Pop a general pointer off parm stack
;* X,A - int
;****************************************
df_ost_popPtr
	ldy #DFST_PTR
	bne df_ost_popParmX			; ALWAYS!
	
	
;****************************************
;* Allocate space on the heap
;* X, A = Number of bytes to allocate
;* X, A return address of allocated space
;****************************************
df_st_malloc
	; save A in Y
	tay
	; save old starend
	lda df_starend+1
	pha
	lda df_starend
	pha
	; restore A from Y
	tya
	; add X,A to starend
	pha
	txa
	clc
	adc df_starend
	sta df_starend
	pla
	adc df_starend+1
	sta df_starend+1
	; return old starend as start of space
	pla
	tax
	pla
	rts
	