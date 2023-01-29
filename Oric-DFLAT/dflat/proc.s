;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  PROC.S
;*  dflat module to handle procedures:
;*  - executing a procedure
;*  - find a proc, pass local and non-local parameters
;*  - return from a proc, unload locals
;*  - save the definition of a proc in the VNT and VVT
;*
;**********************************************************

	; ROM code
	code  

mod_sz_proc_s

df_rt_proc_parmerr
df_rt_deferr
	SWBRK DFERR_PROCPARM


; call procedure
df_rt_proc
	; move past escape token
	ldy df_exeoff
	iny
	; Get VVT address X,A and procptr
	lda (df_currlin),y
	tax
	iny
	lda (df_currlin),y
	iny
	sty df_exeoff
	stx df_procptr
	sta df_procptr+1
	
	; is index 0 (held in dim1)
	; then need to find the procedure
	ldy #DFVVT_DIM1
	lda (df_procptr),y
	bne df_rt_proc_addr
	; find proc
	jsr df_rt_findproc
	; save y (line index)
	sty tmp_d
	; now go and update the proc vvt address
	ldy #DFVVT_HI
	sta (df_procptr),y
	ldy #DFVVT_LO
	txa
	sta (df_procptr),y
	ldy #DFVVT_DIM1
	; get back line index in to A
	lda tmp_d
	sta (df_procptr),y
df_rt_proc_addr
	; move past first open bracket
	inc df_exeoff
	
	; get parm count
	ldy #DFVVT_DIM2
	lda (df_procptr),y	
	beq df_rt_proc_parm_none
	; push the right number of parms on
	pha

	dec df_exeoff		; Pre-adjust
df_rt_proc_push_parm
	; move past comma or opening bracket
	inc df_exeoff
	; if at end then error!
	ldy df_exeoff
	lda (df_currlin),y
	cmp #')'
	beq df_rt_proc_parmerr
	; else try and evaluate
	jsr df_rt_neval
	; get parm count off stack
	pla
	; decrement
	sec
	sbc #1
	; and put back on stack
	pha
	; go back and do all required parms
	bne df_rt_proc_push_parm
	; remove parm counter from stack
df_rt_proc_parm_done
	pla
df_rt_proc_parm_none
	; should be at close bracket
	ldy df_exeoff
	lda (df_currlin),y
	cmp #')'
	bne df_rt_proc_parmerr
	; should be no more parms
	; ok, finally we have all parms on rt stack
	; now execute the procedure
	; get back the proc address
	ldx df_procptr
	lda df_procptr+1
;	bne df_rt_exec_proc		; ALWAYS as procptr+1 !=0
	
; executing a procedure in VVT slot A,X
df_rt_exec_proc
	; save slot address
	stx df_tmpptra
	sta df_tmpptra+1
	
	; need to save all important vars
	lda df_currlin
	pha
	lda df_currlin+1
	pha
	lda df_exeoff
	pha
	lda df_nxtstidx
	pha
	lda df_curstidx
	pha
	lda df_eolidx
	pha
	lda df_ifnest
	pha

	; now initialise the data
	ldy #DFVVT_LO
	lda (df_tmpptra),y
	sta df_currlin
	iny
	lda (df_tmpptra),y
	sta df_currlin+1
	iny
	lda (df_tmpptra),y
	sta df_exeoff
	sta df_curstidx
	ldy #0		; hmm XXXXX might need to be tay to get the next statement idx XXXXX
	lda (df_currlin),y
	sta df_nxtstidx
	; now execute statements
	jsr df_rt_exec_stat
;	bcs df_rt_exec_proc_err
	; now restore the position
	pla
	sta df_ifnest
	pla
	sta df_eolidx
	pla
	sta df_curstidx
	pla
	sta df_nxtstidx
	pla
	sta df_exeoff
	pla
	sta df_currlin+1
	pla
	sta df_currlin
	; should be all restored, so return
	rts


df_rt_def
	; line offset pointing at DFTK_PROC
	; skip over PROC address and open bracket
	ldx df_exeoff
	inx
	inx
	inx
	stx df_exeoff
	; parms on stack in reverse order to parm list
	; so get each parm and type and save to scratch
	ldx #1									; index in to scratch
	stx df_procargs							; Proc args is the #args-1
	ldy df_exeoff
df_rt_def_find_var
	iny
	lda (df_currlin),y
	; check if end of parm list
	cmp #')'
	beq df_rt_def_parm_done
	; else check if found a variable escape token (<32)
	cmp #DFTK_VAR
	beq df_rt_def_got_var
	; else check if non-local specifier
	cmp #DFTK_VARPARM						; This is a regular ASCII char '&'
	bne df_rt_def_find_var
df_rt_def_got_varparm
	; set high bit
	ora #0x80
	; advance over non-local specifier
	iny
df_rt_def_got_var
	eor #0x80			; If was set by '&' qualifier then will be unset
	sta scratch,x		; Save parm type (by value or ref)
	; get address and save in scratch
	iny
	lda (df_currlin),y
	sta scratch+32,x	; Hope 32 params is enough! :-)
	iny
	lda (df_currlin),y
	sta scratch+64,x
	inx
	inc df_procargs
	bne df_rt_def_find_var  ; always - relies not zero!
df_rt_def_parm_done
	; save index that we got to
	sty df_exeoff
	; save def param list position in temp
	stx df_procmode
	; all var indices on the operator stack
	; now load up variables with parameters
	; initially assume no locals
	lda #0
	sta df_procloc
df_rt_def_load_var
	dec df_procargs
	beq df_rt_def_load_var_done
	; get var address
	dec df_procmode
	ldx df_procmode
	lda scratch+32,x
	sta df_tmpptra
	lda scratch+64,x
	sta df_tmpptra+1
	
	lda scratch,x
	; if MSB is clear then this is not a local variable
	; so just go an initialise with stacking
	bpl df_rt_def_initialise_parm
	; else call the local handling code to 
	; push the var address on to the runtime stack
	ldx df_tmpptra
	lda df_tmpptra+1
	jsr df_rt_proc_local
	; increment number of locals
	inc df_procloc
df_rt_def_initialise_parm
	; load type
	ldy #DFVVT_TYPE
	lda (df_tmpptra),y
	; if array or string type then pop pointer from operator stack
	and #DFVVT_STR|DFVVT_ARRY
	beq df_rt_def_load_var_int
	jsr df_ost_popPtr
	jmp df_rt_def_load_var_int_skip
df_rt_def_load_var_int
	; must be int pop it from operator stack
	jsr df_ost_popInt
df_rt_def_load_var_int_skip
	; update the variable
	ldy #DFVVT_HI
	sta (df_tmpptra),y
	dey
	txa
	sta (df_tmpptra),y
	
	jmp df_rt_def_load_var
df_rt_def_load_var_done
	; save the number of local parameters found so they can
	; be unloaded when the proc ends
	lda df_procloc
	jsr df_rst_pushByte
	; continue with next statement
	clc
	rts

	
; return a value
df_rt_return
	; evaluate the return and put on the parameter stack
	; then process this like an end of procedure
	jsr df_rt_neval
; end def for a proc
df_rt_enddef
	; unload any locals
;	jsr df_rt_proc_unlocal
	; nothing to do - main loop will terminate
;	clc
;	rts
; enddef falls through to the unlocal code	
	
; unload any local variables from runtime stack
;df_rt_proc_unlocal
	jsr df_rst_popByte
	tax
	beq df_rt_proc_unload_done
df_rt_proc_unloadvar
	txa
	pha
	; var value is popped first then index
	; get a word and put in tmpb
	jsr df_rst_popWord
	stx df_tmpptrb
	sta df_tmpptrb+1
	; get the var address
	jsr df_rst_popWord
	stx df_tmpptra
	sta df_tmpptra+1
	; store lo byte first
	ldy #DFVVT_LO
	lda df_tmpptrb
	sta (df_tmpptra),y
	; then hi
	iny
	lda df_tmpptrb+1
	sta (df_tmpptra),y
	; restore counter
	pla
	tax
	dex
	bne df_rt_proc_unloadvar
df_rt_proc_unload_done
	pla				; Pull old return address from stack
	pla
	clc
	rts
	
; push a local variable to the runtime stack
; X,A = var slot
df_rt_proc_local
	; save address
	stx df_tmpptra
	sta df_tmpptra+1
	; push var slot on rt stack
	jsr df_rst_pushWord
df_rt_proc_local_load	
	; load x,a with var value lo,hi
	ldy #DFVVT_LO
	lda (df_tmpptra),y
	tax
	iny
	lda (df_tmpptra),y
	; push word on to rt stack
	jsr df_rst_pushWord
	clc
	rts
		
mod_sz_proc_e
