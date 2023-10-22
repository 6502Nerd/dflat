;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  RUNTIME.S
;*  This module is the runtime coordinator.  When the user
;*  wants to run a program, execution of statements from the
;*  required procedure starts and proceeds from there
;*  according to normal program flow.
;*  This module also contains critical routines for the
;*  evaluation of expressions (numeric and string).
;*  Whilst the code to implement a specific command is in
;*  rtsubs.s, this is the key module that controls everything.
;*
;**********************************************************

	; ROM code
	code

mod_sz_runtime_s

;****************************************
;* df_initrun
;* Initialise program space for runtime
;****************************************
df_initrun
	; Set the key mask to check every 16 instructions
	lda #0x10
	sta df_checkmsk
	sta df_checkkey

	; String and array heap initialisation
	; Grows up from end of prog space PLUS 1
	; Initially empty (dim will allocate)
	_cpyZPWord df_prgend,df_starstrt
	_incZPWord df_starstrt
	_cpyZPWord df_starstrt,df_starend

	ldx #0xff
	; Reset runtime stack (grows down)
	stx df_rtstop
	; make X=0
	inx
	; Reset parameter stack (grows up)
	stx df_parmtop
	; Reset data pointer high byte
	stx df_currdat+1
	; if nest counter zeroed
	stx df_ifnest

	; clear proc addresses
	jsr df_rt_init_vvt

;	clc
	rts

;****************************************
;* Initialise vvt
;****************************************
df_rt_init_vvt
	; starting at beginning of vvt
	_cpyZPWord df_vvtend,df_tmpptra
	ldx #0
df_rt_init_vvt_slot
	lda df_tmpptra
	cmp df_vvtstrt
	bne df_rt_init_vvt_slot_undim
	lda df_tmpptra+1
	cmp df_vvtstrt+1
	beq df_rt_init_done
df_rt_init_vvt_slot_undim
	; Only zero out proc and array pointers
	; Scalar variables are not initialised
	ldy #0
	lda (df_tmpptra),y
	and #DFVVT_PROC|DFVVT_PTR
	beq df_rt_init_vvt_skip
	ldx #4			; Number of bytes to clear for array
	and #DFVVT_PROC	; But if proc, then 3 bytes
	beq df_rt_init_vvt_skip_proc
	dex
df_rt_init_vvt_skip_proc
	tya				; Set A to 0 (Y already zero)
	; skip over the first byte which is variable type
	iny
	; zero out bytes for proc and arrays
df_rt_init_vvt_z_loop
	sta (df_tmpptra),y
	iny
	dex
	bne df_rt_init_vvt_z_loop
df_rt_init_vvt_skip
	; increment pointer to next slot
	_adcZPByte df_tmpptra,#8
	jmp df_rt_init_vvt_slot 	; Don't rely on bcc
df_rt_init_done
	rts

;****************************************
;* Evaluate a numeric expression
;* pop  X,A result from operator stack
;****************************************
df_rt_getnval
	; evaluate the expression
	jsr df_rt_neval
	; get X,A off operator stack
	jmp df_ost_popInt


;****************************************
;* Evaluate a numeric expression
;* Push result to operator stack
;****************************************
df_rt_neval
	; push terminator on cpu stack
	; so we know where we are
	lda #0
	pha

	; find escape token or keyword token
	; if escape token push on to operand stack
	; if keyword token push on to operator stack
df_rt_neval_optk
	ldy df_exeoff
	; check end of line

;	cpy df_eolidx
;	beq df_rt_neval_process
	cpy df_nxtstidx
	beq df_rt_neval_process
	lda (df_currlin),y		; Get the byte which could be a token
	bmi df_rt_neval_tk		; N=1 means it's a keyword token
	cmp #DFTK_ESCVAL		; <=32 means it's an escape token
	bcc df_rt_neval_esc
	; check for evaluation terminators
	; specifically ',' and ']'
	cmp #','
	beq df_rt_neval_process
	cmp #']'
	beq df_rt_neval_process
	; check for brackets
	; if close bracket then process
	cmp #')'
	beq df_rt_neval_process
	; Nothing of interest matched or it's open bracket
	; so move to next byte either way
	inc df_exeoff				; Next byte 
	; if bracket then evaluate expression recursively
	cmp #'('
	bne df_rt_neval_optk
	; call evaluation function recursively
	jsr df_rt_neval
	; move to next byte
	inc df_exeoff
	bne df_rt_neval_optk		; ALWAYS as exeoff != 0
df_rt_neval_esc
	jsr df_rt_eval_esc
	; move to next byte
	inc df_exeoff
	bne df_rt_neval_optk		; ALWAYS as exeoff != 0
	; if a token then push on operator stack
df_rt_neval_tk
	and #0x7f
	; check if op (look up type using X as index)
	; X contains the current operator index

	tax
	lda df_tk_tokentype,x
	; A contains token type
	pha
	and #DFTK_OP
	bne df_rt_neval_tk_opp
	; check if fn
	pla
	and #DFTK_FN
	bne df_rt_neval_tk_fn

	; If got here then something wrong
	SWBRK DFERR_OK

df_rt_neval_process
	; pop operator off stack and execute
	; keep popping until reached the terminator
	pla
	beq df_rt_neval_done
	; run the token code
	jsr df_rt_run_token
	; top two bytes on ost stack is the result
	jmp df_rt_neval_process
df_rt_neval_done
	rts

df_rt_neval_tk_fn
	clc
	txa
	; run a fn token - returns a value on stack
	inc df_exeoff
	jsr df_rt_run_token
	; move to next byte
df_rt_neval_nextbyte
	inc df_exeoff
	bne df_rt_neval_optk		; ALWAYS as exeoff != 0

df_rt_neval_tk_opp
	pla				; Get A off stack which is the type
df_rt_neval_tk_op
	; X=Op, A=Type
	; if this op < current top of op stack
	; then do the op as it is higher priority so should
	; not be pushed
	; what is top of the op stack?
	; save current op token value
	; C=0 means process the op now, else don't
	; save current operator index
	stx df_tmpptra				; Index in to token table not zero
	; mask off to keep priority
	and #DFTK_OPMSK
	sta df_tmpptrb
	; peek top of op stack - pull and push X
	pla
	pha
	tax
	; if 0 then nothing so push op
	beq df_rt_neval_pushOp
	; use it to index in to type table
	lda df_tk_tokentype,x
	; mask off to keep priority
	and #DFTK_OPMSK
	; compare with the saved token type which includes priority
	cmp df_tmpptrb
	; if top of stack >= current then C=1
	; else C=0
	; what is the state of C?
	; if 1 then just pushOp
	beq df_rt_neval_donow
	bcs df_rt_neval_pushOp
df_rt_neval_donow
	; was C=0 so process now before pushing the new op
	; get operator off cpu stack
	pla
	tay		; Save whilst need to push other things
	; save the current op on cpu stack
	lda df_tmpptra
	pha
	tax		; X = current op
	tya		; Get A back from Y (new op)
	; now run the token in A that came off the stack
	jsr df_rt_run_token
	; get current op off cpu stack in to X
	pla
	tax
	; get the token type in to A
	lda df_tk_tokentype,x
	; now go back around again to check whether to push the op or exec it
	bne df_rt_neval_tk_op		; ALWAYS as type never zero
df_rt_neval_pushOp
	; push the operator
	lda df_tmpptra
	pha
	; move to next byte
	bne df_rt_neval_nextbyte	; ALWAYS as token index != 0




; jump to escape evaluation routine
df_rt_eval_esc
	asl a
	tax
	lda df_rt_eval_esc_tab,x
	sta df_tmpptra
	lda df_rt_eval_esc_tab+1,x
	sta df_tmpptra+1
	jmp (df_tmpptra)

df_rt_eval_esc_tab
	dw df_rt_eval_chr
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved	; no such thing as bytdec
	dw df_rt_eval_bythex
	dw df_rt_eval_bytbin
	dw df_rt_eval_reserved
	dw df_rt_eval_intdec
	dw df_rt_eval_inthex
	dw df_rt_eval_intbin
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved
	dw df_rt_eval_reserved
	dw df_rt_eval_strlit
	dw df_rt_eval_var
	dw df_rt_eval_proc


;****************************************
;* Evaluate a string expression
;* X, A = Destination is string accumulator
;****************************************
df_rt_seval
	ldx df_sevalptr
	lda df_sevalptr+1
;****************************************
;* Evaluate a string expression
;* X, A = Destination buffer / space
;****************************************
df_rt_sval
	; keep X,A on the stack - will be modified
	sta tmp_d
	pha
	txa
	pha
	lda tmp_d
	; push original destination
	jsr df_ost_pushStr
	; Push the destination to the 6502 stack
	; hi byte first then lo
	; push string idx so we know our starting position
	; in the string buffer
	; this limits all evaluations to 255 bytes
;	lda df_stridx
;	jsr df_st_pushOp

	; find escape token or keyword token
	; if escape token push on to operand stack
	; if keyword operator token push on to operator stack
	; if keyword function token run it

df_rt_seval_optk
	ldy df_exeoff
	; check end of line
;	cpy df_eolidx
;	beq df_rt_seval_done
	cpy df_nxtstidx
	beq df_rt_seval_done

	lda (df_currlin),y
	bmi df_rt_seval_tk

	cmp #DFTK_ESCVAL
	bcc df_rt_seval_esc
	; check for evaluation terminators
	; specifically ',' and ')'
	cmp #','
	beq df_rt_seval_done
	cmp #')'
	beq df_rt_seval_done
	bne df_rt_seval_nextbyte
df_rt_seval_esc
	; the only escape char is STRLIT, VAR or PROC
	cmp #DFTK_STRLIT
	beq	df_rt_seval_esc_strlit
	cmp #DFTK_VAR
	beq	df_rt_seval_esc_var
	cmp #DFTK_PROC
	beq	df_rt_seval_esc_proc

	; error if got here
	SWBRK DFERR_SYNTAX

	; if a token then push on operator stack
df_rt_seval_tk
	sty df_exeoff
	and #0x7f
	; check if op
	tax
	lda df_tk_tokentype,x
	and #DFTK_STROP
	bne df_rt_seval_tk_op
	; check if fn
	lda df_tk_tokentype,x
	and #DFTK_FN
	bne df_rt_seval_tk_fn

	; token type mismatch if got here
	SWBRK DFERR_TYPEMISM

df_rt_seval_tk_op
	; the only op is $+
	; so just ignore!
	txa
	jmp df_rt_seval_nextbyte

df_rt_seval_tk_fn
	txa
	inc df_exeoff
	jsr df_rt_run_token
	jmp df_rt_seval_copy

df_rt_seval_esc_var
	; go process the variable as a normal RVAL
	clc
	jsr df_rt_eval_var
	; copy source off rt stack to destination
df_rt_seval_copy
	; pull destination pointer
	pla
	sta df_tmpptra
	pla
	sta df_tmpptra+1
	; pop source string pointer off stack
	jsr df_ost_popStr
	stx df_tmpptrb
	sta df_tmpptrb+1
	; go and copy the string
	jsr df_rt_copyStr
	; now save the destination
	lda df_tmpptra+1
	pha
	lda df_tmpptra
	pha

;	jmp df_rt_seval_nextbyte
df_rt_seval_nextbyte
	inc df_exeoff
	jmp df_rt_seval_optk

df_rt_seval_esc_strlit
	; evaluate string literal
	jsr df_rt_eval_strlit
	jmp df_rt_seval_copy

df_rt_seval_esc_proc
	; not yet suported *******
	SWBRK DFERR_OK

	; keep going until non-ws char found or end of line / statement
df_rt_seval_done
	;
	pla
	pla

	clc
	rts


; Copy string from ptrb to ptra
df_rt_copyStr
	ldy #0
df_rt_copyStr_ch
	lda (df_tmpptrb),y
	sta (df_tmpptra),y
	beq df_rt_copyStr_done
	iny
	bne df_rt_copyStr_ch	; Always relying in Y never 0
df_rt_copyStr_done
	tya
	clc
	adc df_tmpptra
	sta df_tmpptra
	_bcc 2
	inc df_tmpptra+1
	rts

;****************************************
;* Evaluate and push numeric value
;****************************************
df_rt_eval_intdec
df_rt_eval_bytdec
df_rt_eval_inthex
df_rt_eval_bythex
df_rt_eval_intbin
df_rt_eval_bytbin
df_rt_eval_chr
	; numeric constant
	iny
	lda (df_currlin),y
	tax
	iny
	lda (df_currlin),y
	; save offset before calling any routine
	sty df_exeoff
	; push number on to stack
	jmp df_ost_pushInt

df_rt_eval_reserved
	; should not get here
	SWBRK DFERR_OK

;****************************************
;* Evaluate and push string constant
;****************************************
df_rt_eval_strlit
	sty df_exeoff
	; calculate the effective address in to AX
	; y + currlin
	tya
	; set carry to add one extra
	sec
	adc df_currlin
	tax
	lda df_currlin+1
	adc #0

	; push string on to stack
	jsr df_ost_pushStr
	; now proceed until end of string found
	ldy df_exeoff
df_rt_eval_strlit_ch
	lda (df_currlin),y
	beq df_rt_eval_strlit_done
	iny
	bne df_rt_eval_strlit_ch
df_rt_eval_strlit_done	; Always relying on Y never 0
	sty df_exeoff
	rts

;****************************************
;* Return array parameter
;* A has parm
;****************************************
df_rt_arry_parm
	; move past open bracket or comma
	inc df_exeoff
	; evaluate expression inside bracket
	jsr df_rt_getnval
	txa
;	clc
	rts

;****************************************
;* Return double array parameter
;* X = dim1, Y = dim2
;****************************************
df_rt_arry_parm2
	; go get array parm 1
	jsr df_rt_arry_parm
	pha
	ldx #0
	ldy df_exeoff
df_rt_arry_parm2_term
	lda (df_currlin),y
	cmp #']'
	beq df_rt_arry_parm2_skiparry2
	cmp #','
	beq df_rt_arry_parm2_arry2
	sty df_exeoff
	iny
	jmp df_rt_arry_parm2_term
df_rt_arry_parm2_arry2
	; get second dimension and put in Y
	jsr df_rt_arry_parm
	tay
	pla
	tax
;	clc
	rts
df_rt_arry_parm2_skiparry2
	ldy #0
	pla
	tax
;	clc
	rts


;****************************************
;* Evaluate and push variable
;* The actual value is pushed if numeric
;* The pointer is pushed if string
;* Carry Set = LVAR else normal RVAR
;* LVAR : Y = line index, A=vvt type, tmpptra = vvt slot address
;****************************************
df_rt_eval_var
	; save carry bit
	php
	; if lvar mode then already passed escape token
	bcs df_rt_eval_lvskip
	; move past var escape token
	iny
	; get var vvt address
	lda (df_currlin),y
	sta df_tmpptra
	iny
	sty df_exeoff
	lda (df_currlin),y
	sta df_tmpptra+1
	; push vvt type first as this is the last thing we need
	ldx #0
	lda (df_tmpptra,x)
df_rt_eval_lvskip
	pha
	; Test A for array or string
	and #DFVVT_PTR|DFVVT_STR
	; simple variable
	beq df_rt_eval_var_notarry
	; even if an array if no dimensions then return base pointer
	; if at end of statement or line then simple copy
	cpy df_nxtstidx
	beq df_rt_eval_var_simple
	; if next ch is not [ then simple copy
	iny
	lda (df_currlin),y
	dey
	cmp #'['
	bne df_rt_eval_var_simple
	; go do array handling
	beq df_rt_eval_var_do_arry
df_rt_eval_var_notarry
	; pull the type, not needed here
	pla
	; pull C and check if lvar wanted rather than rvar
	plp
	bcs df_rt_eval_lvar
	; just push the vvt lo,hi value
	ldy #DFVVT_LO
	lda (df_tmpptra),y
	tax
	ldy #DFVVT_HI
	lda (df_tmpptra),y

	jmp df_ost_pushInt

df_rt_eval_lvar
	; it's not an array, push the address of DFVVT_LO
	; add DFVVT_LO offset to slot address in X,A
	clc
	lda #DFVVT_LO
	adc df_tmpptra
	tax
	lda df_tmpptra+1
	adc #0

	; push pointer to lo,hi
	jmp df_ost_pushPtr

	; Simple push of pointer, needs to be valid
df_rt_eval_var_simple
	; clean up stack
	pla
	pla
	; simply get lo,hi and push ptr on stack
	ldy #DFVVT_LO
	lda (df_tmpptra),y
	tax
	ldy #DFVVT_HI
	lda (df_tmpptra),y
	beq df_rt_not_dimed
	jmp df_ost_pushPtr

df_rt_eval_var_do_arry
	; move past var index
	inc df_exeoff
	; zero out x,y as they have dimension info
	ldx #0
	ldy #0

	; ** Array handling routine **
	; A on stack = type
	; save vvt address
	lda df_tmpptra+1
	pha
	lda df_tmpptra
	pha

	; get array parms in X,Y
	jsr df_rt_arry_parm2
	; restore vvt address
	pla
	sta df_tmpptra
	pla
	sta df_tmpptra+1
	; save dimension indices for later
	stx df_tmpptrb
	sty df_tmpptrb+1
	; if y is zero then need to decide some stuff
	cpy #0
	bne df_rt_eval_var_dim2adj
	; if dim2 > 0 then swap x,y
	ldy #DFVVT_DIM2
	lda (df_tmpptra),y
	beq df_rt_eval_var_dim2adj
	; swap x,y
	ldx df_tmpptrb
	ldy df_tmpptrb+1
	stx df_tmpptrb+1
	sty df_tmpptrb

df_rt_eval_var_dim2adj
	ldx df_tmpptrb
	ldy df_tmpptrb+1
	; don't let y=0
	bne df_rt_eval_var_dim2adjy
	iny
df_rt_eval_var_dim2adjy
	; don't let x=0
	cpx #0
	bne df_rt_eval_var_dim2adjx
	inx
df_rt_eval_var_dim2adjx
	;calculate offset
	;(y-1)*dim1 + (x-1)
	dex
	lda #0
	sta num_a+1
	dey
	; (y-1)
	sty num_a
	; if y is 0 then no need to multiply
	beq df_rt_eval_var_nomult
	; (dim1)
	ldy #DFVVT_DIM1
	lda #0
	sta num_b+1
	lda (df_tmpptra),y
	sta num_b
	; (y-1)*dim1 num_a has result
	jsr int_fast_mult
df_rt_eval_var_nomult
	; move x to a
	txa
	; add x to num_a
	clc
	adc num_a
	sta num_a
	_bcc 2
	inc num_a+1
	; now have element offset in num_a
	; dimensions in x and y
	ldx df_tmpptrb
	ldy df_tmpptrb+1
	; get type of variable originally found
	pla
	pha
	and #DFVVT_INT
	beq df_rt_eval_var_push
	; if it is int then multiply offset by 2
	asl num_a
	rol num_a+1
df_rt_eval_var_push
	; add pointer in lo,hi to num_a
	clc
	ldy #DFVVT_LO
	lda (df_tmpptra),y
	adc num_a
	sta num_a
	ldy #DFVVT_HI
	lda (df_tmpptra),y
	bne df_rt_array_exists
	; if vvt address hi is zero then array not dimensioned
df_rt_not_dimed
	SWBRK DFERR_DIM
df_rt_array_exists
	adc num_a+1
	sta num_a+1
	; get the type
	pla
	; if not int or byte then push string
	and #DFVVT_INT|DFVVT_BYT
	beq df_rt_eval_var_str
	; get LVAR preference
	plp
	bcs df_rt_eval_ptr
	; need to load lo and hi for int
	; but only lo for byt
	and #DFVVT_INT
	beq df_rt_eval_byt
	; push the contents pointed to by num_a
	ldy #0
	lda (num_a),y
	tax
	iny
	lda (num_a),y
	jmp df_ost_pushInt
df_rt_eval_byt
	ldy #0
	lda (num_a),y
	tax
	tya			; Make A=0
	jmp df_ost_pushInt
df_rt_eval_var_str
	plp
df_rt_eval_ptr
;	clc
	; put num_a not contents
	ldx num_a
	lda num_a+1
	jmp df_ost_pushPtr

df_rt_eval_proc
	lda df_parmtop				; Save current position of parameter stack
	pha
	jsr df_rt_proc				; Go and call the user function
	pla							; Get back the original parameter stack position
	cmp df_parmtop				; if it is the same, then no return value!
	beq df_rt_eval_proc_err
	clc
	rts
df_rt_eval_proc_err
	; if no return value then report an errror
	SWBRK DFERR_RETURN



;****************************************
;* get two ints off the runtime stack
;* first parm in ptrb, second in ptra
;****************************************
df_rt_get2Ints
	; the first int popped is actually the second parm
	jsr df_ost_popInt
	stx df_tmpptrb
	sta df_tmpptrb+1

	jsr df_ost_popInt
	stx df_tmpptra
	sta df_tmpptra+1
	rts

;****************************************
;* get two strings off the runtime stack
;* first parm in ptrb, second in ptra
;****************************************
df_rt_get2Strs
	; the first int popped is actually the second parm
	jsr df_ost_popStr
	stx df_tmpptrb
	sta df_tmpptrb+1

	jsr df_ost_popStr
	stx df_tmpptra
	sta df_tmpptra+1
	rts

;****************************************
; common code for 2 ints runtime parsing
;****************************************
df_rt_parm_2ints
	; evaluate 1st parm
	jsr df_rt_neval
	; jump over comma
	inc df_exeoff
	; evaluate the 2nd parm
	jsr df_rt_neval
	
	jmp df_rt_get2Ints
	; pop 2nd parm
;	jsr df_ost_popInt
;	stx df_tmpptrb
;	sta df_tmpptrb+1
	; pop 1st parm
;	jsr df_ost_popInt
;	stx df_tmpptra
;	sta df_tmpptra+1
;	rts

;****************************************
; common code for 3 ints runtime parsing
;****************************************
df_rt_parm_3ints
	; evaluate 1st parm
	jsr df_rt_neval
	inc df_exeoff
	; evaluate the 2nd parm
	jsr df_rt_neval
	inc df_exeoff
	; evaluate the 3rd parm
	jsr df_rt_neval

	; pop 3rd parm
df_rt_parm_pop3
	jsr df_ost_popInt
	stx df_tmpptrc
	sta df_tmpptrc+1
	; pop 2nd and 1st
	jmp df_rt_get2Ints
;	jsr df_ost_popInt
;	stx df_tmpptrb
;	sta df_tmpptrb+1
	; pop 1st parm
;	jsr df_ost_popInt
;	stx df_tmpptra
;	sta df_tmpptra+1
;	rts

;****************************************
; common code for 4 ints runtime parsing
;****************************************
df_rt_parm_4ints
	; evaluate 1st parm
	jsr df_rt_neval
	inc df_exeoff
	; evaluate the 2nd parm
	jsr df_rt_neval
	inc df_exeoff
	; evaluate the 3rd parm
	jsr df_rt_neval
	inc df_exeoff
	; evaluate the 4th parm
	jsr df_rt_neval

	; pop 4th parm
	jsr df_ost_popInt
	stx df_tmpptrd
	sta df_tmpptrd+1

	; pop 3,2,1 parms
	jmp df_rt_parm_pop3

	; pop 3rd parm
;	jsr df_ost_popInt
;	stx df_tmpptrc
;	sta df_tmpptrc+1
	; pop 2nd parm
;	jsr df_ost_popInt
;	stx df_tmpptrb
;	sta df_tmpptrb+1
	; pop 1st parm
;	jsr df_ost_popInt
;	stx df_tmpptra
;	sta df_tmpptra+1
;	rts

;****************************************
; common code for 5 ints runtime parsing
;****************************************
;df_rt_parm_5ints
;	; evaluate 1st parm
;	jsr df_rt_neval
;	inc df_exeoff
;	; evaluate the 2nd parm
;	jsr df_rt_neval
;	inc df_exeoff
;	; evaluate the 3rd parm
;	jsr df_rt_neval
;	inc df_exeoff
;	; evaluate the 4th parm
;	jsr df_rt_neval
;	inc df_exeoff
;	; evaluate the 5th parm
;	jsr df_rt_neval
;
;	; pop 5th parm
;	jsr df_ost_popInt
;	stx df_tmpptre
;	sta df_tmpptre+1
;	; pop 4th parm
;	jsr df_ost_popInt
;	stx df_tmpptrd
;	sta df_tmpptrd+1
;	; pop 3rd parm
;	jsr df_ost_popInt
;	stx df_tmpptrc
;	sta df_tmpptrc+1
;	; pop 2nd parm
;	jsr df_ost_popInt
;	stx df_tmpptrb
;	sta df_tmpptrb+1
;	; pop 1st parm
;	jsr df_ost_popInt
;	stx df_tmpptra
;	sta df_tmpptra+1
;
;	rts

;****************************************
;* initialise statement to be executed
;* X,A = line pointer, Y=statement offset
;****************************************
df_rt_init_stat_ptr
	; save current line
	stx df_currlin
	sta df_currlin+1
	sty df_curstidx
	sty df_exeoff
	lda (df_currlin),y
	sta df_nxtstidx
	ldx #0
	lda (df_currlin,x)
	sta df_eolidx
;	ldy df_curstidx
;	lda (df_currlin),y
;	sta df_nxtstidx
	rts

;****************************************
;* Execute from a statement pointed to
;* by currlin and exeoff
;****************************************
df_rt_exec_stat
	ldx df_currlin
	lda df_currlin+1
	ldy df_exeoff
df_rt_exec_init_ptr
	jsr df_rt_init_stat_ptr
	; assume normal flow of control if next line hi = 0
	; this means no line can execute below page 1, no loss
	lda #0
	sta df_nextlin+1

	; find first token in statement
df_rt_exec_find_tok
	iny
	lda (df_currlin),y
	bpl df_rt_exec_find_tok
df_rt_exec_found_tok
	; skip past token to next byte in readiness
	iny
	sty df_exeoff
	; Run that statement - if end of return will pull the jsr
	; and effectively do an rts from this routine.
	jsr df_rt_run_token

	; check for break, asynch get
	dec df_checkkey
	bne df_rt_exec_no_key
	lda df_checkmsk
	sta df_checkkey
	clc
	jsr io_get_ch
	bcc df_rt_exec_check_key
df_rt_exec_no_key
	; check if normal flow of control
	lda df_nextlin+1
	bne df_rt_exec_jump
	; try and execute another statement until end of line
	ldy df_nxtstidx
	cpy df_eolidx
	sty df_exeoff
	bne df_rt_exec_stat
	
	; reached end of line, move to next
	ldx #0
	clc
	lda (df_currlin,x)
	adc df_currlin
	sta df_currlin
	_bcc 2
	inc df_currlin+1

	; start from first statement in new line
	ldy #3
	sty df_exeoff

	; check if this line has any content (length >0)
	lda (df_currlin,x)
	sta df_eolidx
	; no more lines (len = 0), program done
	bne df_rt_exec_stat
	; else done
	; normally wouldn't get here except immediate mode
	; if line number <> 0 then error
	lda df_immed
	beq df_rt_unexpected_end
df_rt_exec_end
	clc
	rts
df_rt_exec_check_key
	cmp #UTF_BRK					; CTRK-Z?
	beq df_rt_exec_ctrl_z
	cmp #UTF_ETX					; CTRL-C?
	bne df_rt_exec_no_key			; None relevant keys, go back to runtime loop
df_rt_exec_ctrl_c
	SWBRK DFERR_BREAK
df_rt_exec_ctrl_z
	; Force a break with zero error number
	; this will drop in to the monitor
	SWBRK 0

df_rt_unexpected_end
	SWBRK DFERR_IMMEDIATE

	; if hi byte of nextline is not zero then
	; current line = next line
df_rt_exec_jump
	; initialise statement pointer from nextlin,tokstidx
	ldx df_nextlin
	lda df_nextlin+1
	ldy df_nxtstidx
	jmp df_rt_exec_init_ptr


;****************************************
;* Run statement in A
;****************************************
df_rt_run_token
	; mask off MSB
;	and #0x7f
	; multiply by 2
	asl a
	tax
	lda df_rt_tokenjmp,x
	sta df_tmpptra
	lda df_rt_tokenjmp+1,x
	sta df_tmpptra+1
	; execution code finishes with rts
	jmp (df_tmpptra)


;****************************************
;* X,A : Line Address, Y = Index
;* C=0 Found next statement
;* C=1 No statement found
;****************************************
df_rt_nextstat
	; save pointer
	stx df_lineptr
	sta df_lineptr+1
	; if end of program then err
	ldx #0
	lda (df_lineptr,x)
	beq df_rt_nextstat_err
	; if next statement idx == line length
;	lda (df_lineptr),y
;	beq df_rt_nextstat_ln
	lda (df_lineptr),y
	cmp (df_lineptr,x)
	beq df_rt_nextstat_ln
	; then go to next line
	; else make this Y
	tay
	; X = line low
	ldx df_lineptr
	; A = line high
	lda df_lineptr+1
	clc
	rts
df_rt_nextstat_ln
	; for next line, add line length to ptr
	clc
;	lda (df_lineptr,x)
	adc df_lineptr
	sta df_lineptr
	_bcc 2
	inc df_lineptr+1
	; if end of program set C as error
	lda (df_lineptr,x)
	beq df_rt_nextstat_err
	ldx df_lineptr
	lda df_lineptr+1
	; always skip line number and length for start of 1st stat
	ldy #3
	clc
	rts
df_rt_nextstat_err
	ldy #0
	sec
	rts

;****************************************
;* Push current line and statement to runtime stack
;****************************************
df_rt_push_stat
	lda df_curstidx
	jsr df_rst_pushByte
	lda df_currlin+1
	ldx df_currlin
	jmp df_rst_pushWord
;	clc
;	rts

;****************************************
;* Pop line and statement from runtime stack
;* And transfer control to next statement
;****************************************
df_rt_pop_stat
	jsr df_rst_popWord
	stx	df_nextlin
	sta df_nextlin+1
	jsr df_rst_popByte
	tay
	ldx df_nextlin
	lda df_nextlin+1
	jsr df_rt_nextstat
	stx df_nextlin
	sta df_nextlin+1
	sty df_nxtstidx
;	clc
	rts

;****************************************
;* Find proc definition in procptr
;* Only call if proc not found before
;****************************************
df_rt_findproc
	; start at program beginning
	_cpyZPWord df_prgstrt,df_lineptr
	ldy #3
	sty df_lineidx
	ldx #0
	lda (df_lineptr,x)
	beq df_rt_findproc_err
df_rt_findproc_cmd
	iny
	lda (df_lineptr),y
	bpl df_rt_findproc_cmd
	cmp #DFRT_DEF
	bne df_rt_findproc_nextstat
	; skip def token
	iny
	; skip proc escape token
	iny
	; now check the proc address against procptr
	lda (df_lineptr),y
	cmp df_procptr
	bne df_rt_findproc_nextstat
	iny
	lda (df_lineptr),y
	cmp df_procptr+1
	bne df_rt_findproc_nextstat

	; found it, return AXY with line details
	ldx df_lineptr
	lda df_lineptr+1
	ldy df_lineidx
	clc
	rts
df_rt_findproc_nextstat
	; restore AXY line details and find next statement
	ldx df_lineptr
	lda df_lineptr+1
	ldy df_lineidx
	jsr df_rt_nextstat
	bcs df_rt_findproc_err
	stx df_lineptr
	sta df_lineptr+1
	sty df_lineidx
	jmp df_rt_findproc_cmd
	; error
df_rt_findproc_err
	SWBRK DFERR_NOPROC

;****************************************
;* Find an escape value
;* Does not check for end of line or statement
;****************************************
df_rt_findescval
	ldy df_exeoff
	dey
df_rt_findescval_loop
	iny
	lda (df_currlin),y
	cmp #DFTK_ESCVAL
	bcs df_rt_findescval_loop
	sty df_exeoff
	rts

;****************************************
;* Skip white space
;* Does not check for end of line or statement
;* A contains non-ws char, df_exeoff updated
;****************************************
df_rt_skip_ws
	ldy df_exeoff
	dey
df_rt_skip_ws_loop
	iny
	lda (df_currlin),y
	cmp #' '
	beq df_rt_skip_ws_loop
	sty df_exeoff
	rts

;****************************************
;* Get an lvar
;* Assumes next token will be escape DFTK_VAR
;* tmpptra contains vvt slot address
;* X,A is the lvar pointer
;****************************************
df_rt_getlvar
	jsr df_rt_findescval
	; move past the escape value
	iny

	; get variable address
	lda (df_currlin),y
	sta df_tmpptra
	iny
	lda (df_currlin),y
	sta df_tmpptra+1

	sty df_exeoff
	; get the type
	ldy #0
	lda (df_tmpptra),y
	; set carry flag to return pointer (lvar)
	sec
	ldy df_exeoff
	jsr df_rt_eval_var
	jmp df_ost_popPtr
;	rts

;****************************************
;* Pop stat from rt stack and continue
;* Y MUST BE ON THE CPU STACK AS IT GETS PLYed HERE
;****************************************
df_rt_pop_stat_go
	jsr df_rt_pop_stat
	; restore stack pointer so we don't lose this entry
	pla
	tay
	sty df_rtstop
	clc
	rts

	include "dflat/rtjmptab.s"
	include "dflat/rtsubs.s"
	include "dflat/proc.s"

mod_sz_runtime_e


