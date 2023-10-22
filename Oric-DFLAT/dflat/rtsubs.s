;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  RTSUBS.S
;*  Module that implements the runtime execution of dflat
;*  keywords and functions.
;*  So this is where most of the action is for runtime, when
;*  a line is being executed, the dflat runtime controller
;*  jumps through the runtime table to routines here.
;*  Every dflat statement begins with a token (ignoring any
;*  whitespace), even the implicit assignment and procedure
;*  invocation.
;*
;**********************************************************

	; ROM code
	code

mod_sz_rtsubs_s

	include "dflat/numop.s"

df_rt_monitor
	jmp command_line

df_rt_new
	jmp df_clear

df_rt_while
	; push statement address
	jsr df_rt_push_stat
	; DFRT_WHILE token
	lda #DFRT_WHILE
	jsr df_rst_pushByte

	; get value in A,X
	jsr df_rt_getnval

	; if value<>0 then continue
	cpx #0
	beq df_rt_while_done
	rts
df_rt_while_done
	; pop while data off stack as not needed
	jsr df_rst_popWord
	jsr df_rst_popWord
	; while evaluated false so find wend
	; but check for any nested while/wends
	; nest = 1 to start
	lda df_ifnest
	pha
	lda #1
	sta df_ifnest
	; find the matching else/elseif/endif
	; start from current statement
	_cpyZPWord df_currlin,df_nextlin
df_rt_findwend
	ldx df_nextlin
	lda df_nextlin+1
	ldy df_curstidx
	jsr df_rt_nextstat
	; got to end of program, then a problem
	bcs df_rt_wend_end
	stx df_nextlin
	sta df_nextlin+1
	sty df_curstidx
	; find the command token
df_rt_while_cmd
	iny
	lda (df_nextlin),y
	bpl df_rt_while_cmd
	; check for wend
	cmp #DFRT_WEND
	bne df_rt_check_while
	; decrement nest
	dec df_ifnest
	; if not zero then go find more commands
	bne df_rt_findwend
	; else found it, restore if nest
	; and skip the wend statement
	pla
	sta df_ifnest
	ldx df_nextlin
	lda df_nextlin+1
	ldy df_curstidx
	jsr df_rt_nextstat
	; got to end of program, then a problem
	bcs df_rt_wend_end
	; need to update nxtstidx to transfer control
	stx df_nextlin
	sta df_nextlin+1
	sty df_nxtstidx
	rts
df_rt_check_while
	; check for while
	cmp #DFRT_WHILE
	bne df_rt_findwend
	; if while found then increment nest
	inc df_ifnest
	jmp df_rt_findwend
df_rt_wend_end
	SWBRK DFERR_IMMEDIATE

df_rt_wend
	jsr df_rst_popByte
	cmp #DFRT_WHILE
	bne df_rt_wend_err
	; pop the stat and continue
	jsr df_rst_popWord
	stx	df_nextlin
	sta df_nextlin+1
	jsr df_rst_popByte
	sta df_nxtstidx
	rts
df_rt_wend_err
	SWBRK DFERR_WEND

;move to next statement during if/else matching
;end of program is an error
df_rt_if_stat
	ldx df_nextlin
	lda df_nextlin+1
	ldy df_curstidx
	jsr df_rt_nextstat
	; got to end of program, then a problem
	bcs df_rt_if_stat_err
	stx df_nextlin
	sta df_nextlin+1
	sty df_curstidx
	sty df_nxtstidx
	rts
; program ended with no match
df_rt_if_stat_err
	SWBRK DFERR_UNCLOSEDIF

; find matching else/elseif/endif
; C = 0 match else/elseif/endif
; C = 1 match endif only
; endif is always matched
; ** MAKE SURE NEXTLIN IS POPULATED! **
df_rt_if_match
	; save the current if nest level
	lda df_ifnest
	pha
	; local if nest level is zero to start with
	lda #0
	sta df_ifnest
	; save match pref
	php
	; find the matching else/elseif/endif
	; start from df_nextlin, df_curstidx
df_rt_findelseendif
	jsr df_rt_if_stat
	; find command
df_rt_ifcmd
	iny
	lda (df_nextlin),y
	bpl df_rt_ifcmd
	; check for endif
	cmp #DFRT_ENDIF
	beq df_rt_ifelse

	plp
	php

	bcs df_rt_ifskipelseif
	cmp #DFRT_ELSE
	beq df_rt_ifelse
	cmp #DFRT_ELSEIF
	beq df_rt_ifelse
df_rt_ifskipelseif
	; another if token found - increment lcoal if nest level
	cmp #DFRT_IF
	bne df_rt_skipnestif
	inc df_ifnest
df_rt_skipnestif
	; no tokens of interest found, so next statement
	jmp df_rt_findelseendif

	; found else/elseif/endif
	; but check if this is nested
df_rt_ifelse
	; nest counter zero then found matching else/elseif/endif
	ldx df_ifnest
	beq df_rt_if_found
	; endif token found so decrement local nest
	cmp #DFRT_ENDIF
	bne df_rt_skipnestendif
	dec df_ifnest
df_rt_skipnestendif
	; continue to search for else/endif
	jmp df_rt_findelseendif
	; ok got a match
df_rt_if_found
	; remove pref
	plp
	; restore global if nest
	sta tmp_d			; Using as a temp place!
	pla					; Get nest from stack
	tax					; And put in X
	lda tmp_d			; Get a back from temp!
	stx df_ifnest		; Now restore the nest counter

	;A contains the token found, Y is index in to df_nextlin of cmd
;	clc
	rts

df_rt_endif
	; decrement if next level
	dec df_ifnest
	bmi df_rt_noif_err
df_rt_if_done
	rts

	; else and ifelse encountered in a normal sequence
	; only happens when the clause has been executed
	; so we only now need to find the endif
df_rt_elseif
df_rt_else
	; not in if mode then error
	lda df_ifnest
	beq df_rt_noif_err
	; find endif only
	; starting from current line and curstidx
	_cpyZPWord df_currlin,df_nextlin
	sec
	jmp df_rt_if_match

; endif/else/elseif encountered outside of an if
df_rt_noif_err
	SWBRK DFERR_NOIF

	; when if is encountered, the job of this routine is
	; to determine which clause to execute, then transfer
	; program control to that point.  in normal program
	; sequence else/elseif statements will signify the end
	; of an if construct.
df_rt_if
	; increment global if nest counter
	inc df_ifnest
df_rt_ifeval
	; get value
	jsr df_rt_getnval
	; if X,A<>0 if is successful then continue normal sequence
	tay
	bne df_rt_if_done
	txa
	bne df_rt_if_done
	; got here then if clause evaluated to false
	; match with else/elseif/endif
	; df_nextlin is used to find the clause to execute
	_cpyZPWord df_currlin,df_nextlin
	clc
	jsr df_rt_if_match
	; A contains the token found, Y is index of this token

	cmp #DFRT_ELSE
	; else: df_nextlin and df_nxtstidx points to the stat
	beq df_rt_do_else

	cmp #DFRT_ENDIF
	; else: df_nextlin and df_nxtstidx points to the stat
	beq df_rt_if_done

	; elif detected - increment past the token and evaluate like if
	; make this the current line and token index
	_cpyZPWord df_nextlin,df_currlin
	; move past the token and save position
	iny
	tya
	pha
	; initialise statement pointer
	ldy df_curstidx
	ldx df_currlin
	lda df_currlin+1
	jsr df_rt_init_stat_ptr
	; restore Y (one byte past the token) and save in exeoff
	pla
	tay
	sty df_exeoff
	; don't force a jump as we've initalised all vars here
	lda #0
	sta df_nextlin+1
	; now everyting is set up to evaluate the elif condition
	jmp df_rt_ifeval

df_rt_do_else
	; we need to point to the next statement not this one
	jmp df_rt_if_stat

df_rt_for
	; push statement address to rt stack
	jsr df_rt_push_stat
	; get lvar
	jsr df_rt_getlvar
	; Save lvar pointer, A is fine to trample
	pha
	txa
	pha

	; find starting value
	; evaluate the starting value
	; can't use df_rt_getnval as need to use A,X first
	inc df_exeoff
	jsr df_rt_neval
	; get ready to update the counter using pointer
	pla
	tax
	stx df_tmpptra
	pla
	sta df_tmpptra+1
	; But remember lvar pointer for later A is ok to trample
	pha
	txa
	pha

	; get the starting value from op stack
	jsr df_ost_popInt
	; save it to counter slot
	ldy #1
	sta (df_tmpptra),y
	txa
	dey
	sta (df_tmpptra),y

	; find end value
	; evaluate the end value
	inc df_exeoff
	jsr df_rt_getnval
	; and put on rt stack
	jsr df_rst_pushWord

	; find step value
	; evaluate the end value
	inc df_exeoff
	jsr df_rt_getnval
	; and push on rt stack
	jsr df_rst_pushWord
	; save the counter lvar pointer
	pla
	tax
	pla
	jsr df_rst_pushWord
	; all done - counter set to start
	; stack contains counter slot, step val, end val, next stat
	; now push for token
	lda #DFRT_FOR
	jmp df_rst_pushByte

df_rt_next
	; remember stack position
	ldy df_rtstop
	tya
	pha
	jsr df_rst_popByte
	cmp #DFRT_FOR
	bne df_rt_next_err
	; get the slot address
	jsr df_rst_popWord
	; save address to ptrd, contents to ptra
	stx df_tmpptrd
	sta df_tmpptrd+1
	ldy #0
	lda (df_tmpptrd),y
	sta df_tmpptra
	iny
	lda (df_tmpptrd),y
	sta df_tmpptra+1

	; get step value, save in ptrb
	jsr df_rst_popWord
	stx df_tmpptrb
	sta df_tmpptrb+1
	pha					; Push A to check for -ve step

	; add step to counter and save back to counter
	_addZPWord df_tmpptra,df_tmpptrb
	lda df_tmpptra
	ldy #0
	sta (df_tmpptrd),y
	lda df_tmpptra+1
	iny
	sta (df_tmpptrd),y

	; get end value, save in ptrb
	jsr df_rst_popWord
	stx df_tmpptrb
	sta df_tmpptrb+1
	pla					; check if if +ve or -ve step
	bpl df_rt_check_pos
	; call gte operation but no need to get ints
	; as already in ptra and ptrb
	jsr df_rt_gte_calc
	jmp df_rt_next_check
df_rt_check_pos
	; call lte operation but no need to get ints
	; as already in ptra and ptrb
	jsr df_rt_lte_calc
df_rt_next_check
	; check if true or false
	jsr df_ost_popInt
	txa
	; if false then next is done
	beq df_rt_untilnext_done
	; else we continue
	jmp df_rt_pop_stat_go

df_rt_next_err
	SWBRK DFERR_NEXTFOR


df_rt_repeat
	; push statement address
	jsr df_rt_push_stat
	; DFRT_REPEAT token
	lda #DFRT_REPEAT
	jmp df_rst_pushByte

df_rt_until
	; remember stack position
	lda df_rtstop
	pha
	jsr df_rst_popByte
	cmp #DFRT_REPEAT
	bne df_rt_until_err
	; evaluate expression in to A,X
	jsr df_rt_getnval
	; if value<>0 then continue
	txa
	bne df_rt_untilnext_done

	; pop the stat and continue
	jmp df_rt_pop_stat_go

df_rt_untilnext_done
	pla
	tay
	; pop 2 items off stack (line address, index)
	; and continue
	jsr df_rst_popWord
	jmp df_rst_popByte

df_rt_until_err
	SWBRK DFERR_UNTIL

df_rt_sadd
	rts

df_rt_print_num
	jsr df_ost_popInt
	clc
	jmp print_a_to_d

df_rt_print_str
	jsr df_ost_popStr
	stx df_tmpptra
	sta df_tmpptra+1
	ldy #0
df_rt_print_str_ch
	lda (df_tmpptra),y
	beq df_rt_print_str_done
	jsr io_put_ch
	iny
	bne df_rt_print_str_ch		; Rely on y not wrapping!
df_rt_print_str_done
	rts

; * Find the position of the next data item to read
df_rt_nextdatum
	ldx #0
	; load data line offset
	ldy df_datoff
	; if data pointer unitialised (because high byte == 0)
	lda df_currdat+1
	bne df_rt_skipinitdataptr
	; then start at program beginning
	_cpyZPWord df_prgstrt,df_currdat
df_rt_datlinstart
	; if end of program then error
	lda (df_currdat,x)
	beq df_rt_datumerr
	; index in to first line byte
	ldy #3
	sty df_datoff
	; find first 'data' statement
df_rt_datastatement
	iny
	tya
	; end of line reached?
	cmp (df_currdat,x)
	; if not find data token
	bne df_rt_getdatatk
df_rt_datnextlin
	; if so then go to next line
	clc
	lda df_currdat
	adc (df_currdat,x)
	sta df_currdat
	_bcc 2
	inc df_currdat+1
	jmp df_rt_datlinstart
df_rt_getdatatk
	lda (df_currdat),y
	bpl df_rt_datastatement
	; found data statement?
	cmp #DFRT_DATA
	; if not then go to next line
	bne df_rt_datnextlin
	sty df_datoff
df_rt_skipinitdataptr
	tya
	; end of line reached?
	cmp (df_currdat,x)
	; if so go to next line
	beq df_rt_datnextlin
	; else see if escape value
	lda (df_currdat),y
	cmp #DFTK_ESCVAL
	iny
	bcs df_rt_skipinitdataptr
	; ok found an escape value
	; save position and return
	dey
	sty df_datoff
	rts
df_rt_datumerr
	SWBRK DFERR_NODATA

; read a datum
df_rt_readdatum
	; update data pointer to next data item
	jsr df_rt_nextdatum
	; now get lvar X,A from current statement
	jsr df_rt_getlvar
	; save lvar in tmpb, vvt ptr in tmpa
	stx df_tmpptrb
	sta df_tmpptrb+1

	; first save save current prgoram line and offset
	lda df_currlin
	pha
	lda df_currlin+1
	pha
	lda df_exeoff
	pha
	lda df_eolidx
	pha
	lda df_nxtstidx
	pha
	lda df_curstidx
	pha

	; use data pointer as current position for evalution routines
	_cpyZPWord df_currdat,df_currlin
	lda df_datoff
	sta df_exeoff
	ldx #0
	lda (df_currdat,x)
	sta df_eolidx
	sta df_nxtstidx
	lda #3
	sta df_curstidx

	; get type from vvt ptr in tmpa
	lda (df_tmpptra,x)
	tay
	; get lvar point from tmpb
	ldx df_tmpptrb
	lda df_tmpptrb+1

	; X,A and Y set up, now evaluate and perform assignment
	jsr df_rt_doassign

	; update data offset as data has been consumed
	lda df_exeoff
	sta df_datoff
	; restore line settings
	pla
	sta df_curstidx
	pla
	sta df_nxtstidx
	pla
	sta df_eolidx
	pla
	sta df_exeoff
	pla
	sta df_currlin+1
	pla
	sta df_currlin
	rts


df_rt_read
	; find variable to read in to from current position
	ldy df_exeoff
df_rt_read_find_var
	iny
	; if end of line or statement then done
;	cpy df_eolidx
;	beq df_rt_read_done
	cpy df_nxtstidx
	beq df_rt_read_done
	; if not found escape then next byte
	lda (df_currlin),y
	cmp #DFTK_ESCVAL
	bcs df_rt_read_find_var
	; ok found escape, save position
	sty df_exeoff
	; go and read in the value
	jsr df_rt_readdatum
	; try find another variable
	jmp df_rt_read

df_rt_read_done
	; save position
	sty df_exeoff
	rts

df_rt_input
	; df_tmpptra has the vvt address, X,A is the lvar ptr
	jsr df_rt_getlvar
	; Save lvar pointer
	stx df_tmpptrb
	sta df_tmpptrb+1
	; go read a line of input
	; buf_lo ptr has the input, Y is size
	sec
	jsr io_read_line
	; check the type
	ldx #0
	lda (df_tmpptra,x)
	and #DFVVT_STR
	bne df_rt_input_str
	lda (df_tmpptra,x)
	and #DFVVT_INT|DFVVT_BYT
	bne df_rt_input_num
	; if not int or byte then error
	beq df_rt_input_err
df_rt_input_str
	lda (buf_lo),y
	sta (df_tmpptrb),y
	dey
	bpl df_rt_input_str
	rts

df_rt_input_num
	; X,A = address, linbuff must be on page boundary
	lda buf_lo+1
	ldx buf_lo
	ldy #0				; any numeric format
	jsr con_n_to_a
	bcs df_rt_input_err
	ldy #0
	lda num_a
	sta (df_tmpptrb),y
	iny
	lda num_a+1
	sta (df_tmpptrb),y
	rts
df_rt_input_err
	SWBRK DFERR_TYPEMISM

df_rt_local
	; get current local count off rt stack
	jsr df_rst_popByte
	; save on pc stack for incrmenting
	pha
	ldy df_exeoff
	dey
df_rt_local_findesc
	iny
	; check end of line
;	cpy df_eolidx
;	beq df_rt_local_done
	cpy df_nxtstidx
	beq df_rt_local_done
	; find a var
	lda (df_currlin),y
	cmp #DFTK_VAR
	bne df_rt_local_findesc
	; jump over escape value
	iny
	; get var slot in A,X
	lda (df_currlin),y
	tax
	iny
	lda (df_currlin),y
	sty df_exeoff
	; localise this variable
	jsr df_rt_proc_local
	; increment local counter
	pla
	clc
	adc #1
	pha
	ldy df_exeoff
	jmp df_rt_local_findesc
df_rt_local_done
	; get the local counter
	; put on to rt stack
	pla
	jmp df_rst_pushByte


df_rt_redim
	sec
	bcs df_rt_dim_main
df_rt_dim
	clc
df_rt_dim_main
	php
df_rt_dim_loop
	ldy df_exeoff
	dey
df_rt_dim_findesc
	; check end of line
	iny
;	cpy df_eolidx
;	beq df_rt_dim_done
	cpy df_nxtstidx
	beq df_rt_dim_done
	; find a var
	lda (df_currlin),y
	cmp #DFTK_VAR
	bne df_rt_dim_findesc
	; jump over escape value
	iny
	; get var address
	lda (df_currlin),y
	sta df_tmpptra
	iny
	lda (df_currlin),y
	sta df_tmpptra+1
	; move to open bracket
	iny
	sty df_exeoff
	; If re-dim, don't check for existing dimensions
	plp
	php
	bcs df_rt_skip_dim_chk
	; check if already dim'd
	ldy #DFVVT_DIM1
	lda (df_tmpptra),y
	bne df_rt_dim_err
df_rt_skip_dim_chk
	; Save slot address found earlier
	lda df_tmpptra
	pha
	lda df_tmpptra+1
	pha
	jsr df_rt_arry_parm2
	; Restore slot address
	pla
	sta df_tmpptra+1
	pla
	sta df_tmpptra
;	bcs df_rt_dim_err
	; save x,y to dim1,2
	tya
	pha
	txa
	pha
	ldy #DFVVT_DIM1
	pla
	sta (df_tmpptra),y
	iny
	pla
	sta (df_tmpptra),y
	plp
	php
	bcs df_rt_dim_set_type
df_rt_dim_alloc
	; ok we have up to 2 dimensions
	; mult dim 1 and 2 if dim 2 <> 0
	ldy #DFVVT_DIM1
	lda (df_tmpptra),y
	sta num_a
	lda #0
	sta num_a+1
	iny
	lda (df_tmpptra),y
	bne df_rt_dim2_nz
	lda #1
df_rt_dim2_nz
	sta num_b
	lda #0
	sta num_b+1
	jsr int_fast_mult
	; check the type if int then mult2
	ldx #0
	lda (df_tmpptra,x)
	and #DFVVT_INT
	beq df_rt_dim2_mul2
	asl num_a
	rol num_a+1
df_rt_dim2_mul2
	; finally, we have a size of array
	ldx num_a
	lda num_a+1

	; get a block of that size from heap
	jsr df_st_malloc
	; save pointer to block in var
	ldy #DFVVT_HI
	sta (df_tmpptra),y
	txa
	dey
	sta (df_tmpptra),y
	; finally, update the type to indicate array
df_rt_dim_set_type
	ldx #0
	lda (df_tmpptra,x)
	ora #DFVVT_PTR
	sta (df_tmpptra,x)
	; don't increment byte again - go check for more vars
	jmp df_rt_dim_loop
df_rt_dim_done
	plp
	rts
df_rt_dim_err
	SWBRK DFERR_DIM


df_rt_cls
	jmp gr_cls


df_rt_plot
	; evaluate the expression
	jsr df_rt_getnval
	; save lo byte
	txa
	pha
	; jump over comma
	inc df_exeoff
	; evaluate the expression
	jsr df_rt_getnval
	; save lo byte
	txa
	pha
	; jump over comma
	inc df_exeoff
	; evaluate the expression
	jsr df_rt_neval
	; check the type on the stack
	jsr df_ost_peekType
	; if >=0x80 then a pointer / string
	and #DFST_STR
	bne df_rt_plotstr
	; else it is int
	jsr df_ost_popInt
	; save  low byte of pop result in a temp
	stx df_tmpptra
	lda gr_scrngeom+gr_mode			; Check screen mode
	bne df_rt_plot_h_int
	; get X and Y coord in that order
	; but Y coord goes in X register!!!
	pla
	tax
	pla
	tay
	lda df_tmpptra	; Get pop result in to A
	jmp gr_plot
df_rt_plot_h_int
	pla
	tay
	pla
	tax
	lda df_tmpptra	; Get pop result in to A
	jmp gr_hchar


df_rt_plotstr
	; pop string pointer
	jsr df_ost_popPtr
	; save pointer to tmpa
	stx df_tmpptra
	sta df_tmpptra+1
	; get y and x in that order
	; but X register is Y coord!!
	pla
	sta df_tmpptrc			; Y coord in C
	pla
	sta df_tmpptrb			; X coord in B
	; set cursror position
	ldy #0
	sty df_tmpptre
df_rt_plotstrch
	ldy df_tmpptre
	lda (df_tmpptra),y
	sta df_tmpptrd			; char in D
	beq df_rt_plotstrdone
	inc df_tmpptre
	lda gr_scrngeom+gr_mode; Check screen mode
	bne df_rt_plot_h_str
	lda df_tmpptrd			; Get char to plot
	ldx df_tmpptrc			; Get Y coord
	ldy df_tmpptrb			; Get X coord
	jsr gr_plot
	inc df_tmpptrb			; Increment X coord
	bne df_rt_plotstrch		; Always - assume x is never 0!
df_rt_plot_h_str
	lda df_tmpptrd			; Get char to plot
	ldx df_tmpptrb			; Get X coord
	ldy df_tmpptrc			; Get Y coord
	jsr gr_hchar
	clc
	lda df_tmpptrb
	adc gr_scrngeom+gr_pitch
	sta df_tmpptrb
	bne df_rt_plotstrch		; Always - assume adding pitch is never 0!
df_rt_plotstrdone
	rts

df_rt_cursor
	; evaluate the expression
	jsr df_rt_getnval
	; write low byte of vdp_curoff
	; by writing a zero then cursor on else not
	stx vdp_curoff
	rts

;df_rt_himem
;	; evaluate the expression
;	jsr df_rt_getnval
;	; write X,A to df_memtop
;	stx df_memtop
;	sta df_memtop+1
;	; now clear everything down
;	jmp df_clear

df_rt_text
	jmp gr_init_screen_txt

df_rt_hires
	jmp gr_init_hires

df_rt_pixmode
	; evaluate the expression X = mode
	jsr df_rt_getnval
	stx gr_scrngeom+gr_pixmode
	rts

df_rt_ink
	; evaluate the expression X = col
	jsr df_rt_getnval
	stx gr_scrngeom+gr_ink
	rts

df_rt_paper
	; evaluate the expression X = col
	jsr df_rt_getnval
	; Add 16 to get paper colour
	txa
	clc
	adc #16
	sta gr_scrngeom+gr_paper
	rts

df_rt_point
	jsr df_rt_parm_2ints
	ldx df_tmpptra
	ldy df_tmpptrb
	jmp gr_point

df_rt_circle
	jsr df_rt_parm_3ints
	lda df_tmpptra				; load x0
	sta num_a
	lda	df_tmpptrb				; load y0
	sta num_a+1
	lda df_tmpptrc				; load r
	sta num_a+2
	jmp gr_circle

df_rt_lineto
	jsr df_rt_parm_2ints
	ldx df_tmpptra				; load x1
	ldy df_tmpptrb				; load y1
df_rt_doline
	stx num_a+2
	sty num_a+3
	jmp gr_line
df_rt_line
	jsr df_rt_parm_4ints
	ldx df_tmpptra				; load x0
	ldy	df_tmpptrb				; load y0
	jsr gr_set_hires_cur		; Start from x,y
	ldx df_tmpptrc				; load x1
	ldy df_tmpptrd				; load y1
	jmp df_rt_doline

df_rt_wait
	; evaluate the expression
	jsr df_rt_getnval
	; put high byte in to Y (X,Y)=16 bits
	tay
df_rt_wait_counter
	; get vdp low byte timer val in A
	lda vdp_cnt
df_rt_wait_tick
	; check if a tick has occurred (i.e. val <> A)
	cmp vdp_cnt
	beq df_rt_wait_tick
	; countdown tick
	txa
	bne df_rt_wait_skiphi
	dey
df_rt_wait_skiphi	
	dex
	txa
	bne df_rt_wait_counter
	tya
	bne df_rt_wait_counter
df_rt_wait_done
	rts

df_rt_printat
	; Get x,y
	jsr df_rt_parm_2ints
	ldx df_tmpptra
	ldy df_tmpptrb
	; Set the cursror here
	jsr gr_set_cur
	; and continue to normal print command
df_rt_print
	ldy df_exeoff
	dey
df_rt_print_ws
	iny

	; evaluate an expression
;	cpy df_eolidx
;	beq df_rt_print_done
	cpy df_nxtstidx
	beq df_rt_print_done
	lda (df_currlin),y
	cmp #':'
	beq df_rt_print_done
	cmp #' '
	beq df_rt_print_ws
	cmp #','
	beq df_rt_print_ws
	; save index
	sty df_exeoff

	; if starts with string literal then process seval
;	cmp #DFTK_STRLIT
;	beq df_rt_print_string
	; else evaluate a numeric
	jsr df_rt_neval
	; check what is on the argument stack
	jsr df_ost_peekType
	and #DFST_STR
	bne df_rt_print_gotstr
	jsr df_rt_print_num
	jmp df_rt_print
df_rt_print_gotstr
	jsr df_rt_print_str
	jmp df_rt_print
;df_rt_print_string
;	jsr df_rt_seval
;	jmp df_rt_print_gotstr
df_rt_print_done
	sty df_exeoff
	rts

df_rt_println
	jsr df_rt_print
	lda #UTF_CR
	jmp io_put_ch


; assign to a number variable
; X,A must have lvar
df_rt_nassign
	; push var address, A ok to trample
	pha
	txa
	pha

	; now go evaluate expression in to A,X
	jsr df_rt_getnval
	; restore variable address to write to to df_tmpptra
	; but don't lose current A
	tay					; Save A in Y
	pla
	sta df_tmpptra
	pla
	sta df_tmpptra+1
	tya					; Get A back from Y

	; save X,A int in contents section
	ldy #1
	sta (df_tmpptra),y
	txa
	dey
	sta (df_tmpptra),y

	rts

; assign to a string variable
; X,A must have lvar
df_rt_sassign
	; now go evaluate expression
	; with the destination being already in X,A
	jsr df_rt_sval

	; get string pointer from top of runtime stack
	jmp df_ost_popStr


; generate lvar from a var token ready for assignment
df_rt_generate_lvar
	; move past escape val
	ldy df_exeoff
	iny
	; pointing to variable index
	lda (df_currlin),y
	sta df_tmpptra
	iny
	lda (df_currlin),y
	sty df_exeoff
	sta df_tmpptra+1

	; get the type and save
	ldx #0
	lda (df_tmpptra,x)
	pha

	; set carry flag to return pointer (lvar)
	sec
	jsr df_rt_eval_var
	jsr df_ost_popPtr

	; pull the type previously saved into Y
	sta tmp_d		; Save A
	pla
	tay
	lda tmp_d
	; move past the lvar variable index
	inc df_exeoff
	rts

; general assignment execution
; generate lvar first
df_rt_assign
	jsr df_rt_generate_lvar
; X,A,Y contain lvar pointer and type
df_rt_doassign
	; save A and put type Y in to A to check for string
	pha
	tya
	and #DFVVT_STR
	; if a string then string expression
	bne df_rt_assign_str
	;  jump to numeric expression evaluator
	; remember to restore A
	pla
	jmp df_rt_nassign
df_rt_assign_str
	; else jump to string expression evaluator
	; remember to restore A
	pla
	jmp df_rt_sassign

; comment or data token is ignored by runtime
df_rt_comment
df_rt_data
	rts


; run token - future expansion
df_rt_run
	rts

; end of line / statement indicator
; CS = End, CC = not end
df_rt_eos
	ldy df_exeoff
;	cpy df_eolidx
;	beq df_rt_eos_true
	cpy df_nxtstidx
	beq df_rt_eos_true
	lda (df_currlin),y
	cmp #':'
	beq df_rt_eos_true
	clc
	rts
df_rt_eos_true
	sec
	rts

; renum startLine,newStart,increment
; renumbers from the first matching line to end of program
;df_rt_renum
;	inc df_exeoff
;	jsr df_rt_parm_3ints
;	; starting line number
;	ldx df_tmpptra
;	lda df_tmpptra+1
;	jsr df_pg_find_line
;	bcc df_rt_renum_ok
;	SWBRK DFERR_NOLINE
;df_rt_renum_ok
;	; save starting position pointer in ptrd
;	stx df_tmpptrd
;	sta df_tmpptrd+1
;df_rt_renum_do
;	; if not end of program
;	ldy #0
;	lda (df_tmpptrd),y
;	; then renumber this line
;	bne df_rt_renum_update
;	; else done
;	rts
;df_rt_renum_update
;	; so set this line number to new line number
;	ldy #DFTK_LINNUM
;	lda df_tmpptrb
;	sta (df_tmpptrd),y
;	iny
;	lda df_tmpptrb+1
;	sta (df_tmpptrd),y
;	; add increment to new line
;	_addZPWord df_tmpptrb,df_tmpptrc
;df_rt_renum_next
;	; point ptrd to the next line
;	clc
;	lda df_tmpptrd
;	ldx #0
;	adc (df_tmpptrd,x)
;	sta df_tmpptrd
;	_bcc 2
;	inc df_tmpptrd+1
;	jmp df_rt_renum_do


; * List all procs in VNT
df_rt_listprocnames
	; start at the beginning of the vnt table
	_cpyZPWord df_vntstrt,df_tmpptra
	; start at varcnt
	lda df_varcnt
	sta df_tmpptrb
df_rt_listcheckvnt
	; If reached 0 then not found
	lda df_tmpptrb
	beq df_rt_listpn_done
	ldy #0
	lda (df_tmpptra),y
	cmp #'_'
	bne df_rt_listnextvnt
df_rt_listprocch
	lda (df_tmpptra),y
	jsr io_put_ch
	tax
	beq df_rt_listproccr
	iny
	bne df_rt_listprocch		; Always
df_rt_listproccr
	lda #UTF_CR
	jsr io_put_ch
	clc
df_rt_listprocpause
	jsr io_get_ch
	cmp #' '
	bne df_rt_listnextvnt
df_rt_listwait
	sec
	bcs df_rt_listprocpause
df_rt_listnextvnt
	lda (df_tmpptra),y
	beq df_rt_listgotnext
	iny
	bne df_rt_listnextvnt		; Always
df_rt_listgotnext
	; dec vnt #
	dec df_tmpptrb
	; skip past zero terminator
	iny
	; add this to vnt pointer
	clc
	tya
	adc df_tmpptra
	sta df_tmpptra
	_bcc 2
	inc df_tmpptra+1
	jmp df_rt_listcheckvnt
df_rt_listpn_done
	rts

df_rt_listproc
	; A already contains '_'
	sta df_linbuff
	ldx #0
	ldy df_exeoff
df_rt_listp_copy
	iny
	inx
	lda (df_currlin),y
	sta df_linbuff,x
	jsr df_tk_isalphanum
	bcs df_rt_listp_copy
	; zero the line index
	ldx #0
	stx df_linoff
	; save runtime pos
	sty df_exeoff
	; Now try and find in VNT
	jsr df_var_find
	bcs df_rt_listp_notfound
	; Ok we have got a match in X,A find the proc
	stx df_procptr
	sta df_procptr+1
	jsr df_rt_findproc
	; Save the line pointer
	stx df_tmpptra
	sta df_tmpptra+1
	; save statement index in to line
;	sty df_lineidx
;	; Check if '-' option used
;	ldy df_exeoff
;	lda (df_currlin),y
;	cmp #'-'
;	; if so, list to end of program
;	beq df_rt_listprgend
	; Now try and find the end of this procedure
	; enddef or another def
	; A,X=Line ptr, Y=line idx
;	ldx df_tmpptra
;	lda df_tmpptra+1
;	ldy df_lineidx
df_rt_listp_findend
	; Go to next stat
	jsr df_rt_nextstat
	bcs df_rt_listprgend
	; save y (a,x in lineptr), A is ok to trample
	tya
	pha
	; find the command
df_rt_listp_findcmd
	iny
	lda (df_lineptr),y
	bpl df_rt_listp_findcmd
	; restore y to stat beginning
	sta tmp_d
	pla
	tay
	lda tmp_d
	; check A - looking for enddef or def
	cmp #DFRT_ENDDEF
	beq df_rt_listp_done
	cmp #DFRT_DEF
	beq df_rt_listp_done
	; if neither then next stat from current
	ldx df_lineptr
	lda df_lineptr+1
	jmp df_rt_listp_findend
df_rt_listp_done
	; Push end line on to stack
	lda df_lineptr+1
	pha
	txa
	pha
	lda df_lineptr+1
	jmp df_rt_list_line
df_rt_listp_notfound
	; Fatal error if proc not found
	SWBRK DFERR_NOPROC

; list token
df_rt_list
	lda #0
	sta df_tmpptre		; Zero means in normal list mode not save mode
	; find non-ws
	jsr df_rt_skip_ws
	; if end of statement then no line specifiers
	jsr df_rt_eos
	; so list whole program
	bcs df_rt_listprg

	;if '_' then use procnames
	cmp #'_'
	bne df_rt_list_all
	jmp df_rt_listproc
df_rt_list_all
	;if '*' then display all procnames
	cmp #'*'
	bne df_rt_list_linno
	jmp df_rt_listprocnames
df_rt_list_linno
	; else get 1st parameter
	jsr df_rt_getnval
	; find the starting line number in X,A
	jsr df_pg_find_line
	; save start in ptra
	stx df_tmpptra
	sta df_tmpptra+1
	jmp df_rt_listprgend
	; NO ability to choose and ending line number!

; Common listing routine used by LIST and SAVE
; tmpe = 0 means in LIST mode else SAVE mode
; can stop the listing in LIST mode with CTRL-C
df_rt_listprg
	; program start and end as for pointer value
	_cpyZPWord df_prgstrt, df_tmpptra
df_rt_listprgend
	lda df_prgend+1
	pha
	lda df_prgend
	pha
df_rt_list_line
	; if line length = 0 then end of program
	ldy #0
	lda (df_tmpptra),y
	beq df_rt_list_line_fin
	; if in list mode and CTRL-C then also stop
	lda df_tmpptre
	bne df_rt_list_line_cont
	; check for break, asynch get
	clc
df_rt_list_synckey
	lda df_tmpptre					; Ignore keys on save mode
	bne df_rt_list_line_cont
df_rt_list_pause
	jsr io_get_ch
	cmp #' '						; Space = PAUSE
	beq df_rt_list_pause			; C=1 for synchronouse key
	cmp #UTF_ETX					; CTRL-C?
	beq df_rt_list_line_fin
df_rt_list_line_cont
	ldy #0
	sty df_linoff
	jsr df_rt_list_all_line
df_rt_list_next_line
	; new line
	jsr utilPrintCRLF
	; increment pointer to next line
	clc
	lda df_tmpptra
	ldy #0
	adc (df_tmpptra),y
	sta df_tmpptra
	_bcc 2
	inc df_tmpptra+1
	; if pointer > end then listing is done
	sec
	pla
	tax
	sbc df_tmpptra
	pla
	pha
	sbc df_tmpptra+1
	txa
	pha
	bcs df_rt_list_line
df_rt_list_line_fin
	; if got here then reached tmpb
	pla
	pla
df_rt_list_line_only_fin
	rts

;Using df_tmpptra as line pointer
;Print decode an entire line
df_rt_list_all_line				; Start here to include number
	jsr df_rt_list_linnum
df_rt_list_line_only			; Start here for just the line
	ldy #3
	lda (df_tmpptra),y
	sta df_lineidx
	iny
	sty df_linoff
df_rt_list_decode
	ldy df_linoff
	lda (df_tmpptra),y
	bmi df_rt_list_token
	cmp #DFTK_ESCVAL
	bcc df_rt_list_escval
	; normal char just print it
	jsr io_put_ch
	jmp df_rt_list_nexttok
df_rt_list_escval
	; A and Y need to be valid on entry
	jsr df_rt_list_decode_esc
	jmp df_rt_list_nexttok
df_rt_list_token
	jsr df_rt_list_decode_token
df_rt_list_nexttok
	; advance the line offset
	inc df_linoff
	lda df_linoff
	; check if at end of line
	ldx #0
	cmp (df_tmpptra,x)
	beq df_rt_list_line_only_fin
	; check if at end of statement
	cmp df_lineidx
	bne df_rt_list_decode
	tay
	; save the next statement offset
	lda (df_tmpptra),y
	sta df_lineidx
	iny
	sty df_linoff
	jmp df_rt_list_decode


; decode escape sequences
; Input: A contains the esc val and Y is char line index
df_rt_list_decode_esc
	; jump over esc byte
	iny
	sty df_linoff
	pha
	; get the next two bytes in case needed
	lda (df_tmpptra),y
	sta df_tmpptrb
	iny
	lda (df_tmpptra),y
	sta df_tmpptrb+1
	dey				; Y is on byte after esc byte
	pla
	; x2 to get jmp offset
	asl a
	tax
	lda df_rt_escjmp,x
	sta df_tmpptrc
	lda df_rt_escjmp+1,x
	sta df_tmpptrc+1
	; now jump to decoder
	jmp (df_tmpptrc)

; reserved
df_rt_lst_reserved
	rts

; decode a byte char
df_rt_lst_chr
	lda #0x27			; Single quote
	jsr io_put_ch
	lda df_tmpptrb
	jsr io_put_ch
	lda #0x27			; Single quote
	jsr io_put_ch
	iny
	sty df_linoff
	rts

; Output 0x for hex chars
df_rt_lst_hex_pre
	lda #'0'
	jsr io_put_ch
	lda #'x'
	jmp io_put_ch


; Decode a byte hex
df_rt_lst_bythex
	jsr df_rt_lst_hex_pre
df_rt_lst_lo_hex
	lda df_tmpptrb
	jsr str_a_to_x
	jsr io_put_ch
	txa
	jsr io_put_ch
df_rt_lst_const_done
	iny
	sty df_linoff
	rts

; Decode an int hex
df_rt_lst_inthex
	jsr df_rt_lst_hex_pre
	lda df_tmpptrb+1
	jsr str_a_to_x
	jsr io_put_ch
	txa
	jsr io_put_ch
	jmp df_rt_lst_lo_hex

; Decode a byte binary
df_rt_lst_bytbin
	lda df_tmpptrb
	sta df_tmpptrb+1
	ldx #8
	bne df_rt_lst_bin

; Decode a int binary
df_rt_lst_intbin
	ldx #16
	; FALL THROUGH
; Main 01 decoding of binary
df_rt_lst_bin
	lda #'0'
	jsr io_put_ch
	lda #'b'
	jsr io_put_ch
df_rt_lst_bit
	lda #'0'
	asl df_tmpptrb
	rol df_tmpptrb+1
	adc #0				; If C=1 then '0' becomes '1'
df_rt_lst_bit_skip0
	jsr io_put_ch
	dex
	bne df_rt_lst_bit
	beq df_rt_lst_const_done

; Decode a decimal integer
df_rt_lst_intdec
	ldx df_tmpptrb
	lda df_tmpptrb+1
	iny
	sty df_linoff
	clc
	jmp print_a_to_d


; decode a variable or procedure
; Slot address to decode in ptrb
df_rt_lst_var
df_rt_lst_proc
	; jump over the address bytes
	iny
	sty df_linoff

	; ptrc starts at VNT start
	_cpyZPWord df_vntstrt,df_tmpptrc

	; ptrd starts at end of VVT
	_cpyZPWord df_vvtend,df_tmpptrd
	ldy #0
df_rt_list_findvvt
	; Check have we reached target addr in vvt?
	lda df_tmpptrd
	cmp df_tmpptrb
	bne df_rt_list_vvtend
	lda df_tmpptrd+1
	cmp df_tmpptrb+1
	beq df_rt_list_gotvvt
df_rt_list_vvtend
	lda (df_tmpptrc),y
	beq df_rt_list_gotvvtend
	_incZPWord df_tmpptrc
	jmp df_rt_list_vvtend
df_rt_list_gotvvtend
	_incZPWord df_tmpptrc
	; move target slot address towards top of mem
	clc
	lda df_tmpptrd
	adc #DFVVT_SZ
	sta df_tmpptrd
	_bcc 2
	inc df_tmpptrd+1
	; go back and check again
	jmp df_rt_list_findvvt
df_rt_list_gotvvt
	lda (df_tmpptrc),y
	beq df_rt_list_donvvt
	jsr io_put_ch
	_incZPWord df_tmpptrc
	jmp df_rt_list_gotvvt

df_rt_lst_strlit
	lda #0x22
	jsr io_put_ch
	ldy df_linoff
df_rt_lst_strlitch
	lda (df_tmpptra),y
	beq df_rt_lst_strlitdon
	jsr io_put_ch
	iny
	jmp df_rt_lst_strlitch
df_rt_lst_strlitdon
	lda #0x22
	jsr io_put_ch
	sty df_linoff
df_rt_list_donvvt
	rts

df_rt_list_linnum
	ldy #1
	lda (df_tmpptra),y
	tax
	iny
	lda (df_tmpptra),y
	clc
	jsr print_a_to_d
	lda #0x20			; Always add a space after line num
	jmp io_put_ch

; decode a token value with MSB set
df_rt_list_decode_token
	; if not assembler then normal listing
	cmp #DFRT_ASM
	bne df_rt_list_decode_token_normal
	jmp df_rt_asm_decode_token
df_rt_list_decode_token_normal
	and #0x7f
	; token 0 and 1 don't get decoded they are implicit
	cmp #2
	bcs df_rt_list_do_decode_tkn
	rts
df_rt_list_do_decode_tkn
	tax
	lda #lo(df_tokensyms)
	sta df_tmpptrb
	lda #hi(df_tokensyms)
	sta df_tmpptrb+1
df_rt_list_find_sym
	txa
	beq df_rt_list_got_sym
	ldy #0
df_rt_list_next_ch
	lda (df_tmpptrb),y
	pha
	_incZPWord df_tmpptrb
	pla
	bpl df_rt_list_next_ch
df_rt_list_got_last_sym
	; ok got to the last ch
	; advance to next sym
	dex
	jmp df_rt_list_find_sym
df_rt_list_got_sym
	lda (df_tmpptrb,x)		; Relies on X=0 from branch
	php
	and #0x7f
	jsr io_put_ch
	_incZPWord df_tmpptrb
	plp
	bpl df_rt_list_got_sym
	rts

;** Decode assembler token in A **
df_rt_asm_decode_token
	inc df_linoff		; Point to token after asm token
	ldy df_linoff
	lda (df_tmpptra),y	;If token N=1 then keyword
	bmi df_rt_asm_decode_token_keyword
	lda #'.'			;Put the '.' before escape processing
	jsr io_put_ch
	lda (df_tmpptra),y	;Get asm token back
	jmp df_rt_list_decode_esc
df_rt_asm_decode_token_keyword
	and #0x7f			; Mask off MSB
	tax					;Put it in to X as the counter
	; Point to asm symbol table
	lda #lo(df_asm_tokensyms)
	sta df_tmpptrb
	lda #hi(df_asm_tokensyms)
	sta df_tmpptrb+1
df_rt_list_find_asm_sym
	txa
	beq df_rt_list_got_asm_sym
	ldy #0
df_rt_list_next_asm_ch
	_incZPWord df_tmpptrb
	lda (df_tmpptrb),y
	cmp #' '			; Skip all chars >=' '
	bcs df_rt_list_next_asm_ch
	sec					; Skip offset and mode bytes
	adc df_tmpptrb
	sta df_tmpptrb
	_bcc 2
	inc df_tmpptrb+1
	dex					; One less symbol to skip over
	jmp df_rt_list_find_asm_sym
df_rt_list_got_asm_sym
	lda (df_tmpptrb,x)	; Relies on X=0 from branch
	cmp #' '
	bcc df_rt_asm_decode_token_done
	jsr io_put_ch
	_incZPWord df_tmpptrb
	jmp df_rt_list_got_asm_sym
df_rt_asm_decode_token_done
	rts


df_rt_doke
	jsr df_rt_parm_2ints
	lda df_tmpptrb
	ldy #0
	sta (df_tmpptra),y
	; get high byte to doke
	lda df_tmpptrb+1
	iny
	; poke hi byte
	sta (df_tmpptra),y
	rts

df_rt_poke
	jsr df_rt_parm_2ints
	lda df_tmpptrb
	ldy #0
	sta (df_tmpptra),y
	rts

df_rt_snd_common
	; 3 inputs
	; tmpa = channel (1,2,3), tmpb = period, tmpc = vol
	lda df_tmpptra
	; tone channel addressing is 0 to 2
	sec
	sbc #1
	and #3
	; ok doing a tone channel, get reg index for period
	asl a
	tax
	; get low byte of period
	lda df_tmpptrb
	jsr snd_set
	; increment reg number to high byte
	inx
	; but if now at R7 then it's was noise period
	; so then no high period or volume
	cpx #7
	bne df_rt_not_noise
	; else do nothing more
	rts
df_rt_not_noise
 	; get high byte of period
	lda df_tmpptrb+1
	and #0x0f
	; set hi period
	jsr snd_set
	; get volume register index (8 = channel 1)
	clc
	lda df_tmpptra
	and #3
	adc #7
	tax
	; get volume
	lda df_tmpptrc
	and #0x0f
	bne df_rt_sound_env_skip
	; envelope mode
	ora #0x10
df_rt_sound_env_skip
	jmp snd_set

; sound chan,period,volume
df_rt_sound
	jsr df_rt_parm_3ints
df_rt_dosound
	jmp df_rt_snd_common

; music chan,octave,note,volume
df_rt_music
	jsr df_rt_parm_4ints
	; parm 2 = octave, need to x 12word = 24
	; get period A,X (hi/lo)
	; pass in octave # in X and note # in Y
	ldx df_tmpptrb
	ldy df_tmpptrc
	jsr snd_get_note
	; store note period in b
	stx df_tmpptrb
	sta df_tmpptrb+1
	; put vol in tmpc
	lda df_tmpptrd
	sta df_tmpptrc
	; tmpa,b,c contain chan,per,vol
	jmp df_rt_dosound


; play tonemask,noisemask,envelope,period
df_rt_play
	jsr df_rt_parm_4ints
	; parm 1 = tone enable
	lda df_tmpptra
	and #7
	sta df_tmpptra
	; parm 2 = noise enable
	lda df_tmpptrb
	and #7
	asl a
	asl a
	asl a
	ora df_tmpptra
	; we now have bits set for channels to enable
	; but need to invert for the 8910
	; top 2 bits are 0 and 1 as these are port b (in) and a (out)
	eor #0x7f
	; reg 7 is control register
	ldx #7
	jsr snd_set
	; parm 4 = envelope period
	; 11 is envelope period register
	ldx #11
	; get low
	lda df_tmpptrd
	jsr snd_set
	; get high
	inx
	lda df_tmpptrd+1
	jsr snd_set
	; parm 3 = envelope mode
	lda df_tmpptrc
	and #0xf
	; 13 is envelope shape register
	ldx #13
	jmp snd_set

;df_rt_fill
;	jsr df_rt_parm_5ints
;	rts
	
;* common filename procesing routine
;*
df_rt_init_filename
	; evaluate string
	jsr df_rt_neval
	jsr df_ost_popStr

	; save string address
	stx df_tmpptrc
	sta df_tmpptrc+1

	; Check first 2 chars of string
	; if s: then device = sd card
	; if t: then device = tape
	lda #0							; Assume tape i.e. device 0
	pha
	ldy #1
	lda (df_tmpptrc),y
	dey								; Y=0 i.e. assume no s: or t:
	cmp #':'						; if no ':' in pos 1 then tape
	bne df_rt_do_fname
	lda (df_tmpptrc),y				; get first char
	ldy #2							; filename must start at pos 2
	cmp #'t'						; if t then still tape
	beq df_rt_do_fname
	cmp #'s'						; MUST be s else an error!
	bne df_rt_file_errc
	pla
	lda #1							; Set to sd card
	pha
df_rt_do_fname
	; copy string to fhandle, Y is at start pos of filename
	ldx #0
df_rt_copy_fn
	lda (df_tmpptrc),y
df_rt_fname_case
	sta df_linbuff,x				; Put filename in line buffer
	iny
	inx
	cmp #0
	bne df_rt_copy_fn
	pla								; Get device number in A
	rts

; Delete a file from SD card
; Only valid for SD card
df_rt_delete
	jsr df_rt_parse_file
	cmp #1
	bne df_rt_file_errc
	jsr sd_delete
	jmp io_set_default

df_rt_dir
	jmp sd_dir

;* common file parsing routine
df_rt_parse_file
	; now process filename
	jsr df_rt_init_filename
	; device number is in A (0=tape, 1=sdcard)
	pha								; Save sub-device number
	lda #0							; Always 0 for file device
	jsr io_active_device
	pla								; Exit with device number found
	rts
df_rt_file_errc
	SWBRK DFERR_FNAME

; save "file" as text
df_rt_tsave
	jsr df_rt_parse_file
	jsr io_open_write
	bcs df_rt_file_errc
	; ok now have redirected output to device
	; go and list the program in save mode
	lda #1
	sta df_tmpptre
	jsr df_rt_listprg
	; final CR to end the save
	lda #UTF_CR
	jsr io_put_ch
df_rt_file_cleanup
	; close the file
	jsr io_close
	; restore to default device io
	jmp io_set_default


; load "file" from text
df_rt_tload
	jsr df_rt_parse_file
	jsr io_open_read
	bcs df_rt_file_errc
	; no echo - very important
	; else might try and write to a device
	; only open for reading (i.e. SD CARD)
df_rt_loadline
	clc					; NO ECHO!
	jsr df_pg_inputline
	; if C clear then tokenise line
	bcc df_rt_ldtokenise
	; else done
	; clear dflat runtime else will try to execute
	; the last tokenised line!
	ldx #0
	stx df_tokbuff			; Offset to next line
	stx df_tokbuff+1		; Clear line low
	stx df_tokbuff+2		; Clear line high
	stx df_nxtstidx			; Clear next statement
	stx df_eolidx			; Clear end of line too
	inx						; Set immediate mode
	stx df_immed
	jmp df_rt_file_cleanup	; Ok now can close and done
df_rt_ldtokenise
	jsr df_pg_tokenise		; Tokenise loaded string
	jmp df_rt_loadline		; Continue with next until blank


; Utility to open in binary mode save
df_rt_openforbinsave
	jsr df_rt_parse_file
	jsr io_open_ext2		; Ext2 is binary file save
	bcs df_rt_file_errc
	rts

; Utility to open in binary mode load
df_rt_openforbinload
	; Get filename and open file for binary read
	jsr df_rt_parse_file
	jsr io_open_ext1		; Ext1 is binary file read
	bcs df_rt_file_errc
	rts

; bload addr,"file"
df_rt_bload
	; Get address but keep on stack
	inc df_exeoff
	jsr df_rt_neval

	; Get filename and open file for binary read
	inc df_exeoff
	jsr df_rt_openforbinload

	; Get address from stack in to ptra
	jsr df_ost_popInt
	stx df_tmpptra
	sta df_tmpptra+1

	; Get file address to X,Y
	jsr io_get_ch
	tax
	jsr io_get_ch
	tay

	; Check if user address is zero
	lda df_tmpptra
	ora df_tmpptra+1
	bne df_rt_bload_addr
	; If it is zero then use address from file
	stx df_tmpptra
	sty df_tmpptra+1
df_rt_bload_addr
	; Get file length
	jsr io_get_ch
	sta df_tmpptrb
	jsr io_get_ch
	sta df_tmpptrb+1

	; Go and load the bytes
	jsr df_rt_loadbin
	; Close the file
	jmp df_rt_file_cleanup

; Save a binary file
; addr,length,fname
df_rt_bsave
	; Get source address and length but leave on stack!
	; evaluate 1st parm
	jsr df_rt_neval
	; jump over comma
	inc df_exeoff
	; evaluate the 2nd parm
	jsr df_rt_neval

	; Process file and open for binary save
	inc df_exeoff
	jsr df_rt_openforbinsave

	; Get length in to ptrb
	jsr df_ost_popInt
	stx df_tmpptrb
	sta df_tmpptrb+1

	; Get address in to ptra
	jsr df_ost_popInt
	stx df_tmpptra
	sta df_tmpptra+1

	; now save bytes
	jsr df_rt_savebin
	; Close the file
	jmp df_rt_file_cleanup


; save dflat tokenised program as binary
;df_rt_save
;	; Process file and open for binary save
;	jsr df_rt_openforbinsave
;
;	; first save zero page stuff
;	; ok this saves a bit more than needed
;	; but it's no biggie and doesn't
;	; clobber temp space
;	lda #lo(dflat_zp_save_s)
;	sta df_tmpptra
;	lda #hi(dflat_zp_save_s)
;	sta df_tmpptra+1
;	; save length
;	sec
;	lda #lo(dflat_zp_save_e)
;	sbc #lo(dflat_zp_save_s)
;	sta df_tmpptrb
;	lda #hi(dflat_zp_save_e)
;	sbc #hi(dflat_zp_save_s)
;	sta df_tmpptrb+1
;	; now save bytes
;	jsr df_rt_savebin
;
;	; now save the dflat program
;	lda df_prgstrt
;	sta df_tmpptra
;	lda df_prgstrt+1
;	sta df_tmpptra+1
;	; save length
;	sec
;	lda df_prgend
;	sbc df_prgstrt
;	sta df_tmpptrb
;	lda df_prgend+1
;	sbc df_prgstrt+1
;	sta df_tmpptrb+1
;	; now save bytes
;	jsr df_rt_savebin
;
;	; now save the variables VVT and VNT
;	lda df_vntstrt
;	sta df_tmpptra
;	lda df_vntstrt+1
;	sta df_tmpptra+1
;	; save length
;	sec
;	lda df_vvtstrt
;	sbc df_vntstrt
;	sta df_tmpptrb
;	lda df_vvtstrt+1
;	sbc df_vntstrt+1
;	sta df_tmpptrb+1
;	; now save bytes
;	jsr df_rt_savebin
;
;	; close the file
;	jsr io_close
;	clc
;	; Close the file
;	jmp df_rt_file_cleanup


; load dflat tokenised program as binary
;df_rt_load
;	jsr df_rt_openforbinload
;
;	; Get zero page header
;	jsr df_rt_getbin_parms
;	; and get bytes
;	jsr df_rt_loadbin
;
;	; Get program header
;	jsr df_rt_getbin_parms
;	; and get bytes
;	jsr df_rt_loadbin
;
;	; Get variables header
;	jsr df_rt_getbin_parms
;	; and get bytes
;	jsr df_rt_loadbin
;
;	; close the file
;	jsr io_close
;	clc
;	; Close the file
;	jmp df_rt_file_cleanup
;

; Utility to get 4 parms from binary header
;df_rt_getbin_parms
;	ldx #0
;df_rt_getbin_parms_loop
;	jsr io_get_ch
;	sta df_tmpptra,x
;	inx
;	cpx #4
;	bne df_rt_getbin_parms_loop
;	rts

; Utility to load a bin file in address ptra
; Length in ptr b
df_rt_loadbin
	; Now load all bytes
	ldy #0
df_rt_loadbin_byte
	; Get a byte from tape
	jsr io_get_ch
	; Save it to destination
	sta (df_tmpptra),y
	; Increment destination
	iny
	bne df_rt_loadbin_inc
	inc df_tmpptra+1
df_rt_loadbin_inc
	; Decrement length remaining
	jsr df_rt_dec_binlen
	; Not done, do another byte
	bne df_rt_loadbin_byte
	; Yes the close the file
	rts

; Utility to save a bin file from address ptra
; Length in ptr b
df_rt_savebin
	; Save dest and length to tape
	ldx #0
df_rt_bsave_parms
	lda df_tmpptra,x
	jsr io_put_ch
	inx
	cpx #4
	bne df_rt_bsave_parms

	; Now save all bytes
	ldy #0
df_rt_savebin_byte
	; Get byte from memory
	lda (df_tmpptra),y
	; Put byte to tape
	jsr io_put_ch
	; Increment destination
	iny
	bne df_rt_savebin_inc
	inc df_tmpptra+1
df_rt_savebin_inc
	; Decrement length remaining
	jsr df_rt_dec_binlen
	; Not done, do another byte
	bne df_rt_savebin_byte
	rts

; Decrement length in ptrb
df_rt_dec_binlen
	; Decrement length remaining
	lda df_tmpptrb
	bne df_rt_binlen_skip
	dec df_tmpptrb+1
df_rt_binlen_skip
	dec df_tmpptrb
	; Reached zero?
	lda df_tmpptrb
	ora df_tmpptrb+1
	rts


; reset var
df_rt_reset
	; now get lvar X,A from current statement
	jsr df_rt_getlvar
	; save lvar in tmpb, vvt ptr in tmpa
	stx df_tmpptrb
	sta df_tmpptrb+1
	; load the vdp count as the reset value of timer
	; turn off interrupts while reading vdp lo,hi
	ldy #0	; This is in readiness to read high byte of var value
	; clear interrupts to access 3 byte vdp counter (only use low 2 bytes)
	sei
	lda vdp_cnt
	sta (df_tmpptrb),y
	lda vdp_cnt+1
	iny
	sta (df_tmpptrb),y
	; restore interrupts asap
	cli
	rts

;***** FUNCTIONS *****

df_rt_deek
	sec
	db 0x24	; BIT skip the clc
df_rt_peek
	clc
df_rt_readbyte
	php
;	inc df_exeoff
	jsr df_rt_getnval
	stx df_tmpptra
	sta df_tmpptra+1
	ldy #0
	lda (df_tmpptra),y
	tax
	lda #0
	plp
	bcc df_rt_readbyte_skip
	iny
	lda (df_tmpptra),y
df_rt_readbyte_skip
	jmp df_ost_pushInt

; Random number generator
; rnd(0) = get next number
; rnd(>0) = set seed
df_rt_rnd
;	inc df_exeoff
	jsr df_rt_getnval
	; if input is 0 then generate next random number
	cpx #0
	bne df_rt_rnd_set
	cmp #0
	bne df_rt_rnd_set
	; generate next number
	lda df_rnd+1
	lsr a
	rol df_rnd
	bcc df_rt_rnd_noeor
	eor #0xb4
df_rt_rnd_noeor
	sta df_rnd+1
	eor df_rnd
	jmp df_ost_pushIntA
	; else set the seed to that number and done
df_rt_rnd_set
	stx df_rnd
	sta df_rnd+1
	jmp df_ost_pushInt


;* Return memory footprint as follows:
;* 0	Return free memory (start of vnt - end of heap)
;* 1	Return program size (end of prg - start of prg)
;* 2	Return size of vars (end of vvt - start of vnt)
df_rt_mem
;	inc df_exeoff
	jsr df_rt_getnval
	; only low byte is used
	cpx #1
	beq df_rt_mem_prg
	cpx #2
	beq df_rt_mem_var
	; default is free memory
df_rt_mem_free
	_cpyZPWord df_vntstrt,df_tmpptra
	_cpyZPWord df_starend,df_tmpptrb
	jmp df_rt_mem_calc
df_rt_mem_prg
	_cpyZPWord df_prgend,df_tmpptra
	_cpyZPWord df_prgstrt,df_tmpptrb
	jmp df_rt_mem_calc
df_rt_mem_var
	_cpyZPWord df_vvtend,df_tmpptra
	_cpyZPWord df_vntstrt,df_tmpptrb
df_rt_mem_calc
	; tmpa-tmpb result in X,A
	sec
	lda df_tmpptra
	sbc df_tmpptrb
	tax
	lda df_tmpptra+1
	sbc df_tmpptrb+1
	jmp df_ost_pushInt

;* a=stick()
;* returns bit condition of joystick positions
;* no actual joystick support so this is
;* Check for fire | down | up | right | left
;*        bit  4     3      2     1       0
df_rt_stick
;	inc df_exeoff
	jsr kb_stick				; Get pos in to A
	jmp df_ost_pushIntA

; k=get(sync) sync>=1 means sync
df_rt_get
;	inc df_exeoff
	jsr df_rt_getnval
	; only low byte is used, check for sync or async
	; c=0 if x<1 else x>=1 makes c=1
	cpx #1
df_rt_get_sync
	php
	jsr io_get_ch				; Return in A, C=0 is good
	bcc df_rt_get_pushp			; Push A (and do plp)
	plp
	bcs df_rt_get_sync			; If sync then check again
	bcc df_rt_get_push			; Else push zero (no plp)
df_rt_get_pushp
	plp
df_rt_get_push
	jmp df_ost_pushIntA

; s = scrn(x,y)
df_rt_scrn
;	inc df_exeoff
	jsr df_rt_parm_2ints
	ldy df_tmpptra			; Y is the x coord!
	ldx df_tmpptrb			; X is the y coord!
	jsr gr_get
	jmp df_ost_pushIntA

; p = pixel(x,y)
df_rt_pixel
;	inc df_exeoff
	jsr df_rt_parm_2ints
	ldx df_tmpptra
	ldy df_tmpptrb
	jsr gr_pixel
	jmp df_ost_pushIntA


; e=elapsed(var)
df_rt_elapsed
	; now get lvar X,A from current statement
	jsr df_rt_getlvar
	inc df_exeoff
	; save lvar in tmpb, vvt ptr in tmpa
	stx df_tmpptrb
	sta df_tmpptrb+1
	; subtract vdp counter from value
	; turn off interrupts while reading vdp lo,hi
	ldy #0	; This is in readiness to read high byte of var value
	sec
	; disable interrupts to access vdp counter
	sei
	lda vdp_cnt
	sbc (df_tmpptrb),y
	tax
	lda vdp_cnt+1
	; restore interrupts asap
	cli
	iny
	sbc (df_tmpptrb),y
	jmp df_ost_pushInt

df_rt_call
;	inc df_exeoff
	jsr df_rt_parm_4ints
	lda df_tmpptrb				; load A
	ldx	df_tmpptrc				; load X
	ldy df_tmpptrd				; load Y
	jsr df_rt_calljsr
df_rt_push_int1
	jmp df_ost_pushInt			; A,X pair is return value
df_rt_calljsr
	jmp (df_tmpptra)			; tmpptra is address, return with RTS

df_rt_sgn
;	inc df_exeoff
	jsr df_rt_getnval
	stx df_tmpptra
	ora df_tmpptra
	beq df_rt_sgn_z
	and #0x80
	bne df_rt_sgn_n
	ldx #1
	lda #0
	beq df_rt_push_int1			; Always
df_rt_sgn_n
	ldx #0xff
	txa
	bne df_rt_push_int1			; Always
df_rt_sgn_z
	ldx #0
	txa
	beq df_rt_push_int1			; Always

; addr(X)
df_rt_addr
	; get lvar X,A from current statement
	jsr df_rt_getlvar
	inc df_exeoff
	jmp df_ost_pushInt

; string length calculator
; X,A = source
; A = length not including zero
df_rt_strlen_common
	stx df_tmpptra
	sta df_tmpptra+1
	ldy #0xff
df_rt_strlen_count
	iny
	lda (df_tmpptra),y
	bne df_rt_strlen_count
	tya
	rts


; common routine to extract a string
; tmpa = source string
; tmpb = dest string
; tmpc = start pos
; tmpd = endpos
df_rt_str_extract
	; source string
	jsr df_ost_popStr
	stx df_tmpptra
	sta df_tmpptra+1
	; destination is string accumulator
	lda df_sevalptr
	sta df_tmpptrb
	lda df_sevalptr+1
	sta df_tmpptrb+1
	; start pos
	ldy df_tmpptrc
	ldx #0
df_rt_str_cpy_ch
	cpy df_tmpptrd
	beq df_str_src_end
	lda (df_tmpptra),y
	beq df_str_src_end
	sta (df_tmpptrb,x)
	_incZPWord df_tmpptrb
	iny
	bne df_rt_str_cpy_ch
	SWBRK DFERR_STRLONG
df_str_src_end
	lda #0
	sta (df_tmpptrb,x)
	ldx df_sevalptr
	lda df_sevalptr+1
	jmp df_ost_pushStr

; $c = chr(x)
df_rt_chr
;	inc df_exeoff
	; get char in X
	jsr df_rt_getnval
	ldy #0
	; transfer lo byte to A
	txa
	sta (df_sevalptr),y
	iny
	; zero terminator
	lda #0
	sta (df_sevalptr),y
	; point to seval scratch area
	ldx df_sevalptr
	lda df_sevalptr+1
	jmp df_ost_pushStr

; $c = hex(x)
df_rt_hex
;	inc df_exeoff
	; create hex digits
	jsr df_rt_getnval
	sta df_tmpptra	; Save the high byte
	txa				; Convert low byte first
	jsr str_a_to_x	; Hex digits in A,X
	sta df_tmpptrb
	txa				; Push low digit of low byte from X
	pha
	lda df_tmpptrb	; Get A back from temp
	pha				; Push high digit of low byte
	lda df_tmpptra	; Get the high byte
	jsr str_a_to_x	; Hex digits in A,X
	; create string
	ldy #0			; Index in to string temp area
	; hi/hi
	sta (df_sevalptr),y
	iny
	; hi/lo
	txa
	sta (df_sevalptr),y
	iny
	; lo/hi
	pla
	sta (df_sevalptr),y
	iny
	; lo/lo
	pla
	sta (df_sevalptr),y
	iny
	; zero terminator
	lda #0
	sta (df_sevalptr),y
	; point to seval scratch area
	ldx df_sevalptr
	lda df_sevalptr+1
	jmp df_ost_pushStr

; $c = dec(x)
df_rt_dec
;	inc df_exeoff
	; create dec digits
	jsr df_rt_getnval
	jsr int_to_str
	; point to num_buf scratch area
	ldx #lo(num_buf)
	lda #hi(num_buf)
	jmp df_ost_pushStr


; $l = left($s, x)
df_rt_left
;	inc df_exeoff

	; first get the string to act on
	; point to string accumulator
	jsr df_rt_seval
	; now get the num of chars
	inc df_exeoff
	jsr df_rt_getnval
	; number of chars to extract
	stx df_tmpptrd
	; start position
	ldy #0
	sty df_tmpptrc
	jmp df_rt_str_extract

; $r = right($s, x)
df_rt_right
;	inc df_exeoff
	; first get the string to act on
	; point to string accumulator
	jsr df_rt_seval
	; now get the num of chars from the right
	inc df_exeoff
	jsr df_rt_getnval
	; number of chars to extract from the right
	stx df_tmpptrc
	; end pos = len
	ldx df_sevalptr
	lda df_sevalptr+1
	jsr df_rt_strlen_common
	sta df_tmpptrd
	; subtract num chars to extract to get start pos
	sec
	sbc df_tmpptrc
	sta df_tmpptrc
	jmp df_rt_str_extract

; $m = mid($s, x, y)
df_rt_mid
;	inc df_exeoff
	; first get the string to act on
	; point to string accumulator
	jsr df_rt_seval
	; now get start of string segment
	inc df_exeoff
	jsr df_rt_neval
	; number of chars to extract
	inc df_exeoff
	jsr df_rt_getnval
	stx df_tmpptrd
	; start position
	jsr df_ost_popInt
	dex					; zero offset rather than 1
	stx df_tmpptrc
	; update end pos by adding start pos
	txa
	clc
	adc df_tmpptrd
	sta df_tmpptrd
	jmp df_rt_str_extract

; %l = len($s)
df_rt_len
;	inc df_exeoff
	; evaluate the string in the string accumulator
	jsr df_rt_seval
	jsr df_ost_popStr
	; now calculate the length of this string
	jsr df_rt_strlen_common
	jmp df_ost_pushIntA

; %l = asc($s)
df_rt_asc
;	inc df_exeoff
	; Evaluate string in the string accumulator
	jsr df_rt_seval
	jsr df_ost_popStr
	; Store point in ZP
	stx df_tmpptra
	sta df_tmpptra+1
	; Find the character at beginning
	ldx #0
	lda (df_tmpptra,x)
	jmp df_ost_pushIntA

; %l = val($s)
df_rt_val
;	inc df_exeoff
	; evaluate the string
	jsr df_rt_seval
	jsr df_ost_popStr
	ldy #0				; any numeric format
	jsr con_n_to_a		; result in num_a
	bcs df_rt_val_err
	ldx num_a
	lda num_a+1
	; Save as an int
	jmp df_ost_pushInt
df_rt_val_err
	SWBRK DFERR_TYPEMISM

; stop execution
df_rt_abort
	SWBRK DFERR_ABORT

df_rt_sprchar
	jsr df_rt_parm_2ints
	lda df_tmpptra
	ldx df_tmpptrb
	jmp gr_spr_char

df_rt_sprpos
	jsr df_rt_parm_3ints
	lda df_tmpptra
	ldx df_tmpptrb
	ldy df_tmpptrc
	jmp gr_spr_pos

df_rt_sprupd
	jsr gr_spr_erase
	jsr gr_spr_new
	jmp gr_spr_draw

df_rt_sprinit
	jmp gr_spr_init

df_rt_sprmulti
	jsr df_rt_parm_2ints
	jmp gr_spr_multi_pos

df_rt_sprhit
;	inc df_exeoff
	jsr df_rt_getnval
	txa
	jsr gr_spr_hit
	bcs df_rt_sprhit_inactive
	; Active sprite hit = background
	jmp df_ost_pushIntA
df_rt_sprhit_inactive
	; Inactive sprite hit = -1
	ldx #0xff
	txa
	jmp df_ost_pushInt


mod_sz_rtsubs_e

