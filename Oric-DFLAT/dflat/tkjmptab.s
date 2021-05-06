;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  TKJUMPTAB.S
;*  Runtime token jump table.
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
; In token order of df_tokensyms
df_tk_tokenjmp
	dw	df_tk_assign
	dw	df_tk_callproc
	dw	df_tk_comment
	dw	df_tk_println
	dw	df_tk_printat
	dw	df_tk_print
	dw	df_tk_def
	dw	df_tk_enddef
	dw	df_tk_return
	dw	df_tk_abort
	dw	df_tk_local
	dw	df_tk_dim
	dw	df_tk_repeat
	dw	df_tk_until
	dw	df_tk_for
	dw	df_tk_next
	dw	df_tk_while
	dw	df_tk_wend
	dw	df_tk_if
	dw	df_tk_else
	dw	df_tk_endif
	dw	df_tk_elseif
	dw	df_tk_data
	dw	df_tk_asm_parse_command
	dw	df_tk_run
	dw	df_tk_list
	dw	df_tk_input
	dw	df_tk_text
	dw	df_tk_plot
	dw	df_tk_cursor
	dw	df_tk_cls
	dw	df_tk_poke
	dw	df_tk_doke
	dw	df_tk_sound
	dw	df_tk_music
	dw	df_tk_play
	dw	df_tk_save
	dw	df_tk_load
	dw	df_tk_read
	dw	df_tk_new
	dw	df_tk_renum
	dw	df_tk_wait
	dw	df_tk_reset
	dw	df_tk_hires
	dw	df_tk_point
	dw	df_tk_line
	dw	df_tk_lineto
	dw	df_tk_pixmode
	dw	df_tk_ink
	dw	df_tk_paper
	dw	df_tk_circle
;	dw	df_tk_fill
	dw	df_tk_himem
	dw	df_tk_monitor
	dw	df_tk_sprchar
	dw	df_tk_sprpos
	dw	df_tk_sprupd
	dw	df_tk_sprinit
	dw	df_tk_sprmulti
	dw	df_tk_bsave
	dw	df_tk_bload
	dw	df_tk_tsave
	dw	df_tk_tload
	
	dw	df_tk_peek
	dw	df_tk_deek
	dw	df_tk_stick
	dw	df_tk_get
	dw	df_tk_chr
	dw	df_tk_left
	dw	df_tk_right
	dw	df_tk_mid
	dw	df_tk_len
	dw	df_tk_mem
	dw	df_tk_scrn
	dw	df_tk_rnd
	dw	df_tk_elapsed
	dw	df_tk_call
	dw	df_tk_hex
	dw	df_tk_asc
	dw	df_tk_val
	dw	df_tk_pixel
	dw	df_tk_sprhit
	dw	df_tk_sgn
	dw	df_tk_addr
	
	dw	df_tk_mult
	dw	df_tk_div
	dw	df_tk_mod
	dw	df_tk_asl
	dw	df_tk_lsr
	dw	df_tk_add
	dw	df_tk_sub
	
	dw	df_tk_and
	dw	df_tk_or
	dw	df_tk_eor
	dw	df_tk_lte
	dw	df_tk_gte
	dw	df_tk_ne
	dw	df_tk_lt
	dw	df_tk_gt
	dw	df_tk_eq


	
	
	
	
	