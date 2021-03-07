;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  RTJUMPTAB.S
;*  Runtime token jump table.
;*  dflat uses four key tables to tokenise and run programs:
;*  - df_tokensyms    - table of token symbols
;*  - df_tk_tokentype - table of token types
;*  - df_tk_tokenjmp  - table of tokenising routines
;*  - df_rt_tokenjmp  - table of runtime token and escape routines
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
df_rt_tokenjmp
	dw	df_rt_assign
	dw	df_rt_proc
	dw	df_rt_comment
	dw	df_rt_println
	dw	df_rt_printat
	dw	df_rt_print
	dw	df_rt_def			; 0x86
	dw	df_rt_enddef		; 0x87
	dw	df_rt_return		; 0x88
	dw	df_rt_abort			; 0x89
	dw	df_rt_local
	dw	df_rt_dim
	dw	df_rt_repeat		; 0x8c
	dw	df_rt_until
	dw	df_rt_for			; 0x8e
	dw	df_rt_next			; 0x8f
	dw	df_rt_while			; 0x90
	dw	df_rt_wend			; 0x81
	dw	df_rt_if			; 0x92
	dw	df_rt_else			; 0x93
	dw	df_rt_endif			; 0x94
	dw	df_rt_elseif		; 0x95
	dw	df_rt_data			; 0x96
	dw	df_rt_asm_assemble	; 0x97 df_rt_asm_assemble
	dw	df_rt_run
	dw	df_rt_list
	dw	df_rt_input
	dw	df_rt_text
	dw	df_rt_plot
	dw	df_rt_cursor
	dw	df_rt_cls
	dw	df_rt_poke
	dw	df_rt_doke
	dw	df_rt_sound
	dw	df_rt_music
	dw	df_rt_play
	dw	df_rt_save
	dw	df_rt_load
	dw	df_rt_read
	dw	df_rt_new
	dw	df_rt_renum
	dw	df_rt_wait
	dw	df_rt_reset
	dw	df_rt_hires
	dw	df_rt_point
	dw	df_rt_line
	dw	df_rt_lineto
	dw	df_rt_pixmode
	dw	df_rt_ink
	dw	df_rt_paper
	dw	df_rt_circle
	dw	df_rt_himem
	dw	df_rt_monitor
	dw	df_rt_sprchar
	dw	df_rt_sprpos
	dw	df_rt_sprupd
	dw	df_rt_sprinit
	dw	df_rt_sprmulti
	dw	df_rt_bsave
	dw	df_rt_bload
	dw	df_rt_tsave
	dw	df_rt_tload

	dw	df_rt_peek
	dw	df_rt_deek
	dw	df_rt_stick
	dw	df_rt_get
	dw	df_rt_chr
	dw	df_rt_left
	dw	df_rt_right
	dw	df_rt_mid
	dw	df_rt_len
	dw	df_rt_mem
	dw	df_rt_scrn
	dw	df_rt_rnd
	dw	df_rt_elapsed
	dw	df_rt_call
	dw	df_rt_hex
	dw	df_rt_asc
	dw	df_rt_val
	dw	df_rt_pixel
	dw	df_rt_sprhit
	
	dw	df_rt_mult
	dw	df_rt_div
	dw	df_rt_mod
	dw	df_rt_asl
	dw	df_rt_lsr
	dw	df_rt_add
	dw	df_rt_sub
	
	dw	df_rt_and
	dw	df_rt_or
	dw	df_rt_eor
	dw	df_rt_comlte
	dw	df_rt_comgte
	dw	df_rt_comne
	dw	df_rt_comlt
	dw	df_rt_comgt
	dw	df_rt_comeq


; escape sequence handlers
; to do the reverse of tokenising during the listing
; command which is also used to save to disk.
df_rt_escjmp
	dw df_rt_lst_chr
	dw df_rt_lst_reserved
	dw df_rt_lst_reserved
	dw df_rt_lst_reserved
	dw df_rt_lst_reserved	
	dw df_rt_lst_reserved	; no such thing as bytdec
	dw df_rt_lst_bythex
	dw df_rt_lst_bytbin
	dw df_rt_lst_reserved	
	dw df_rt_lst_intdec
	dw df_rt_lst_inthex
	dw df_rt_lst_intbin
	dw df_rt_lst_reserved
	dw df_rt_lst_reserved
	dw df_rt_lst_reserved
	dw df_rt_lst_reserved	
	dw df_rt_lst_strlit
	dw df_rt_lst_var
	dw df_rt_lst_proc
