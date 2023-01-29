;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  GRAPH.I
;*  This is the definition file for graphics, specifically
;*  The graphics screen handling module.  It is just a
;*  structure definition - but this structure is used to
;*  record the important attributes of a text screen.
;*	This was needed in the previous code due to 32 and 40
;*	byte width screens, but Oric only  has 40, so probably
;*	could be optimised a little.
;*
;**********************************************************

; Important screen addresses for the Oric ULA
TEXTSCRN	=	0xbb80
TEXTCHAR	=	0xb400
HISCRN		=	0xa000
HICHAR		=	0x9800
HITEXT		=	0xbf68

	struct gr_screen
	db gr_mode					; Text or Hires mode
	dw gr_hi_start				; Address fo hires screen
	dw gr_char					; Start address of charset
	dw gr_text_start			; Start of text memory
	db gr_geom_fill1			; (Not used, maintained for alignment)
	db gr_text_w				; Number of columns
	db gr_text_h				; Number of rows
	db gr_cur_x					; Current X position of cursor
	db gr_cur_y					; Current Y position of cursor
	dw gr_cur_ptr				; VDP address of cursor
	db gr_pixmode				; Pixel plot mode (0=Erase, 1=Plot, -1=XOR)
	db gr_pitch					; Pixel pitch for char plotting
	db gr_hires_x				; X pos of hires cursor
	db gr_hires_y				; Y pos of hires cursor
	db gr_ink					; Ink colour
	db gr_paper					; Paper colour
	db gr_margin				; Left margin
	dw gr_geom_tmp				; One word of temp storage for local use
	db gr_geom_tmp2				; One byte of temp storage for local use
	end struct
	
; Sprite data stored in free 256 in font space
spr_curX	= TEXTCHAR			; Sprite current X pos
spr_newX	= TEXTCHAR+32		; Sprite new X pos
spr_curY	= TEXTCHAR+64		; Sprite current Y pos
spr_newY	= TEXTCHAR+96		; Sprite new Y pos
spr_chr		= TEXTCHAR+128		; Sprite character
spr_bgnd	= TEXTCHAR+160		; Background character under sprite
spr_baseadrl= TEXTCHAR+192		; Y low address of sprite screen (with X=0)
spr_baseadrh= TEXTCHAR+224		; Y high address of sprite screen (with X=0)

; Tables stored in alternate character set area
; only used for high-resolution screen handling
hires_row_low	=	HICHAR+0x400		; Low byte of row pointer
hires_row_hi	=	hires_row_low+200	; High byte of row pointer
hires_col		=	hires_row_hi+200	; Pixel to byte column to mapping
hires_mask		=	hires_col+240		; Pixel column to pixel mask mapping

