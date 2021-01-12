;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  ERROR.I
;*  Error definitions file.
;*  The macro to throw an error is elswhere, but basically
;*  It issues a 6502 BRK commmand with the next byte being
;*  the error code.  The BRK handler then picks up the
;*  code and shows the appropriate message plus any line
;*  number if a program was running.
;*
;**********************************************************

	; ROM code
	code  

; Error message numbers
DFERR_OK		=	0
DFERR_SYNTAX	=	1
DFERR_TYPEMISM	=	2
DFERR_DIM		=	3
DFERR_UNTIL		=	4
DFERR_NOPROC	=	5
DFERR_PROCPARM	=	6
DFERR_IMMEDIATE	=	7
DFERR_UNCLOSEDIF=	8
DFERR_NOIF		=	9
DFERR_NEXTFOR	=	10
DFERR_FNAME		=	11
DFERR_STRLONG	=	12
DFERR_BREAK		=	13
DFERR_NODATA	=	14
DFERR_WEND		=	15
DFERR_NOLINE	=	16
DFERR_RETURN	=	17
DFERR_ABORT		=	18
DFERR_QUANTITY	=	19
DFERR_NOORG		=	20


