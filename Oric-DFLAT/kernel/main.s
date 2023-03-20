;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  MAIN.S
;*  This is where the main user program is executed by
;*  the 'kernel' once the system is initialised and ready.
;*  Today, main does very little - first shows the system
;*  boot up message, and then passes control to dflat.
;*
;**********************************************************

	; ROM code
	code

main
	ldx #lo(msg_hello_world)
	lda #hi(msg_hello_world)
	jsr io_print_line

infinity

	jmp df_pg_dflat

msg_hello_world
	;* build.s is generate by the assemble.bat file
	;* all it does is echo an assembler line to
	;* including the build date in the message.
	include "kernel/build.s"
