;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  CIA.S
;*  Code to initialise and utilise the 6522, which is used
;*	for various functions on the Oric including;
;*		Port A:
;*			[7..0] 	Printer data bits
;*			[7..0] 	Interface to AY-3-8912
;*		Port B:
;*			[2..0] 	Keyboard matrix row selector
;*			[7]	   	Tape output
;*			[6]    	Tape player motor signal
;*			[4]		Printer strobe
;*		CA1:		Printer ACK
;*		CB1: 		Tape input
;*		CA2,CB2: 	Selecting the AY-3-8912
;*
;*  This file is called cia.s because code was originally
;*  for a MOS 6526 from a CMB64, but didn't get around to
;*	renaming this file ;-)
;*
;**********************************************************


	; ROM code
	code

mod_sz_cia_s


;****************************************
;* init_via0
;* Initialise cia 0
;* Input : None
;* Output : None
;* Regs affected : A
;****************************************
init_via0_tape					; initialisation bytes for tape
	ldy #init_tape_tab-init_via0_tab
	db 0x2c						; Ignore next two bytes
init_via0
	ldy #0						; Standard initialisation of VIA
init_via0_loop
	lda init_via0_tab,y
	bmi init_via0_done
	tax
	iny
	lda init_via0_tab,y
	sta IO_0,x
	iny
	bne init_via0_loop

init_via0_done
	rts							; return from sub

init_via0_tab
	db IER, 	0x7f
	db DDRA,	0xff			; Port A output by default
	db DDRB,	0xf7			; Tape motor + KB select
	db PRB,		KB_PRB+7		; Port B default (cassette motor off)
	db PCR,		0xdd			; Ensure AY is not selected (CB1 active)
	db T1CL,	lo(TIMER1_RATE)	; 50Hz
	db T1LL,	lo(TIMER1_RATE)	; 50Hz
	db T1CH,	hi(TIMER1_RATE)	; 50Hz
	db T1LH,	hi(TIMER1_RATE)	; 50Hz
	db ACR,		0x40			; Timer 1 continuous
	db IER,		0xc0			; Timer 1 interrupt enabled
	db -1
init_tape_tab
	db IER,		0x7f			; Disable all interrupts
	db T2CL,	0xf4			; Timer 2 used for measuring CB1 time
	db PCR,		0x10			; Interrupt on CB1 positive edge
;	db DDRB,	0xff			; Set port B output
	db ACR,		0xc0			; T1 continuous and toggle PB7
	db T1CL,	lo(TAPE_RATE*2)	; Tape rate /2 = 0
	db T1CH,	hi(TAPE_RATE*2)	; Tape rate /2 = 0
	db PRB,		KB_PRB+0x40		; Tape motor ON
	db -1
;init_ser_tab
;	db IER,		0x7f			; Disable all interrupts
;	db T2CL,	0xf4			; Timer 2 used for measuring bit time
;	db PCR,		0xdd			; Ensure AY is not selected (CB1 active)
;	db DDRB,	0xff			; Set port B output
;	db -1

mod_sz_cia_e
