;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  INCLUDES.S
;*  Main include file for key definitions and macros.
;*  Many of the settings here are exremely machine dependent.
;*  Defines : IO block addresses, VIA port usage, SD card
;*  settings, useful macros, sound chip registers, VIA, VDP
;*  and ACIA registers.
;*	Of course the Oric doesn't have a VDP or an ACIA by default
;*	so this file could be optimised.
;*
;**********************************************************

;* The IO block is at 0x04000 and decodes up to
;* eight IO addresses, at 0x0080 intervals
;* All eight are not used at the present time:
;* - 0 : VIA 1 (Keyboard)
;* - 1 : VIA 2 (Sound and SD card interface)
;* - 2 : VDP (Video)
;* - 3 : ACIA (Serial)
IO_0		= 0x0300

VDP_FLASH	= 16				;* 32/50 second flash

TIMER1_RATE	= 1000000 / 50		;* 50 times per second interrupt
TAPE_RATE	= 0xd0				;* Assumed required FM frequency

;* Standard definitions of 6522 registers
;* As found in the datasheets
PRB			= 0x00
PRA			= 0x01
DDRB		= 0x02
DDRA		= 0x03
T1CL		= 0x04
T1CH		= 0x05
T1LL		= 0x06
T1LH		= 0x07
T2CL		= 0x08
T2CH		= 0x09
SR			= 0x0a
ACR			= 0x0b
PCR			= 0x0c
IFR			= 0x0d
IER			= 0x0e
PRAH		= 0x0f

IFR_CA2		= 0x01
IFR_CA1		= 0x02
IFR_CB1		= 0x10

;* AY-3-8910 definitions
;* The sound chip is accessed through VIA 2
SND_ADBUS	= IO_0+PRAH
SND_MODE	= IO_0+PCR

; Values for the PCR register - always enable CB1 active edge (bit 4)
SND_SELREAD			= 0b11011111
SND_SELWRITE		= 0b11111101
SND_SELSETADDR		= 0b11111111
SND_DESELECT		= 0b11011101

SND_REG_CHAPL	= 0x00
SND_REG_CHAPH	= 0x01
SND_REG_CHBPL	= 0x02
SND_REG_CHBPH	= 0x03
SND_REG_CHCPL	= 0x04
SND_REG_CHCPH	= 0x05
SND_REG_CHNP	= 0x06
SND_REG_CTL		= 0x07
SND_REG_CHAVOL	= 0x08
SND_REG_CHBVOL	= 0x09
SND_REG_CHBVOL	= 0x0a
SND_REG_ENVPL	= 0x0b
SND_REG_ENVPH	= 0x0c
SND_REG_ENVCYC	= 0x0d

SND_REG_IOA	= 0x0e
SND_REG_IOB	= 0x0f


;* Port B
KB_SENSE	= 0x08			; Input - Bit 3 port A
KB_CAPSLK	= 0x01			; Id of Caps Lock - maps to Led 1

KB_REP_TIM	= 3 			; Number of VB periods for the repeat speed
KB_REP_DEL	= 20			; Number of VB periods before repeat activates
KB_DEBOUNCE	= 2				; Number of VB periods before debounce

UTF_ETX		= 0x03			; Break character
UTF_BEL		= 0x07
CRSR_LEFT	= 0x08
CRSR_RIGHT	= 0x09
CRSR_DOWN	= 0x0a
CRSR_UP		= 0x0b
CTRL_CAPS	= 0x14			; CTRL-T to toggle caps
UTF_ACK		= 0x01			; Used for the CTRL-A copy in this implementation
UTF_FF		= 0x0c
UTF_CR		= 0x0d
UTF_BRK		= 0x1a			; Debug - drop in to monitor
UTF_DEL		= 0x7f
UTF_SPECIAL = 0x20

CMD_ERR_NOERROR			= 0x00
CMD_ERR_NOTFOUND		= 0x01
CMD_ERR_PARM			= 0x02
CMD_ERR_VAL				= 0x03

;* Number formats for conversion routines
NUM_ANY		= 0x00
NUM_DEC		= 0x01
NUM_HEX		= 0x02
NUM_BIN		= 0x03

	

;* USEFUL MACROS HERE

;* Software break to throw errors
;* use like this : SWBRK XX
;* Where XX is the error code
SWBRK macro sig
	brk
	db sig
	endm

_pushAXY macro
	pha
	sta tmp_d
	txa
	pha
	tya
	pha
	lda tmp_d
	endm

_pullAXY macro
	pla
	tay
	pla
	tax
	pla
	endm

_println macro msg
	_pushAXY
	ldx #lo(msg)
	lda #hi(msg)
	jsr io_print_line
	_pullAXY
	endm

_println_low macro msg
	ldx #lo(msg)
	lda #hi(msg)
	jsr io_print_line
	endm

_printmsgA macro msg
	_pushAXY
	ldx #lo(msg)
	lda #hi(msg)
	jsr io_print_line
	pla
	pha
	jsr str_a_to_x
	jsr _put_byte
	txa
	jsr _put_byte
	lda #UTF_CR
	jsr _put_byte
	_pullAXY
	endm

_printA macro
	_pushAXY
	jsr str_a_to_x
	jsr io_put_ch
	txa
	jsr io_put_ch
	_pullAXY
	endm

_printCRLF macro
	pha
	lda #UTF_CR
	jsr _put_byte
	pla
	endm

_printC macro ch
	pha
	lda #ch
	jsr io_put_ch
	pla
	endm

_printCA macro
	pha
	jsr _put_byte
	pla
	endm

_sendcmd macro cmd
	_pushAXY
	ldx #lo(cmd)
	lda #hi(cmd)
	jsr sd_sendcmd
	_pullAXY
	endm

_incZPWord macro wordp
	inc wordp
	db	0xd0, 0x02
	inc wordp+1
	endm

_decZPWord macro wordp
	pha
	sec
	lda wordp
	sbc #1
	sta wordp
	lda wordp+1
	sbc #0
	sta wordp+1
	pla
	endm

_cpyZPWord macro worda,wordb
	lda worda
	sta wordb
	lda worda+1
	sta wordb+1
	endm
	
_addZPWord macro worda, wordb
	clc
	lda worda
	adc wordb
	sta worda
	lda worda+1
	adc wordb+1
	sta worda+1
	endm

_subZPWord macro worda, wordb
	sec
	lda worda
	sbc wordb
	sta worda
	lda worda+1
	sbc wordb+1
	sta worda+1
	endm
	
_adcZPWord macro worda,const
	clc
	lda worda
	adc #const
	sta worda
	lda worda+1
	adc #0
	sta worda+1
	endm
	
_debug macro ch
	pha
	lda #ch
	sta 48000
	pla
	endm
	

