;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  ASMSYMTAB.S
;*	Mainly this contains the symbol table for assembler
;*	mnemonics but also meta data like what addressing modes
;*	and the length of opcodes.
;*
;**********************************************************

	; ROM code
	code  

	;* Length of each addressing mode
	;* ORDER significant!
df_asm_length
	db	0,3,3,3,2,2,2,2,2,2,3,3,2,1,1,2,0

	; Always try for lowest addressing mode, but
	; this table maps to alternative
df_asm_altaddrmode
	db AM_NONE		;AM_NONE	= 0
	db AM_NONE		;AM_ABS	 	= 1
	db AM_NONE		;AM_ABSX	= 2
	db AM_NONE		;AM_ABSY	= 3
	db AM_ABS		;AM_ZP		= 4
	db AM_ABSX		;AM_ZPX		= 5
	db AM_ABSY		;AM_ZPY		= 6
	db AM_ABSIND	;AM_ZPIND	= 7
	db AM_ABSINDX	;AM_ZPINDX	= 8
	db AM_ZPINDY	;AM_ZPINDY	= 9
	db AM_NONE		;AM_ABSIND	= 10
	db AM_NONE		;AM_ABSINDX	= 11
	db AM_NONE		;AM_IMM		= 12
	db AM_NONE		;AM_ACC		= 13
	db AM_NONE		;AM_IMP		= AM_ACC
	db AM_REL		;AM_REL		= 15
	db AM_NONE		;AM_DIR		= 16


	;*	Mnemonic/	How many addressing modes /
	;* 	Mode		Opcode for mode
df_asm_tokensyms
	;* Start with directives
	db	"org",		1*2
	db	AM_DIR,		0x00
	db	"opt",		1*2
	db	AM_DIR,		0x00
	db	"db",		1*2
	db	AM_DIR,		0x00
	db	"dw",		1*2
	db	AM_DIR,		0x00
	db	"ds",		1*2
	db	AM_DIR,		0x00	

	db	"adc",		8*2
	db	AM_IMM,		0x69
	db	AM_ZP,		0x65
	db	AM_ZPX,		0x75
	db	AM_ABS,		0x6d
	db	AM_ABSX,	0x7d
	db	AM_ABSY,	0x79
	db	AM_ZPINDX,	0x61
	db	AM_ZPINDY,	0x71
	
	db	"and",		8*2
	db	AM_IMM,		0x29
	db	AM_ZP,		0x25
	db	AM_ZPX,		0x35
	db	AM_ABS,		0x2d
	db	AM_ABSX,	0x3d
	db	AM_ABSY,	0x39
	db	AM_ZPINDX,	0x21
	db	AM_ZPINDY,	0x31

	db	"cmp",		8*2
	db	AM_IMM,		0xc9
	db	AM_ZP,		0xc5
	db	AM_ZPX,		0xd5
	db	AM_ABS,		0xcd
	db	AM_ABSX,	0xdd
	db	AM_ABSY,	0xd9
	db	AM_ZPINDX,	0xc1
	db	AM_ZPINDY,	0xd1

	db	"eor",		8*2
	db	AM_IMM,		0x49
	db	AM_ZP,		0x45
	db	AM_ZPX,		0x55
	db	AM_ABS,		0x4d
	db	AM_ABSX,	0x5d
	db	AM_ABSY,	0x59
	db	AM_ZPINDX,	0x41
	db	AM_ZPINDY,	0x51

	db	"lda",		8*2
	db	AM_IMM,		0xa9
	db	AM_ZP,		0xa5
	db	AM_ZPX,		0xb5
	db	AM_ABS,		0xad
	db	AM_ABSX,	0xbd
	db	AM_ABSY,	0xb9
	db	AM_ZPINDX,	0xa1
	db	AM_ZPINDY,	0xb1

	db	"ora",		8*2
	db	AM_IMM,		0x09
	db	AM_ZP,		0x05
	db	AM_ZPX,		0x15
	db	AM_ABS,		0x0d
	db	AM_ABSX,	0x1d
	db	AM_ABSY,	0x19
	db	AM_ZPINDX,	0x01
	db	AM_ZPINDY,	0x11

	db	"sbc",		8*2
	db	AM_IMM,		0xe9
	db	AM_ZP,		0xe5
	db	AM_ZPX,		0xf5
	db	AM_ABS,		0xed
	db	AM_ABSX,	0xfd
	db	AM_ABSY,	0xf9
	db	AM_ZPINDX,	0xe1
	db	AM_ZPINDY,	0xf1

	db	"sta",		8*2
	db	AM_ZP,		0x85
	db	AM_ZPX,		0x85
	db	AM_ABS,		0x8d
	db	AM_ABSX,	0x9d
	db	AM_ABSY,	0x99
	db	AM_ZPINDX,	0x81
	db	AM_ZPINDY,	0x91
	db	AM_ZPIND,	0x92

	db	"asl",		5*2
	db	AM_IMP,		0x0a
	db	AM_ZP,		0x06
	db	AM_ZPX,		0x16
	db	AM_ABS,		0x0e
	db	AM_ABSX,	0x1e
	db	"dec",		4*2
	db	AM_ZP,		0xc6
	db	AM_ZPX,		0xd6
	db	AM_ABS,		0xce
	db	AM_ABSX,	0xde
	db	"inc",		4*2
	db	AM_ZP,		0xe6
	db	AM_ZPX,		0xf6
	db	AM_ABS,		0xee
	db	AM_ABSX,	0xfe
	db	"lsr",		5*2
	db	AM_IMP,		0x4a
	db	AM_ZP,		0x46
	db	AM_ZPX,		0x56
	db	AM_ABS,		0x4e
	db	AM_ABSX,	0x5e
	db	"rol",		5*2
	db	AM_IMP,		0x2a
	db	AM_ZP,		0x26
	db	AM_ZPX,		0x36
	db	AM_ABS,		0x2e
	db	AM_ABSX,	0x3e
	db	"ror",		5*2
	db	AM_IMP,		0x6a
	db	AM_ZP,		0x66
	db	AM_ZPX,		0x76
	db	AM_ABS,		0x6e
	db	AM_ABSX,	0x7e

	db	"bit",		2*2
	db	AM_ZP,		0x24
	db	AM_ABS,		0x2c
	
	db	"brk",		1*2
	db	AM_IMP,		0x00
	
	db	"clc",		1*2
	db	AM_IMP,		0x18
	db	"cld",		1*2
	db	AM_IMP,		0xd8
	db	"cli",		1*2
	db	AM_IMP,		0x58
	db	"clv",		1*2
	db	AM_IMP,		0xb8
	db	"sec",		1*2
	db	AM_IMP,		0x38
	db	"sed",		1*2
	db	AM_IMP,		0xf8
	db	"sei",		1*2
	db	AM_IMP,		0x78
	
	db	"cpx",		3*2
	db	AM_IMM,		0xe0
	db	AM_ZP,		0xe4
	db	AM_ABS,		0xec
	db	"cpy",		3*2
	db	AM_IMM,		0xc0
	db	AM_ZP,		0xc4
	db	AM_ABS,		0xcc
	db	"dex",		1*2
	db	AM_IMP,		0xca
	db	"dey",		1*2
	db	AM_IMP,		0x88
	db	"inx",		1*2
	db	AM_IMP,		0xe8
	db	"iny",		1*2
	db	AM_IMP,		0xc8
	db	"ldx",		5*2
	db	AM_IMM,		0xa2
	db	AM_ZP,		0xa6
	db	AM_ZPY,		0xb6
	db	AM_ABS,		0xae
	db	AM_ABSY,	0xbe
	db	"ldy",		5*2
	db	AM_IMM,		0xa0
	db	AM_ZP,		0xa4
	db	AM_ZPX,		0xb4
	db	AM_ABS,		0xac
	db	AM_ABSX,	0xbc
	db	"stx",		3*2
	db	AM_ZP,		0x86
	db	AM_ZPY,		0x96
	db	AM_ABS,		0x8e
	db	"sty",		3*2
	db	AM_ZP,		0x84
	db	AM_ZPX,		0x94
	db	AM_ABS,		0x8c

	db	"bcc",		1*2
	db	AM_REL,		0x90
	db	"bcs",		1*2
	db	AM_REL,		0xb0
	db	"beq",		1*2
	db	AM_REL,		0xf0
	db	"bmi",		1*2
	db	AM_REL,		0x30
	db	"bne",		1*2
	db	AM_REL,		0xd0
	db	"bpl",		1*2
	db	AM_REL,		0x10
	db	"bvc",		1*2
	db	AM_REL,		0x50
	db	"bvs",		1*2
	db	AM_REL,		0x70
	db	"jmp",		2*2
	db	AM_ABS,		0x4c
	db	AM_ABSIND,	0x6c
	db	"jsr",		1*2
	db	AM_ABS,		0x20
	
	db	"nop",		1*2
	db	AM_IMP,		0xea
	db	"pha",		1*2
	db	AM_IMP,		0x48
	db	"php",		1*2
	db	AM_IMP,		0x08
	db	"pla",		1*2
	db	AM_IMP,		0x68
	db	"plp",		1*2
	db	AM_IMP,		0x28
	db	"rti",		1*2
	db	AM_IMP,		0x40
	db	"rts",		1*2
	db	AM_IMP,		0x60
	db	"tax",		1*2
	db	AM_IMP,		0xaa
	db	"tay",		1*2
	db	AM_IMP,		0xa8
	db	"tsx",		1*2
	db	AM_IMP,		0xba
	db	"txa",		1*2
	db	AM_IMP,		0x8a
	db	"txs",		1*2
	db	AM_IMP,		0x9a
	db	"tya",		1*2
	db	AM_IMP,		0x98	
	
	;* Terminates with a zero
	db	0
