;* Addressing modes - 15 in total, 16 with directives
;*  0   Nothing
;*	1 	Absolute			XXX <word>
;*	2 	Absolute,X			XXX <word>,x
;*	3 	Absolute,Y			XXX <word>,y
;*	4	Zeropage			XXX	<byte>
;*	5	Zeropage,X			XXX	<byte>,x
;*	6	Zeropage,Y			XXX	<byte>,y
;*	7	Indirect			XXX	(<byte>)
;*	8	Indirect,X			XXX	(<byte>,x)
;*	9	Indirect,Y			XXX	(<byte>),y
;*	A	Absolute indirect	XXX	(<word>)
;*	B	Absolute indirect,x	XXX	(<word>,x)
;*	C	Immediate			XXX #<byte>
;*	D	Accumulator			XXX (same as implied)
;*	E	Implied				XXX
;*	F	Relative			XXX	<byte>
;*	10	Assembler directive	XXX .......

AM_NONE		= 0
AM_ABS	 	= 1
AM_ABSX		= 2
AM_ABSY		= 3
AM_ZP		= 4
AM_ZPX		= 5
AM_ZPY		= 6
AM_ZPIND	= 7
AM_ZPINDX	= 8
AM_ZPINDY	= 9
AM_ABSIND	= 10
AM_ABSINDX	= 11
AM_IMM		= 12
AM_ACC		= 13
AM_IMP		= AM_ACC
AM_REL		= 15
AM_DIR		= 16


