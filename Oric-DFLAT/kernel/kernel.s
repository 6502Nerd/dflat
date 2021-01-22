;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  KERNEL.S
;*	Lol, I thought this would become more sophisticated
;*	hence the name 'kernel'. But no, it just initialises
;*	memory, VIA, sound and screen, before passing control
;*	to main.
;*	References to banks etc. are from ported code which
;*	did do more stuff, namely helping with ROM banking.
;*
;**********************************************************

;* Include all definition and code files in the right order
	include "inc/includes.i"
	include "inc/graph.i"
	include "io/io.i"
	include "dflat/dflat.i"
	include "kernel/zeropage.i"
	include "dflat/dflat.i"
	include "dflat/error.i"


;****************************************
;*	Set 6502 default vectors	*
;****************************************
	data				; Set vectors
	org 0xfffa			; Vectors are at these addresses
	fcw nmi				; 0xfffa : NMI Vector
	fcw init			; 0xfffc : Reset Vector
	fcw call_irq_master	; 0xfffe : IRQ Vector

	; ROM code
	code				;
	org 0xc000			; Start of ROM

_code_start
	; Restore current bank always at address c001
mod_sz_kernel_s

;* Include all core code in the right order
	include "kernel/snd-low.s"
	include "kernel/main.s"
	include "kernel/irq.s"
	include "utils/utils.s"
	include "io/io.s"

;* Reset vector points here - 6502 starts here
init
;	jmp init_test
	; First clear ram
	sei					; Need this for MOS 6502
	cld					; Need this for MOS 6502
	jmp init_ram		; jmp not jsr to ram initialiser
init_2					; init_ram will jump back to here
	ldx #0xff			; Initialise stack pointer
	txs

	jsr kernel_init

	jmp main

kernel_init
	jsr init_irq		; Initialise IRQ handling

	jsr init_via0		; initialise cia 0 - tape inactive
	jsr tp_init			; Initialise tape handling

kernel_test
	jsr init_snd		; initialise the sound chip

	jsr gr_init			; Initialise graphics, default is text mode

	jsr init_keyboard	; initialise keyboard timer settings
	jsr io_init			; Set default input/output device

	lda #0
	sta vdp_cnt

	jsr df_init			; Initialise interpreter

	cli					; irq interrupts enable

	rts


;* Initialises RAM, skipping page 3 which is for IO
;* Zeroes all addressable RAM in the default bank i.e. up to 0xc000
init_ram
	ldy #0x02			; But Y initially at 2 to not overwrite pointer
	ldx #0x00			; Page counter starts at zero
	stx 0x00			; Start at page 0
	stx 0x01
init_ram_1
	cpx	#3				; Ignore page 3 (IO page)
	beq init_ram_skip
init_ram_fill
	lda #0				; Normal RAM filled with zero
	sta (0x00),y		; Write byte to RAM (zero or copy of ROM)
init_ram_skip
	iny
	bne init_ram_1		; Do a whole page
	inc 0x01			; Increase page pointer
	inx					; Reduce page count
	cpx #0xc0			; Do all pages until page until we get to page C0
	bne init_ram_1

	jmp init_2			; Carry on initialisation

; 6502 Non-maskable interrupt come here
nmi
	rti

mod_sz_kernel_e

