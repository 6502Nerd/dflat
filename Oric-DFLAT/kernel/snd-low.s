;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  SND-LOW.S
;*	Routines to access the AY-3-8912, which is done through
;*	lines of the VIA - CB2 and CA2 for chip select, and
;*	Port A for data interface. This makes accessing the
;*	8912 a bit slow..
;*
;* 	CB2		CA2		Function		CB2=BDIR, CA2=BC1
;*	0		0		Not selected
;*	0		1		Read register in to Port A
;*	1		0		Write register from Port A
;*	1		1		Select register # from Port A
;**********************************************************

;****************************************
;* snd_sel_reg
;* Select AY register from A
;* Input : A = Value
;* Output : None
;* Regs affected : None
;****************************************
snd_sel_reg
	pha
	sta SND_ADBUS			; Put reg # on Port A (sound bus)

	lda #SND_SELSETADDR		; Get ready to select the reg
	sta SND_MODE			; Latch the reg # on Port A

	lda #SND_DESELECT		; Deselect AY
	sta SND_MODE

	pla
	rts

;****************************************
;* snd_set_reg
;* Set previosuly selected AY register
;* Input : A = Value to set
;* Output : None
;* Regs affected : None
;****************************************
snd_set_reg
	pha

	sta SND_ADBUS			; Put reg value on Port A (sound bus)
	lda #SND_SELWRITE		; Select mode for writing data
	sta SND_MODE			; Latch reg value on Port A	
	lda #SND_DESELECT		; Deselect AY
	sta SND_MODE

	pla
	rts
	
;****************************************
;* snd_set
;* Set reg X to value A
;* Input : X=Reg, A = Value to set
;* Output : None
;* Regs affected : None
;****************************************
snd_set
	pha
	txa
	jsr snd_sel_reg
	pla
	jmp snd_set_reg


;****************************************
;* snd_get
;* Get AY register X value
;* Input : X = Reg no
;* Output : A = Value
;* Regs affected : None
;****************************************
;snd_get
;
;	lda #0xff				; Set Port A to output
;	sta IO_0+DDRA
;
;	stx SND_ADBUS			; Put X on the sound bus (X = reg address)
;
;	lda #SND_SELSETADDR		; Get ready to select the reg
;	sta SND_MODE			; Latch the reg # on Port A
;
;	lda #SND_DESELECT		; Deselect AY
;	sta SND_MODE
;
;	lda #0x00				; Set Port A to input
;	sta IO_0+DDRA
;
;	lda #SND_SELREAD		; Select mode for reading data
;	sta SND_MODE			; Set read mode on AY
;
;	lda SND_ADBUS			; Get value in to Y from Port A
;	pha						; Save it to stack
;	
;	lda #SND_DESELECT		; Deselect AY
;	sta SND_MODE
;
;	lda #0xff				; Set Port A back to output
;	sta IO_0+DDRA
;
;	pla						; Get the value off stack
;	
;	rts

