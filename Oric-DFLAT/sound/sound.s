;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2020
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  SOUND.S
;*  Sound driver module - routines to access the AY-3-8912
;*  This sound chip was found in a number of popular micros
;*  in the early to mid 80s, including my first computer,
;*  the Oric-1, as well as the MSX range.
;*
;**********************************************************


	; ROM code
	code

mod_sz_sound_s


;****************************************
;* snd_get_note
;* Get a note from the music scale table
;* Input : Octave in X, note in Y
;*         Octave between 1 and 6
;* Output : A,X = Value hi,lo
;* Regs affected : X
;****************************************
snd_get_note
	tya
	asl a
	tay
	; Get note for octave 1
	lda snd_music_tab,y
	sta tmp_alo
	lda snd_music_tab+1,y
	
snd_get_note_oct
	dex
	beq snd_note_done
	; Divide freq by 2 each octave
	lsr a
	ror tmp_alo
	jmp snd_get_note_oct
snd_note_done
	ldx tmp_alo
	rts

	
;****************************************
;* init_snd
;* Initialise sound - after cia 1 has been initialised
;* Input : None
;* Output : None
;* Regs affected : All
;****************************************
init_snd
	ldx #15
init_snd_regs
	lda snd_init_tab,x
	jsr snd_set				; Set X to A
	dex
	bpl init_snd_regs		; 16 regs
	
	rts						; return from sub

	; Register array initialisation values
	; Assuming 1.34Mhz input clock
snd_init_tab
	db 0x40				; R0 = Channel A Tone Low
	db 0x00				; R1 = Channel A Tone High
	db 0x00				; R2 = Channel B Tone Low
	db 0x01				; R3 = Channel B Tone High
	db 0x00				; R4 = Channel C Tone Low
	db 0x02				; R5 = Channel C Tone High
	db 0x00				; R6 = Noise period
	db 0b01111110		; R7 = Control : IOB input, IOA output, No Noise, only A enabled
	db 0x1f				; R8 = Channel A Vol
	db 0x1f				; R9 = Channel B Vol
	db 0x1f				; R10 = Channel C Vol
	db 0x00				; R11 = Envelope Period Low
	db 0x03				; R12 = Envelope Period High
	db 0b00000000		; R13 = Envelope Shape : 0000
	db 0xff				; R14 = IO Port A - KB lines disabled
	db 0x00				; R15 = IO Port B ; Initialise to 0 (doesn't exist)

snd_music_tab
	dw 3058				; C		0 (Octave 1 3058Hz)
	dw 2886				; C#	1
	dw 2724				; D		2
	dw 2571				; D#	3
	dw 2427				; E		4
	dw 2291				; F		5
	dw 2162				; F#	6
	dw 2041				; G		7
	dw 1926				; G#	8
	dw 1818				; A		9
	dw 1716				; A#	10
	dw 1620				; B		11
	dw 0				; Null  12

mod_sz_sound_e
