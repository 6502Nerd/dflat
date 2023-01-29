;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2021
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  OSROMVEC.I
;*	This include file sets up the OS ROM vectors which can
;*	be invoked by user programs to access ROM functions and
;*	will be maintained across versions of dflat to support
;*	backward and forward compatibility.
;*	Make sure these vectors are set up from 0xc000 as this
;*	what user programs will rely on!
;*
;**********************************************************
	
	; Common OS ROM calls - user programs should JMP or JSR
	; through these routines where possible to maintain
	; compatibility with ROM changes as these calls will
	; always be in the same position
_rom_vec_00	jmp	gr_init_screen_txt			; Go in to text mode
_rom_vec_01	jmp gr_cls						; Clear text screen
_rom_vec_02	jmp	gr_set_cur					; Set text cursor position
_rom_vec_03	jmp	gr_init_hires				; Go in to hires mode
_rom_vec_04	jmp (io_block+io_get_byte)		; Get byte
_rom_vec_05	jmp (io_block+io_put_byte)		; Put byte
_rom_vec_06 jmp io_read_line				; Read a line
_rom_vec_07	jmp io_print_line				; Print a line
_rom_vec_08	jmp snd_set						; Set sound register
_rom_vec_09	jmp kb_stick					; Get joystick status
_rom_vec_0a	jmp gr_plot						; Plot a lores character
_rom_vec_0b	jmp	gr_hchar					; Plot a hires character
_rom_vec_0c	jmp gr_point_setup				; Get address, offset, mask for hires coords
_rom_vec_0d	jmp gr_point					; Plot a hires pixel
_rom_vec_0e	jmp df_rt_sprupd				; Refresh sprites in lores mode
_rom_vec_0f	jmp	gr_getXY_base				; Get row address of text screen
_rom_vec_10	jmp gr_pixel					; Get a pixel value from hires screen
_rom_vec_11 jmp gr_get						; Get screen at coords
_rom_vec_12 jmp io_active_device			; Set the active device
_rom_vec_13	jmp (io_block+io_open_r)		; Open file for read
_rom_vec_14	jmp (io_block+io_open_w)		; Open file for write
_rom_vec_15	jmp (io_block+io_close_f)		; Close file
_rom_vec_16	jmp (io_block+io_del_f)			; Delete file
_rom_vec_17	jmp (io_block+io_ext1)			; Extended op1 (open file for binary read)
_rom_vec_18	jmp (io_block+io_ext2)			; Extended op2 (open file for binary write)
_rom_vec_19 jmp snd_get_note				; Get for an octave,note (X,Y) get pitch lo,hi (X,A)
