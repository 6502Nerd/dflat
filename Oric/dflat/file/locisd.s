
;**********************************************************
;*
;*	ORIC DFLAT
;*	Dolo Miah (@6502Nerd)
;*	Copyright (c) 2024
;*  Free to use for any non-commercial purpose subject to
;*  credit of original my authorship please!
;*
;*  LOCISD.S
;*	Routines to use LOCI SD card storage.
;*	The file format is similar to the dflat tape
;**********************************************************
	if DFLATLOCI
	; ROM code
	code

mod_sz_lc_s

; Using extended RAM to keep current path
; Page aligned
XMEM		=	0x4000
XMEMDIRPATH	=	(XMEM)

	include "file/loci.i"

loci_default_path
	db	":/dflat/",0
loci_dir_str
	db	"<DIR>",0

; a=high address, x=low address
; step = 1 default
; y=0 on return
lc_init_xmempath
	ldy #hi(XMEMDIRPATH)
	sty MIA_ADDR0+1
	ldy #1
	sty MIA_STEP0
	dey
	sty MIA_ADDR0
; Convenient place for labels with immediate rts
lc_block_gap
lc_put_delay
	rts

; Copy filedes to MIA A,X
lc_fd_to_mia_ax
	lda lc_fildes
	sta MIA_A
	lda lc_fildes+1
	sta MIA_X
	rts

; Starting directory for dflat in LOCI
lc_init_path
	jsr lc_init_xmempath
	ldx #'1'							; Default device=1
	clc
	jsr gr_get_key
	bcs lc_init_path_skip_dev
	tax									; Use this key as device number
lc_init_path_skip_dev
	stx MIA_RW0
lc_init_path_ch
	iny
	lda loci_default_path-1,y
	sta MIA_RW0
	bne lc_init_path_ch
	rts

; Close open file or directory
lc_release
	jsr lc_fd_to_mia_ax
	lda #MIA_OP_CLOSEDIR
	ldx tp_flag
	beq lc_release_dir
	lda #MIA_OP_CLOSE
lc_release_dir
	sta MIA_OP
	jmp lc_op_wait

; Read byte
; Only saves X (as Y not touched)
; A=byte read from file
lc_read_byte
	stx tmp_d					; Save X in tmp_d

	lda #0xd					; Assume CR (meaning directory)
	ldx tp_flag					; If tp_flag zero then this is a directory
	beq lc_read_byte_dir
	ldx #0						; Count hi = 0
	stx MIA_XSTACK
	inx							; Count lo = 1 (inefficient!)
	stx MIA_XSTACK
	jsr lc_fd_to_mia_ax
	lda #MIA_OP_READ_XSTACK		; Ok do a read of just 1 byte
	jsr lc_op_wait
	lda MIA_XSTACK				; Get the byte from the stack
lc_read_byte_dir				; If dir then skip all that

	ldx tmp_d					; Get X back
	rts

; Write byte to file
; only uses A,X (Y not touched)
lc_write_byte
	pha
	stx tmp_d

	sta MIA_XSTACK
	jsr lc_fd_to_mia_ax
	lda #MIA_OP_WRITE_XSTACK	; Ok do a write of just 1 byte
	jsr lc_op_wait

	pla
	ldx tmp_d
	rts

; A = MIA op code
; Waits until MIA ready
lc_op_wait
	sta MIA_OP
	jmp MIA_SPIN

; A = MIA op code
; Waits until MIA ready
; put A,X in filedes
lc_op_wait_fd
	jsr lc_op_wait
	sta lc_fildes				; Save A,X from MIA op call
	stx lc_fildes+1
	rts

lc_push_dir_path
	jsr lc_init_xmempath
lc_find_dir_path_end
	iny
	lda MIA_RW0
	bne lc_find_dir_path_end
	dey							; Go back to the zero
	sty MIA_ADDR0				; set addr low
	lda #255					; -ve step
	sta MIA_STEP0
	lda MIA_RW0					; Step back xmem ptr too
lc_dir_path_push_ch
	lda MIA_RW0
	sta MIA_XSTACK
	dey
	bne lc_dir_path_push_ch
lc_dir_path_done
	rts

; Push filename onto MIA XSTACK
lc_push_fname
	ldy #0
lc_init_findend
	iny
	lda df_linbuff-1,y
	bne lc_init_findend
lc_push_fnchar					; Push in reverse order
	lda df_linbuff-1,y
	sta MIA_XSTACK
	dey
	bne lc_push_fnchar
	jsr lc_push_dir_path		; Also push path in reverse
	rts

; Get a filehandle from MIA
; Could be a file or a directory
; If a directory can only be read mode tp_flag set to 0
lc_init
	jsr lc_push_fname
;	lda #0
	sty MIA_X					; Y is zero from lc_push_fname !!!
	lda tp_flag					; 1=Read, 2=Write
	cmp #2						; If write
	bne lc_init_skip_create
	ora #0x10					; Create file if not exists
lc_init_skip_create	
	sta MIA_A
	lda #MIA_OP_OPEN			; Try to open file first
	jsr lc_op_wait_fd
	cmp #255					; if error try to open dir
	bne lc_init_done			; else done
	lda tp_flag					; but only if reading
	cmp #1
	bne lc_init_error
	jsr lc_push_fname
	lda #MIA_OP_OPENDIR
	jsr lc_op_wait_fd
	cmp #255					; if error try to open dir
	beq lc_init_error
	jsr lc_init_xmempath		; dir is valid so add to path
	sty tp_flag					; Y already zero from xmem init
	lda df_linbuff				; if the dir '/' ?
	cmp #'/'
	bne lc_init_dir_end
	jmp lc_init_path			; Then back to dflat root
lc_init_dir_end
	iny
	lda MIA_RW0
	bne lc_init_dir_end
	ldx #0
	dey							; Overwrite the zero in xmem0
	sty MIA_ADDR0
lc_init_dir_append
	lda df_linbuff,x
	beq lc_init_dir_term
	sta MIA_RW0
	inx
	bne lc_init_dir_append
lc_init_dir_term				; Finally add '/' and zero terminator
	lda #'/'
	sta MIA_RW0
	lda #0
	sta MIA_RW0
lc_init_done
	lda tp_flag					; If opened for write then send 4 bytes of SYN
	cmp #2
	bne lc_init_rts
	lda #0x16					; SYN byte
	ldy #4
lc_init_syn
	jsr lc_write_byte
	dey
	bne lc_init_syn
lc_init_rts
	rts
lc_init_error
	SWBRK DFERR_FNAME

sd_delete
	jsr lc_push_fname
	lda #MIA_OP_UNLINK
	jsr lc_op_wait
	cmp #255
	beq lc_init_error
	rts

sd_dir
	lda #0								; No filename
	sta df_linbuff
	sta tp_flag							; Also no file mode
	jsr lc_push_fname					; Just push current dir
	ldy #0
	sta MIA_X							; Zero
	iny									; 1=read
	sty MIA_A
	lda #MIA_OP_OPENDIR
	jsr lc_op_wait_fd
	cmp #255							; Didn't open?
	beq lc_init_error					; Then error
sd_dir_read_entry
	jsr lc_fd_to_mia_ax
	lda #MIA_OP_READDIR					; Try to read entry
	jsr lc_op_wait
	ldy #0								; Pop entry off stack
sd_get_entry
	lda MIA_XSTACK						; Get a byte
	sta df_linbuff,y					; Save to memory
	iny
	cpy #dirent							; For size of directory entry
	bne sd_get_entry
	ldy #lo(df_linbuff+dirent_name)		; ptr to filename
	lda #hi(df_linbuff+dirent_name+1)
	jsr gr_print_line					; print the filename
	cpy #0								; Nothing was printed?
	beq sd_dir_end
sd_dir_padding
	jsr utilPrintSPC
	iny
	cpy #25
	bne sd_dir_padding
	ldx df_linbuff+dirent_size			; Low byte of size
	lda df_linbuff+dirent_size+1		; High byte of size
	clc
	jsr int_to_str
	lda df_linbuff+dirent_attrib		; Directory?
	and #0x20							; Attribute bit for file
	bne sd_dir_not_folder
	tax
sd_dir_stuff_ch
	inx
	lda loci_dir_str-1,x
	sta num_buf-1,x
	bne sd_dir_stuff_ch
sd_dir_not_folder
	ldy #lo(num_buf)
	lda #hi(num_buf)
	jsr gr_print_line
	jsr utilPrintCRLF
	jmp sd_dir_read_entry
sd_dir_end
	jmp lc_release

mod_sz_lc_e

	endif