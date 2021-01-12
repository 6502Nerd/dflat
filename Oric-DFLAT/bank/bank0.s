	org 0xc000
mod_sz_bios_s
	include "kernel\kernel.s"
	
; Bank specific code goes here
	include "cia\cia.s"
	include "tape\tape.s"
	include "keyboard\keyboard.s"
	include "vdp\graph.s"
	include "monitor\cmd.s"
	include "sound\sound.s"
mod_sz_bios_e
mod_sz_language_s
	include "utils\intmath.s"
	include "dflat\dflat.s"
	include "dflat\error.s"
	include	"dflat\asm.s"
mod_sz_language_e
	; End of Code
_code_end
