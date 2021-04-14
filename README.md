# dflat
dflat is a BASIC-like language for 8-bit micros and retro computers running on 6502 and 65c02.  Key features highlights;
* Procedure orientated structure (all runnable code within def..enddef blocks)
* Local variables - supports recursion
* Structured programming including if..elif..endif, while..wend, repeat..until
* Line numbers only used for sequencing lines, but cannot be referenced - no goto or gosub!
* Support for sound and graphics
* Inline assembler that can access dflat variables and vice-versa
* Fits in to 16KB ROM (including all low-level BIOS)
* Core language can be ported easily - just needs character put and get routines for input/output

The base version here is targeted for the Oric-1 and Atmos computers from the early 80s and due to being integer only and tokenisation, is much faster than Oric BASIC.

Right now I am in refinement and documentation mode. Please see the wiki pages for details of the language.

The source and binaries are in the Oric-DFLAT folder - the readme provides a quickstart guide to try dflat rather wihtout building it from scratch.

I also have a hackaday page for this, although this repo will always have the latest code;
https://hackaday.io/project/175585-oric-1-dflat-system
