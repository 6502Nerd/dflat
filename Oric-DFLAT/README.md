# About Oric
I've owned the same Oric-1 since 1983 (my first computer!) and thought it was well enough known.. but apparently not!

So for the unitiated, the Oric-1 was a low cost microcomputer designed and built in the UK as a competitor to the ZX Spectrum.

For more information please see: https://en.wikipedia.org/wiki/Oric

# For the impatient (like me)!
If you want to get dflat running quickly, please visit the emulator folder and follow a few short steps in the readme.txt file to spin up an instance of Oricutron running dflat and then load up a game!

# Note: Wiki
A friend recommended that I should point out there is a fairly extensive wiki on dflat.

So after reading this, please have a look through the wiki to really get a feel for this language targeted at constrained 8-bit machines.

https://github.com/6502Nerd/dflat/wiki


# About dflat
dflat is a BASIC-like language for 8-bit micros and retro computers running on 6502 and 65c02.  Key features & highlights;
* Procedure orientated structure (all runnable code within def..enddef blocks)
* Local variables - supports recursion
* Structured programming including if..elif..endif, while..wend, repeat..until
* Line numbers only used for sequencing lines, but cannot be referenced - no goto or gosub!
* Support for sound and graphics
* Inline assembler that can access dflat variables and vice-versa
* Fits in to 16KB ROM (including all low-level BIOS)
* Core language can be ported easily - just needs character put and get routines for input/output

Here is hello world in dflat:
* 10 def_hello()
* 20   println "Hello world!"
* 30 enddef

The base version here is targeted for the Oric-1 and Atmos computers from the early 80s and due to being integer only and tokenisation, is much faster than Oric BASIC.

Right now I am in refinement and documentation mode. Please see the wiki pages for details of the language.

The source and binaries are in the Oric-DFLAT folder - the readme.txt provides a quickstart guide to try dflat rather wihtout building it from scratch.

I also have a hackaday page for this, although this repo will always have the latest code;
https://hackaday.io/project/175585-oric-1-dflat-system

Also on hackaday, I have my homebrew computer made on breadboard and housed in a BBC Micro case. I made dflat for this computer first, then ported to the Oric for hopefully a slightly wider user base!
https://hackaday.io/project/5789-65c02-homebrew-computer-on-breadboard
