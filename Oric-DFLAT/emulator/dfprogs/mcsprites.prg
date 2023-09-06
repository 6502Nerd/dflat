; Sprites and machine code demo
; Uses dflat software sprite
; system. Also inline assembler
; to speed up calculating
; sprite coords.
; _test(max,d)
; max=max # of sprites 1-32
; d=delay (50ms ticks)
def_test(max,d)
dim x[32],y[32],dx[32],dy[32]
dim code[250]
dim a$[100]
text:cursor 1:_initGfx()
_asmAll()
_plotMsg(15,11,"dflat software sprites:")
_plotMsg(16,12,"Up to 32 sprites using")
_plotMsg(16,13,"dflat commands only.")
_plotMsg(16,14,"inline assembler for")
_plotMsg(16,15,"sprite positions only")
_plotMsg(16,16,"in this example")
reset t:t=rnd(t)
for i=1,32,1
 x[i]=-1:y[i]=0
 sprchar i-1,33
next
for i=1,max,1
 x[i]=rnd(0)\34+4
 y[i]=rnd(0)\24+2
 t=rnd(0)\2:if t==0:t=-1:endif
 dx[i]=t
 t=rnd(0)\2:if t==0:t=-1:endif
 dy[i]=t
next
repeat
 t=call(code,max-1,0,0)
 sprmulti x,y
 sprupd
 wait d
until 0
enddef
;
def_plotMsg(p,q,a$)
local i
 i=1
 repeat
  plot p,q,asc(mid(a$,i,1))+128
  p=p+1
  i=i+1
 until i>len(a$)
enddef
;
def_initGfx()
local d
; Read character data and poke
; in to char 33. Std char set
; in text mode is 0xb400. Note
; that the if the decimal
; value of 46080 was used, the
; tokeniser would show this as
; -19456 hence best to show
; use hex number format even
; though it would work fine.
 for a=0,7,1
  read d
  poke 0xb400+33*8+a,d
 next
enddef
;
; Alien bitmap definition
data 0b00100001
data 0b00010010
data 0b00011110
data 0b00101101
data 0b00111111
data 0b00100001
data 0b00011110
data 0b00100001
;
; Need to make 3 passes of asm code
def_asmAll()
 _asmMain(0):_asmMain(0)
 _asmMain(3)
enddef
;
; The asm code
def_asmMain(o)
 .opt o
; Assemble in to array code
 .org code
; asmStart label is start of
; code could also use array
; code.
; On entry, A=highest sprite
; number to process.
.asmStart
; 2 bytes per array element
 asl
; Using X as index in to sprite
; x,y array. Notice that the
; asm can refer to arrary vars
; x[] and y[] - the base
; address of the arrays are
; used similar to C
 tax
.loop
; x[X]=x[X]+dx[X]
 clc
 lda x,x
 adc dx,x
 sta x,x
 lda x+1,x
 adc dx+1,x
 sta x+1,x
; y[X]=y[X]+dy[X]
 clc
 lda y,x
 adc dy,x
 sta y,x
 lda y+1,x
 adc dy+1,x
 sta y+1,x
; if x[X] is 2 or 39
; then dx[X]=0-dx[x]
 lda x,x
 cmp #2
 beq negDX
 cmp #39
 bne skipDX
.negDX
 sec
 lda #0
 sbc dx,x
 sta dx,x
 lda #0
 sbc dx+1,x
 sta dx+1,x
.skipDX
; if y[X] is 0 or 27
; then dy[X]=0-dy[x]
 lda y,x
 cmp #0
 beq negDY
 cmp #27
 bne skipDY
.negDY
 lda #0
 sbc dy,x
 sta dy,x
 lda #0
 sbc dy+1,x
 sta dy+1,x
.skipDY
; X=X-2
; (2 bytes per array element)
 dex
 dex
 bpl loop
; back to dflat
 rts

