; WORK IN PROGRESS!
def_start()
 _initSprites()
 paper 0:text:cursor 1
 for a=40*8,40*20-1,1:poke 48000+a,16:next
 i=-64
 _dplot("People are in a hurry!!",2,1,10,2,3)
 _dplot("They're rushing to get to..",6,1,10,1,2)
 _dplot("The Retrocomputer Festival",20,1,14,0,0)
 _dplot("Cambridge 4th & 5th November!",24,1,14,0,0)
 vol=7
 repeat
  reset t
  a=call(bigSpr, 6, i-40,10)
  a=call(bigSpr, i&6>>1, i-20,10)
  a=call(bigSpr, 5, i, 10)
  if(i>-8)&(i<80)
   plot 1,20,2:plot 1,21,6
   plot 1,24,3:plot 1,25,5
  else
   plot 1,20,0:plot 1,21,0
   plot 1,24,0:plot 1,25,0
  endif
  i=i+1:if i>128:i=-64:endif
  sound 1,100+(i&16)*2,vol
  repeat:until elapsed(t)>1
 until 0
enddef
;
def_dplot(a$,y,dy,a,c1,c2)
 redim a$[40]
 plot 0,y,a:plot 0,y+dy,a
 plot 1,y,c1:plot 1,y+dy,c2
 plot 20-len(a$)/2,y,a$
 plot 20-len(a$)/2,y+dy,a$
enddef
;
; def_something
def_initSprites()
 dim plotCode[200]
 dim spriteData[100,20]
 dim pat$[129], line$[25]
 ptr=spriteData
 row=0
; println "Sprite data =",hex(addr(spriteData))
; println "Pattern data=",hex(addr(pat$))
; println "Assembling.."
; println "Pass 0"
 _asmPlot(0)
; println "Pass 1"
 _asmPlot(0)
; println "Pass 2"
 _asmPlot(2)
 repeat
  read sprNum
  if sprNum<>-1
   pat$=""
   for i=0,7,1
    read line$
    pat$=pat$+line$
   next
   i=call(strToSpr, sprNum, addr(spriteData)>>8, addr(spriteData)&0xff)
  endif
 until sprNum==-1
enddef
; 
data 0 
data "     33333      "
data "   333331133    "
data "  33333331333   "
data "  33333333      "
data "  333333        "
data "  33333333      "
data "   333333333    "
data "     33333      "
;
data 1
data "     33333      "
data "   333331133    "
data "  33333331333   "
data "  333333333     "
data "  333333        "
data "  33333333333   "
data "   333333333    "
data "     33333      "
;
data 2
data "     33333      "
data "   333331133    "
data "  33333331333   "
data "  33333333333   "
data "  333333        "
data "  33333333333   "
data "   333333333    "
data "     33333      "
;
data 3
data "     33333      "
data "   333331133    "
data "  33333331333   "
data "  33333333333   "
data "  33333333333   "
data "  33333333333   "
data "   333333333    "
data "     33333      "
;
data 4
data "     66666      "
data "   666666666    "
data "  6667 667 66   "
data "  6667 667 66   "
data "  66666666666   "
data "  66666666666   "
data "  66066066066   "
data "  6  6  6  6    "
;
data 5
data "     55555      "
data "   555555555    "
data "  5557 557 55   "
data "  5557 557 55   "
data "  55555555555   "
data "  55555555555   "
data "  55 55 55 55   "
data "   5  5  5  5   "
;
data 6
data "     11111      "
data "   111111111    "
data "  1117 117 11   "
data "  1117 117 11   "
data "  11111111111   "
data "  11111111111   "
data "  11 11 11 11   "
data "   1  1  1  1   "
;
data -1
data ""
;
;
def_asmPlot(o)
 .opt o:.org plotCode
 ztmp24=0xb0
 kbstick=0xc000+0x09*3
 sndset=0xc000+0x08*3
 grplot=0xc000+0x0a*3
 grget=0xc000+0x11*3
 grxybase=0xc000+0x0f*3
 grtext=0xc000+0x00*3
 dfrtsprupdate=0xc000+0x0e*3
 grptr=0x11+0x15
 scrRow=ztmp24+2
 xpos=ztmp24+3
 ypos=ztmp24+4
 tempAdr=ztmp24
 ;
.asmStart
.strToSpr:;A=Sprite#, XY=Sprite base HL
 ;Sprite# * 128
 lsr:sta ztmp24+1
 lda #0:ror:sta ztmp24
 ;Add sprite offset to sprite base
 tya:clc:adc ztmp24:sta ztmp24
 txa:adc ztmp24+1:sta ztmp24+1
 ;Source is pat$
 lda #addr(pat$)&0xff:sta ztmp24+2
 lda #addr(pat$)>>8:sta ztmp24+3
 ldy #0
.charToBin
 lda (ztmp24+2),y
 beq strToSprDone
 sec:sbc #' ':ora #16
 sta (ztmp24),y
 iny
 bne charToBin
.strToSprDone
 tya:tax:lda #0
 rts
;
.eraseSpr
 pha
 lda #0:sta bltMsk+1
 lda #16:sta bltMsk+3
 pla
 jmp init
.bigSpr:;A=Sprite #, X,Y=Coords
 pha
 lda #0xff:sta bltMsk+1
 lda #0:sta bltMsk+3
 pla
.init
 stx xpos:sty ypos
 ; Check if all of sprite would be
 ; out of bounds, if so do nothing
 cpy #28:bcc doSetup
 cpy #(256-8):bcs doSetup
 cpx #40:bcc doSetup
 cpx #(256-16):bcs doSetup
 ; Nothing can be seen!
 rts
.doSetup
 ; Calculate sprite# * 128 for offset
 ; Instead of rotating left 7 times
 ; rotate right, carry in to MSB of low byte
 ; then remainder is the high byte
 ; Also clear source address
 lsr:sta bltSrc+2
 lda #0:sta tempAdr+1:ror:sta bltSrc+1
 ; Now add sprite data base to the offset C=0 here
 ; Goes in to self-mod code
 lda bltSrc+1:adc #spriteData&0xff:sta bltSrc+1
 lda bltSrc+2:adc #spriteData>>8:sta bltSrc+2
 ; Get base address of text screen
 ; 0xbb80 + (32*y)+(8*y)
 ; First calc 8*y
 tya
 bpl notNegY
 dec tempAdr+1
.notNegY
 asl:rol tempAdr+1
 asl:rol tempAdr+1
 asl:rol tempAdr+1
 sta tempAdr
 ; Save 8*y in destination
 sta bltDst+1
 lda tempAdr+1
 sta bltDst+2
 ; Now calc 32*y (2 more shifts of tempAdr)
 lda tempAdr
 asl:rol tempAdr+1
 asl:rol tempAdr+1
 ; Now add this to bltDst
 clc
 adc bltDst+1
 sta bltDst+1
 lda tempAdr+1
 adc bltDst+2
 sta bltDst+2
 ; Now add screen base address 0xbb80
 clc
 lda bltDst+1
 adc #0x80
 sta bltDst+1
 lda bltDst+2
 adc #0xbb
 sta bltDst+2
 ; Start blt from beginning of sprite data
 ldy #0
 lda ypos
 sta scrRow
.bltRow
 ; Start blit for a row, but not if row out of bounds
 lda scrRow
 cmp #28:bcc doRow
 ; Add 16 bytes to sprite row as we're skipping it altogether
 tya:clc:adc #16:tay:bne nextRow
.doRow
 ; Start at desired xpos
 ldx xpos
.bltByte
 ; If out of bounds do not write
 cpx #40:bcs skipx
 ; Get a byte of sprite data based on index Y
.bltSrc:lda 0xffff,y
 ; Mask it
.bltMsk:and #0xff:ora #0
 ; If zero do not write
 beq skipx
.bltDst:sta 0xffff,x
.skipx
 ; Next x pos on screen
 inx
 ; Next data pos on sprite
 iny
 ; If not on 16 byte boundary of sprite, do next byte of row
 tya
 and #0x0f
 bne bltByte
.nextRow
 ; Prepare for next row
 inc scrRow
 ; Update destination address
 clc
 lda bltDst+1:adc #40:sta bltDst+1
 lda bltDst+2:adc #0:sta bltDst+2
 ; Done all 128 bytes of sprite?
 cpy #128
 ; No, then next row
 bne bltRow
 rts
.asmEnd
enddef
;println "Program loaded - type _start() to start!"
_start()
