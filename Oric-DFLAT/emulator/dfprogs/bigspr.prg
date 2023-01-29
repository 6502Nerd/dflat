def_start()
 _initSprites()
 repeat
  input yy
  a=call(drawSprite,0,10,yy)
  pr.peek(rowStart)," ",peek(rowExtent)
 until yy==999
enddef
; def_something
def_initSprites()
 dim plotCode[200]
 dim spriteData[100,20]
 dim pat$[20]
 ptr=spriteData
 repeat
  i=1:read pat$:pr.pat$
  while i<=len(pat$)
   poke ptr,asc(mid(pat$,i,1))-'0'+16
   ptr=ptr+1
   i=i+1
  wend
 until pat$==""
 println "Pass 0":_asmPlot(0)
 println "Pass 1":_asmPlot(0)
 println "Pass 2":_asmPlot(3)
enddef
;  
data "   33333    "
data " 333331133  "
data "33333333333 "
data "33333333    "
data "333333      "
data "33333333    "
data " 333333333  "
data "   33333    "
data "                                "
;
data "   33333    "
data " 333331133  "
data "33333333333 "
data "333333333   "
data "333333      "
data "33333333333 "
data " 333333333  "
data "   33333    "
data "                                "
;
data "   33333    "
data " 333331133  "
data "33333333333 "
data "33333333333 "
data "333333      "
data "33333333333 "
data " 333333333  "
data "   33333    "
data "                                "
;
data "   33333    "
data " 333331133  "
data "33333333333 "
data "33333333333 "
data "33333333333 "
data "33333333333 "
data " 333333333  "
data "   33333    "
data "                                "
;
data "   66666    "
data " 666666666  "
data "66670667066 "
data "66670667066 "
data "66666666666 "
data "66666666666 "
data "66 66 66 66 "
data "6  6  6  6  "
data "                                "
;
data "   66666    "
data " 666666666  "
data "66670667066 "
data "66670667066 "
data "66666666666 "
data "66666666666 "
data "66 66 66 66 "
data " 6  6  6  6 "
data "                                "
;
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
 dfarga=0xa1
 dfargb=0xa3
 dfargc=0xa5
 sprcurX=0xb400
 sprnewX=0xb420
 sprcurY=0xb440
 sprnewY=0xb460
 sprchr=0xb480
 sprbgnd=0xb4a0
 ;
.sprNum:.db 0
.xpos:.db 0
.ypos:.db 0
.scrRow:.db 0
.scrCol:.db 0
.rowStart:.db 0
.rowExtent:.db 0
.colStart:.db 0
.colExtent:.db 0
.dex
.drawSprite:;A=Sprite #, X,Y=Coords
 .dex:.stx xpos:.dey:.sty ypos:.sta sprNum
 ; Check if all of sprite would be
 ; out of bounds, if so do nothing
 .cpy #26:.bcc adjustRow
 .cpy #(255-8):.bcc notVisible
.adjustRow
 .tya:.clc:.adc #8
 .cmp #8:.bcc clipTop
 .cmp #26:.bcc checkCol
 .sty .scrRow:
.clipTop 
 .cpx #38:.bcc checkColExtent
 .cpx #(255-12):.bcc notVisible
 ; Ok it's visible but does it need clipping
.doClip
 .stx xpos:.sty ypos
 .stx scrCol:.sty scrRow
 .cpx 
 .stx colStart:.sty rowStart
 ; Assume no clipping
 .ldx #8:.stx rowExtent:.ldx #12:.stx colExtent
 .ldx #0
 ; Calculate source address
 .stx bltSrc+1
 ; Calculate sprite# * 128 for offset
 ; Instead of rotating left 7 times
 ; rotate right, carry in to MSB of low byte
 ; then remainder is the high byte
 .clc
 .lsr:.ror bltSrc+1
 .sta bltSrc+2
 ; Now add sprite data base to the offset C=0 here
 ; Goes in to self-mod code
 .lda bltSrc+1:.adc #spriteData&0xff:.sta bltSrc+1
 .lda bltSrc+2:.adc #spriteData/256:.sta bltSrc+2
 ; Clip between 1,1->38,26 inclusive
 ; Sprites are row 1..26 - 26 row window
 ; How many rows to display from 8 in total?
 ; Check bottom border
 .tya:.clc:.adc #7
 .cmp #27:.bcc topBorder
 ; C=1 here.
 ; Ok sprite bottom > 26
 ; how many sprites rows to row 26?
 ; we need row extent
 .sbc #26:.sta rowExtent
 .lda #8:.sbc rowExtent:.sta rowExtent
 .bne rightBorder
 ; Check top border
.topBorder:;c=0 here
 ;Subtract 7 (-6-C)
 .sbc #6:.bcs rightBorder
 ; Ok clip top of sprite, C=0 here
 .adc #7:.sta rowExtent
 .lda #8:.sbc rowExtent:.sta rowStart
.rightBorder
.notVisible
 .rts
;
.bltRows:.db 0
.bltCols:.db 0
.bltSrc:.dw 0
.bltDst:.dw 0
.bltANDMask:.db 0
.bltORMASK:.db 0
.bltSrcSkip:.db 0
.bltDstSkip:.db 0
.blt
 ; If y<=0 then do nothing
 .ldy bltRows:.beq bltDone:.bmi bltDone
 ; num rows-1
 .dey
 ; If x<=0 then do nothing
 .ldx bltCols:.beq bltDone:.bmi bltDone
 ; num cols-1
 .dex:.stx blt1Row+1
 ; Initialise blit variables
 .lda bltSrc:.sta bltGetSrc+1
 .lda bltSrc+1:.sta bltGetSrc+2
 .lda bltANDMask:.sta bltDoAND+1
 .lda bltORMASK:.sta bltDoOR+1
 .lda bltDst:.sta bltWrtDst+1
 .lda bltDst+1:.sta bltWrtDst+2
 .lda bltSrcSkip:.sta bltSrcOffset+1
 .lda bltDstSkip:.sta bltDstOffset+1
.blt1Row:;Self modifying code+1
 .ldx #0xff
.blt1Col
.bltGetSrc:;Self modifying code+1+2
 .lda 0xffff,x 
.bltDoAND:;Self modifying code+1
 .and #0xff
.bltDoOR:;Self modifying code+1
 .ora #0xff
.bltWrtDst:;Self modifying code+1+2
 .sta 0xffff,x
 .dex
 .bpl blt1Col
 .clc
.bltSrcOffset:;Self modifying code+1
 .lda #0xff
 .adc bltGetSrc+1
 .sta bltGetSrc+1
 .lda #0
 .adc bltGetSrc+2
 .sta bltGetSrc+2
 .clc
.bltDstOffset:;Self modifying code+1
 .lda #0xff
 .adc bltWrtDst+1
 .sta bltWrtDst+1
 .lda #0
 .adc bltWrtDst+2
 .sta bltWrtDst+2
 .dey
 .bpl blt1Row
 .rts
