def_start(speed,rep)
 println "Parsing RTTL data.."
 _initMusic()
 musLen=len(musStr$)
 println "RTTL length=",musLen
 println musStr$
 println "Press any key.."
 z=get(1)
 repeat
     musIdx=1
	 repeat
      println musIdx," ",octave[musIdx]," ",note[musIdx]," ",duration[musIdx]
	  music 1,octave[musIdx],note[musIdx],0:play 1,0,1,1000
	  wait duration[musIdx]
	  musIdx=musIdx+1
	 until note[musIdx]==-1
     println
 until rep
 println "Done!"
enddef
;
def_initMusic()
 dim musStr$[255],musChar$[2]
;Popcorn2:d=8,o=6,b=140:
 musStr$="c,a#5,c,g5,16d#5,g5,4c5,c,a#5,c,g5,16d#5,g5,4c5,c,d,d#,16d,d#,"
 musStr$=musStr$+"16d#,c,d,16c,d,16d,a#5,c,16a#5,c,16c,g#5,4c,c,a#5,c,g5,d#5,g5,"
 musStr$=musStr$+"4c5,c,a#5,c,g5,16d#5,g5,4c5,c,d,d#,16d,d#,16d#,c,d,16c,d,16d,"
 musStr$=musStr$+"a#5,c,16a#5,c,16c,d,4d#"
 musDefOct=6
 musBeat=3000/100
 musDefDur=32/8
;
;TakeOnMe:d=16,o=5,b=100:
; musStr$="8p,a#,a#,a#,8f#,8d#,8g#,8g#,g#,c6,c6,c#6,d#6,c#6,c#6,c#6,"
; musStr$=musStr$+"8g#,8f#,8a#,8a#,a#,g#,g#,a#,g#,a#,a#,a#,8f#,"
; musStr$=musStr$+"8d#,8g#,8g#,g#,c6,c6,c#6,d#6,c#6,c#6,c#6,8g#,8f#,8a#,8a#"
; musDefOct=5
; musBeat=3000/100
; musDefDur=32/16
;
 dim note[200],octave[200],duration[200]
 curIdx=1
 musIdx=1
 repeat
  octave[curIdx]=musDefOct
  duration[curIdx]=musDefDur
  musChar$=mid(musStr$,musIdx,1)
  if (musChar$>"0") & (musChar$<"9")
   duration[curIdx]=32/(asc(musChar$)-'0')
   if musChar$=="1"
    musIdx=musIdx+1
    musChar$=mid(musStr$,musIdx,1)
    if musChar$=="6"
	 duration[curIdx]=32/16
	else
	 musIdx=musIdx-1
	endif
   elif musChar$=="3"
    duration[curIdx]=32/32
	musIdx=musIdx+1
   endif
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if (musChar$>="a") & (musChar$<="p")
   if   musChar$=="c":note[curIdx]=0
   elif musChar$=="d":note[curIdx]=2
   elif musChar$=="e":note[curIdx]=4
   elif musChar$=="f":note[curIdx]=5
   elif musChar$=="g":note[curIdx]=7
   elif musChar$=="a":note[curIdx]=9
   elif musChar$=="b":note[curIdx]=11
   else:note[curIdx]=12
   endif
   musIdx=musIdx+1
   musChar$=mid(musStr$,musIdx,1)
   if musChar$=="#"
	note[curIdx]=note[curIdx]+1
   else
    musIdx=musIdx-1
   endif
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if (musChar$>="4") & (musChar$<="7")
   octave[curIdx]=asc(musChar$)-'0'
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if musChar$=="."
   duration[curIdx]=duration[curIdx]+duration[curIdx]/2
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if musChar$==","
   musIdx=musIdx+1
  endif
  duration[curIdx]=duration[curIdx]*musBeat/speed
; println musIdx," ",curIdx," ",note[curIdx]," ",octave[curIdx]," ",duration[curIdx]
  curIdx=curIdx+1
 until musIdx>len(musStr$)
 note[curIdx]=-1
enddef
;

def_initSprites()
 dim plotCode[200]
 dim spriteData[100,20]
 dim pat$[20]
 ptr=spriteData
 repeat
  i=1:read pat$
  while i<=len(pat$)
   poke ptr,asc(mid(pat$,i,1))-'0'+16
   ptr=ptr+1
   i=i+1
  wend
 until pat$==""
 asmPlot
  
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
.xpos:.db 0
.ypos:.db 0
.rowsRemaining:.db 0
.colsRemaining:.db 0
.colOffset:.db 0
.drawSprite:;A=Sprite #, X,Y=Coords
 .stx xpos:.sty ypos
 .ldx #0:.stx spriteSrc+1
 ; Calculate sprite# * 128 for offset
 ; Instead of rotating left 7 times
 ; rotate right, carry in to MSB of low byte
 ; then remainder is the high byte
 .clc
 .lsr:.ror spriteSrc+1
 .sta spriteSrc+2
 ; Now add sprite data base to the offset C=0 here
 ; Goes in to self-mod code
 .lda spriteSrc+1:.adc #spriteData&0xff:.sta spriteSrc+1
 .lda spriteSrc+2:.adc #spriteData/256:.sta spriteSrc+2
 ; each sprite is 8 rows tall
 .ldy #8:.sty rowsRemaining
 ; X=starting Y pos
 .ldx ypos
 ; Find firsy y row in range 2..25
.checkYbound
 .txa:.bmi tryNextRow
 .cpx #2:.bcc tryNextRow
 .cpx #26:.bcc doColumns
.tryNextRow
 .inx
 ; Move to next row of sprite source
 .clc:.lda spriteSrc+1:.adc #12:.sta spriteSrc+1
 .lda spriteSrc+2:.adc #0:.sta spriteSrc+2
 .dey:.beq done:.bne checkYbound
.doColumns
 ; Remember rows remaining
 .sty rowsRemaining
 ; Remember starting y position
 .stx ypos
 ; Get base pointer for this row
 .jsr grget
 ; Put pointer in to self-mod code
 .lda grptr:.sta spriteDst+1
 .lda grptr+1:.sta spriteDst+2
 ; each sprite is 12 cols wide
 .ldy #12:.sty colsRemaining
 .ldx xpos
 ; Find first x col in range 2..37
.checkXbound
 .txa:.bmi tryNextCol
 .cpx #2:.bcc tryNextCol
 .cpx #38:.bcc doPlot
.tryNextCol
 ; Move to next col of sprite source
 .clc:.lda spriteSrc+1:.adc #1:.sta spriteSrc+1
 .lda spriteSrc+2:.adc #0:.sta spriteSrc+2
 .inx:.dey:.beq done:.bne checkXbound
.doPlot
 ; Remember rows remaining
 .sty colsRemaining
 ; Add xpos to destination
 .clc:.txa:.adc spriteDst+1:.sta spriteDst+1
 .lda spriteDst+2:.adc #0:.sta spriteDst+2
.writeSrcDstByte
.spriteSrc
 .lda spriteData,x
 .sta spriteDst,x
 .ldx xpos:.ldy ypos
 .jsr grxybase
 .clc
 .lda grptr:.adc #0x80:.sta 
 
.bltRows:.db 0
.bltCols:.db 0
.bltSrc:.dw 0
.bltDst:.dw 0
.bltANDMask:.db 0
.bltORMASK:.db 0
.bltSrcSkip:.db 0
.bltDstSkip:.db 0
.blt
 .ldy bltRows
 .beq bltDone
 .bmi bltDone
 .dey
 .ldx bltCols
 .beq bltDone
 .bmi bltDone
 .dex
 .stx blt1Row+1
 .lda bltSrc
 .sta bltGetSrc+1
 .lda bltSrc+1
 .sta bltGetSrc+2
 .lda bltANDMask
 .sta bltDoAND+1
 .lda bltORMASK
 .sta bltDoOR+1
 .lda bltDst
 .sta bltWrtDst+1
 .lda bltDst+1
 .sta bltWrtDst+2
 .lda bltSrcSkip
 .sta bltSrcOffset+1
 .lda bltDstSkip
 .sta bltDstOffset+1
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
 
def_asmMus(o)
 .opt o
 .org musCode
.startASM
.getNote
 .lda #0
 .sta addr(musNote)
 .sta addr(musNote)+1
 .sta addr(musOct)+1
 .sta addr(musDur)+1
 .lda addr(musDefOct)
 .sta addr(musOct)
 .lda addr(musDefDur)
 .sta addr(musDur)
 .ldx addr(musIdx)
 .lda musStr$,x
 .beq done
.checkDuration
 .cmp #'1':.beq doDuration1or16
 .cmp #'2':.beq doDuration2
 .cmp #'3':.beq doDuration32
 .cmp #'4':.beq doDuration4
 .cmp #'8':.beq doDuration8
.checkNote
 .lda musStr$,x
 .cmp #'c':.beq doC
 .cmp #'d':.beq doD
 .cmp #'e':.beq doE
 .cmp #'f':.beq doF
 .cmp #'g':.beq doG
 .cmp #'a':.beq doA
 .cmp #'b':.beq doB
 .cmp #'p':.beq doP
.checkOctave
 .lda musStr$,x
 .cmp #'4':.beq doO4
 .cmp #'5':.beq doO5
 .cmp #'6':.beq doO6
 .cmp #'7':.beq doO7
.checkPeriod
 .lda musStr$,x
 .cmp #'.':.beq doPeriod
.checkComma
 .lda musStr$,x
 .cmp #',':.beq doComma
.done
 .stx addr(musIdx)
 .rts
; Proces duration of note
.doDuration1or16
 .ldy musStr$+1,x
 .cpy #'6'
 .bne doDuration1
 .lda #'0'+16
 .inx
.doDuration1
.doDuration2
.doDuration4
.doDuration8
 .sec:.sbc #'0'
 .ldy #64
 .sty addr(musDur)
.adjustDuration
 .lsr addr(musDur)
 .lsr
 .bne adjustDuration
 .inx
 .bne checkNote
.doDuration32
 .lda #'0'+32
 .inx
 .bne doDuration1
; Process note and sharp
.doP
 .ldy #12
 .db 0x2c
.doC
 .ldy #0
 .db 0x2c
.doD
 .ldy #2
 .db 0x2c
.doE
 .ldy #4
 .db 0x2c
.doF
 .ldy #5
 .db 0x2c
.doG
 .ldy #7
 .db 0x2c
.doA
 .ldy #9
 .db 0x2c
.doB
 .ldy #11
 .lda musStr$+1,x
 .cmp #'#'
 .bne skipSharp
 .iny
 .inx
.skipSharp
 .sty addr(musNote)
 .inx
 .bne checkOctave
; Process octave 4,5,6,7
.doO4
.doO5
.doO6
.doO7
 .sec:.sbc #'0'
 .sta addr(musOct)
 .inx
 .bne checkPeriod
; Process dotted period
.doPeriod
 .lda addr(musDur)
 .lsr addr(musDur)
 .clc
 .adc addr(musDur)
 .sta addr(musDur)
 .inx
 .bne checkComma
; Process comma
.doComma
 .inx
 .bne done
.endASM
enddef

TakeOnMe:d=16,o=5,b=100:8p,a#,a#,a#,8f#,8d#,8g#,8g#,g#,c6,c6,c#6,d#6,c#6,c#6,c#6,8g#,8f#,8a#,8a#,a#,g#,g#,a#,g#,a#,a#,a#,8f#,8d#,8g#,8g#,g#,c6,c6,c#6,d#6,c#6,c#6,c#6,8g#,8f#,8a#,8a#
