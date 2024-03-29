def_start()
 reset t:t=rnd(t)
 _init()
 state=0
 repeat
  if state==0:_attract():endif
  if state==1:_initGame():endif
  if state==2:_playGame():endif
 until 0
enddef
;
def_init()
 dim a$[5],b$[5],floating$[5],thrustUp$[5],thrustLeft$[5],thrustRight$[5]
 paper 4:ink 3:text:cursor 1
 plot 2,0,12:plot 3,0,"INITIALISING..PLEASE WAIT"
 repeat:read c:if c<>-1
  for i=0,7,1
   read b:poke 0xb400+8*c+i,b
  next
 endif:until c<0
 floating$=chr(35)+chr(36)
 thrustUp$=chr(37)+chr(38)
 thrustLeft$=chr(40)+chr(41)
 thrustRight$=chr(42)+chr(43)
 padLeft=220
 padTop=188
 _assemble()
 hires:cursor 1
 score=0:level=0:fuel=0
enddef
;
def_attract()
 hires
 plot 60,10, "Oric Lander in dflat"
 plot 24,30, "Land your spacecraft on the base"
 plot 24,45, "bottom right avoid obstacles and"
 plot 24,60, "boundaries and be sure to make a"
 plot 75,75, "gentle landing!"
 plot 20,90, "Controls:"
 plot 30,105,"   Left arrow = Thrust Left"
 plot 30,120,"  Right arrow = Thrust Right"
 plot 30,135,"     Up arrow = Thrust Up"
 plot 15,170,"Press any key to start - Good Luck!"
 _showStatus()
 k=get(1)
 level=1
 score=0
 state=1
enddef
;
def_initGame()
  hires
  fuel=500
  g=2:sx=2:sy=6:p=0
  _showStatus()
  _drawObstacles()
  state=2
enddef
;
def_showStatus()
  printat 2,0,"FUEL:",fuel," "
  printat 16,0,"LEVEL:",level
  printat 29,0,"SCORE:",score
enddef
;
def_drawObstacles()
	pixmode 1
	for i=0,3,1
	  line i,i,239-i,i:lineto 239-i,199-i
	  lineto i,199-i:lineto i,i
	next
	for i=0,30+level*10,1
  	  line padLeft,padTop+7,239,padTop+7
      x=rnd(0)\210+15:y=rnd(0)\180+10
	  if ((x>25)|(y>25))&((x<(padLeft-6))|(y<(padTop-8)))
	    plot x,y,"/"
	  endif
	next
enddef
;
def_playGame()
  play 0,0,0,0:sound 0,16,0:sound 1,1,10
  x=4<<6:y=4<<6:dx=0:dy=0
  xx=x>>6:yy=y>>6
  a$=floating$:b$=a$
  pixmode -1:plot xx,yy,a$
  repeat
    ox=xx:oy=yy
    dy=dy+g
    if dy>=128:dy=128:elif dy<=-128:dy=-128:endif
    if dx>=128:dx=128:elif dx<=-128:dx=-128:endif
    y=y+dy:x=x+dx
    s=stick()
    if (s&7)&(fuel>0):play 0,1,0,0
      if s&4:dy=dy-sy:b$=thrustUp$:fuel=fuel-2
      elif s&1:dx=dx-sx:b$=thrustLeft$:fuel=fuel-1
      elif s&2:dx=dx+sx:b$=thrustRight$:fuel=fuel-1:endif
      if fuel<0:fuel=0:endif
      printat 7,0,fuel," "
    else
      b$=floating$:play 0,0,0,0
    endif
    xx=x>>6:yy=y>>6
    if(xx<>ox)|(xy<>oy)|(a$<>b$)
;     plot ox,oy,a$
;     p=pixel(xx+5,yy)+pixel(xx+2,yy+3)+pixel(xx,yy+7)+pixel(xx+2,yy+7)
;     p=p+pixel(xx+6,yy)+pixel(xx+9,yy+3)+pixel(xx+11,yy+7)+pixel(xx+9,yy+7)
	  p=call(plotShip,0,0,0)
;     a$=b$
;     plot xx,yy,a$
    endif
  until p
  play 0,0,0,0
  if (xx>=padLeft)&(yy>=padTop)&(dx>=-18)&(dx<=18)&(dy<=22)
	_doWon()
    wait 100
	state=1
  else
    _doCrash()
    wait 200
    state=0
  endif
enddef
;
def_doWon()
  print chr(7)
  printat 13,1,"LANDED SAFELY!"
  level=level+1
  score=score+fuel
  _showStatus()
enddef
;
def_doCrash()
  printat 8,1,"YOU DESTROYED THE CRAFT"
  sound 0,31,0:sound 1,20,0:sound 2,20,0:sound 3,20,0:play 7,7,1,4000:play 7,7,1,4000
  pixmode 1:for i=1,6,1:circle xx+5,yy+3,i:wait 5:next
  pixmode 0:for i=1,6,1:circle xx+5,yy+3,i:wait 5:next
enddef
;
def_assemble()
 dim asmCode[200]
 _doAsm(0):_doAsm(0):_doAsm(2)
enddef
;
def_doAsm(o)
 pixRoutine=0xc000+0x10*3
 hcharRoutine=0xc000+0x0b*3
 .opt o
 .org asmCode
 .plotShip
 ;   plot ox,oy,a$
 ldx addr(ox):ldy addr(oy):lda a$
 jsr hcharRoutine
 lda addr(ox):clc:adc #6:tax:ldy addr(oy):lda a$+1
 jsr hcharRoutine
 ;
 ; Collision check
 ;pixel(xx+5,yy)
 lda addr(xx):clc:adc #5:tax:ldy addr(yy)
 jsr pixRoutine:sta tmpP
 ;pixel(xx+2,yy+3)
 lda addr(xx):clc:adc #2:tax:lda addr(yy):adc #3:tay
 jsr pixRoutine:clc:adc tmpP:sta tmpP
 ;pixel(xx,yy+7)
 ldx addr(xx):lda addr(yy):adc #7:tay
 jsr pixRoutine:clc:adc tmpP:sta tmpP
 ;pixel(xx+2,yy+7)
 lda addr(xx):adc #2:tax:lda addr(yy):adc #7:tay
 jsr pixRoutine:clc:adc tmpP:sta tmpP
 ;pixel(xx+6,yy)
 lda addr(xx):adc #6:tax:ldy addr(yy)
 jsr pixRoutine:clc:adc tmpP:sta tmpP
 ;pixel(xx+9,yy+3)
 lda addr(xx):adc #9:tax:lda addr(yy):adc #3:tay
 jsr pixRoutine:clc:adc tmpP:sta tmpP
 ;pixel(xx+11,yy+7)
 lda addr(xx):adc #11:tax:lda addr(yy):adc #7:tay
 jsr pixRoutine:clc:adc tmpP:sta tmpP
 ;pixel(xx+9,yy+7)
 lda addr(xx):adc #9:tax:lda addr(yy):adc #7:tay
 jsr pixRoutine:clc:adc tmpP:pha
 ;
 ; a$=b$
 lda b$:sta a$
 lda b$+1:sta a$+1
 ;
 ;   plot xx,yy,a$
 ldx addr(xx):ldy addr(yy):lda a$
 jsr hcharRoutine
 lda addr(xx):clc:adc #6:tax:ldy addr(yy):lda a$+1
 jsr hcharRoutine
 ;
 ;return int - low = X, high = A
 pla
 tax
 lda #0
 rts
 .tmpP:.ds 1
enddef
;
; Graphics data - char number followed by 8 bytes of data
; char number -1 terminates
data 35
data 0b000001
data 0b000111
data 0b001101
data 0b001111
data 0b001111
data 0b000111
data 0b001000
data 0b111000
data 36
data 0b100000
data 0b111000
data 0b101100
data 0b111100
data 0b111100
data 0b111000
data 0b000100
data 0b000111
data 37
data 0b000001
data 0b000111
data 0b001101
data 0b001111
data 0b001111
data 0b000111
data 0b001001
data 0b111011
data 38
data 0b100000
data 0b111000
data 0b101100
data 0b111100
data 0b111100
data 0b111000
data 0b100100
data 0b110111
data 40
data 0b000001
data 0b000111
data 0b001101
data 0b001111
data 0b001111
data 0b000111
data 0b001000
data 0b111000
data 41
data 0b100000
data 0b111000
data 0b101101
data 0b111111
data 0b111101
data 0b111000
data 0b000100
data 0b000111
data 42
data 0b000001
data 0b000111
data 0b101101
data 0b111111
data 0b101111
data 0b000111
data 0b001000
data 0b111000
data 43
data 0b100000
data 0b111000
data 0b101100
data 0b111100
data 0b111100
data 0b111000
data 0b000100
data 0b000111
data 47
data 0b001100
data 0b110111
data 0b011011
data 0b110110
data 0b111011
data 0b110110
data 0b011100
data 0b000000
data -1
_start()
