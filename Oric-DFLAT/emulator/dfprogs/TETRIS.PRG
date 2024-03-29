; * TETRIS in DFLAT! *
; * 01/2020 6502Nerd *
;
;
;** PLOT SHAPE x,y,s,r **
def_plotShape(x,y,s,r)
  sprpos 0,x+sprX0[s,r],y+sprY0[s,r]
  sprpos 1,x+sprX1[s,r],y+sprY1[s,r]
  sprpos 2,x+sprX2[s,r],y+sprY2[s,r]
  sprpos 3,x+sprX3[s,r],y+sprY3[s,r]
  sprupd
enddef
; 
;
;** SET SHAPE TO P **
def_setShape(p)
  sprchar 0,p:sprchar 1,p:sprchar 2,p:sprchar 3,p
enddef
;
;** SHOW NEXT SHAPE **
def_showNext(sns)
  local p,a
  _setShape(shapeP[sns])
  plot nextX,nextY,nextBlank$:plot nextX,nextY+1,nextBlank$
  a=nextX+screenWidth*nextY
  _plotShape(nextX,nextY,sns,1)
  sprinit
enddef
;
;** STAMP SHAMP IN MAP **
def_stampShape(mo,s,r)
  local idx,p
  p=shapeP[s]
  poke mo+mapOff0[s,r],p
  poke mo+mapOff1[s,r],p
  poke mo+mapOff2[s,r],p
  poke mo+mapOff3[s,r],p
  mo=mo-mapBase
  idx=(mo+mapOff0[s,r])/mapSizeX
  lineCount[idx]=lineCount[idx]+1
  idx=(mo+mapOff1[s,r])/mapSizeX
  lineCount[idx]=lineCount[idx]+1
  idx=(mo+mapOff2[s,r])/mapSizeX
  lineCount[idx]=lineCount[idx]+1
  idx=(mo+mapOff3[s,r])/mapSizeX
  lineCount[idx]=lineCount[idx]+1
  sprinit
enddef
;
;** CHECK SHAPE IN MAP **
def_checkShape(mo,s,r,&f)
  f=0 
  moff = s*2+r*14-16
  if peek(mo+peek(mapOff0+moff))<>blank:f=1:endif
  if peek(mo+peek(mapOff1+moff))<>blank:f=1:endif
  if peek(mo+peek(mapOff2+moff))<>blank:f=1:endif
  if peek(mo+peek(mapOff3+moff))<>blank:f=1:endif
enddef
;
;** DRAW TETRIS GAME AREA **
def_drawGamePanel()
  local q
  for q=2,mapSizeY-1,1
    plot gameX,gameY+q-2,mid(gameMap$[q],2,sizeX)
  next
enddef
;
;** DRAW BORDER **
def_border(bx,by,bw,bh)
  local i
  for i=by,by+bh-1,1
    plot bx,i,borderSolid:plot bx+bw-1,i,borderSolid
	plot bx+1,i,blank:plot bx+bw,i,blank
  next
  for i=bx+1,bx+bw-2,1
    plot i,by,solid
    plot i,by+bh-1,solid
  next
  plot bx,by,borderTL:plot bx+bw-1,by,borderTR
  plot bx,by+bh-1,borderBL:plot bx+bw-1,by+bh-1,borderBR
enddef
;
;** DRAW TOP SCORES **
def_drawTopScores()
  local i,s,y
  for i=1,7,1
    s=topScore[i]:y=i*2+topY-1
	plot topX-2,y,i
    plot topX+1,y,topName$[i]
    if s<1000:printat topX+6,y,s:else:printat topX+5,y,s:endif
  next
enddef
;
;** DRAW STATUS LINE **
def_drawStatusLine()
  local i
  for i=0,1,1
    printat 4,i,"SCORE:",score
    printat 17,i,"LINES:",lines
    printat 29,i,"LEVEL:",level
  next
enddef
;
;** DRAW GAME SCREEN **
def_drawScreen()
  printat 2,0," "
  plot 1,0,2
  plot 1,1,3
  plot 2,0,10
  plot 2,1,10
  plot 1,27,1
  _drawStatusLine()
  _border(gameX-1,gameY,sizeX+2,sizeY+1)
  _border(nextX-1,nextY-1,6,4)
  plot nextX,nextY-1,"NEXT"
  _border(statX-1,statY-1,9,15)
  plot statX+1,statY-1,"STATS"
  _border(topX-1,topY-1,12,16)
  plot topX,topY-1,"TOP SCORES"
  _drawTopScores()
enddef
;
;** ANIMATE SHAPE **
def_animateShape(ss)
  sound 0,20,0:sound 2,100,0:play 2,2,4,1000
  tx=nextX:ty=nextY
  repeat
    _plotShape(tx,ty,ss,1)
    if (tx<sprX):tx=tx+1:endif
    if (tx>sprX):tx=tx-1:endif
    if (ty<sprY):ty=ty+1:endif
    if (ty>sprY):ty=ty-1:endif
  until(tx==sprX)&(ty==sprY)
enddef
;
;** NEW SHAPE AT TOP **
def_newShape()
  sprX=5+offX:sprY=2+offY
  mo=mapBase+5+2*mapSizeX
  ss=ns:sr=1:ns=rnd(0)\7+1
  _showNext(ns)
  _setShape(shapeP[ss])
  _animateShape(ss)
  _plotShape(sprX,sprY,ss,sr)
  stat[ss]=stat[ss]+1
  if stat[ss]<13
    plot statX-1+ss, statY+13-stat[ss],shapeP[ss]
  endif
enddef
;
;** CLEAR GAME MAP **
def_initGameMap()
  local q
  gameMap$[1]=fillLine$:gameMap$[mapSizeY]=fillLine$
  for q=2,mapSizeY-1,1
    gameMap$[q]=blankLine$
    lineCount[q]=0
  next
enddef
;
;**ONE GO OF THE GAME**
def_startGame()
  dead=0:noteDelay=0:noteIndex=1
  repeat
    _newShape():_checkShape(mo,ss,sr,f)
	forceDrop=0
    if f
      dead=1
	  ; disable sprites, leaving imprint on screen
	  sprinit
    else
      stop=0
      reset dropTimer:reset moveTimer
      repeat
		om=mo:or=sr
        if elapsed(moveTimer)>moveSpeed
          reset moveTimer
          if stick()&1:_checkShape(mo-1,ss,sr,f)
		    if f==0:mo=mo-1:sprX=sprX-1:endif
          elif stick()&2:_checkShape(mo+1,ss,sr,f)
		    if f==0:mo=mo+1:sprX=sprX+1:endif
		  endif
          if stick()&8
           forceDrop=1
          elif stick()&16
            if fire==0
              fire=1:tr=sr+1:if tr>4:tr=1:endif
			  _checkShape(mo,ss,tr,f)
			  if f==0:sr=tr:endif
            endif
          else
            fire=0
          endif
        endif
        if forceDrop|(elapsed(dropTimer)>dropSpeed)
          reset dropTimer
          _checkShape(mo+mapSizeX,ss,sr,f)
          if f==0
		    mo=mo+mapSizeX:sprY=sprY+1
          else
		    stop=1
          endif
        endif
		if (om<>mo)|(or<>sr):_plotShape(sprX,sprY,ss,sr):endif
      until stop
      _stampShape(mo,ss,sr)
	  _checkLineFull()
    endif
  until dead
 _checkTop()
enddef
;
;** Set messages colour **
def_setAttr(flash,colour,rowStart,rowEnd)
  for i=rowStart,rowEnd,1
    plot topX-2,i,flash:plot topX-1,i,colour
  next
enddef
;
def_scrollMsg()
  if elapsed(msgTimer)>msgDelay
    plot 2,27,mid(message$,msgIdx,36)
    msgIdx=msgIdx+1:if msgIdx>msgLen:msgIdx=1:endif
    reset msgTimer
  endif
enddef
;
;** PLAY MUSIC **
def_playTune()
  if elapsed(musicTimer)>noteDelay
    reset musicTimer:noteDelay=delay[noteIndex]*2
	if(musicFlag)
      music 1,oct[noteIndex],note[noteIndex],0:play 1,0,1,1000
      noteIndex=noteIndex+1:if note[noteIndex]==999:noteIndex=1:endif
	endif
  endif
enddef
;
;** START A GAME **
def_start()
  paper 0:ink 7:cursor 1:text:cls
  plot 14,13,"PLEASE  WAIT":plot 14,14,"INITIALISING"
  _init():cls
  _drawScreen():_initGameMap():_drawGamePanel():_showNext(1)
  repeat
    _setAttr(8,2,21,23)
    plot topX,21,"PRESS FIRE"
    plot topX,22," TO START "
    reset musicTimer:noteDelay=0:noteIndex=1
    repeat
      if musicFlag
        plot topX,23,"MUSIC: ON "
      else
        plot topX,23,"MUSIC: OFF"
      endif
      if stick()&1:musicFlag=0:endif
      if stick()&2:musicFlag=1:endif
      _playTune()
	  _scrollMsg()
    until stick()&16
	plot topX,21,"          "
	plot topX,22,"          "
	plot topX,23,"          "
	plot topX,24,"          "
	plot 2,27,"                                    "
    score=0:level=1:dropSpeed=20:moveSpeed=1:lines=0
    plot 10,0,"    ":plot 23,0,"   ":plot 35,0,"   "
    plot 10,1,"    ":plot 23,1,"   ":plot 35,1,"   "
    _drawStatusLine()
    ; Clear Stats Line
    for i=statY,statY+12,1:plot statX,i,statBlank$:next
    _initStats():_initGameMap()
	_drawGamePanel()
    ns=rnd(peek(5))\7+1
    _startGame()
  until 0
enddef
;
;** INIT STATS TABLE **
def_initStats()
  for i=1,7,1
    stat[i]=0
  next
enddef
;
;** KILL A LINE **
def_killLine(l)
  local x,c
  sound 0,20,0:sound 1,300,0:play 1,1,1,1000
  for l=l,3,-1
    lineCount[l]=lineCount[l-1]
    gameMap$[l]=gameMap$[l-1]
    plot gameX-1,gameY+l-2,gameMap$[l-1]
  next
  lineCount[2]=0
  gameMap$[2]=blankLine$:plot gameX-1,gameY,gameMap$[2]
enddef
;
;** CHECK LINE FULL **
def_checkLineFull()
  local x,y,count,bonus
  y=sizeY+1:bonus=1
  while y>1
    if lineCount[y]==sizeX
      _killLine(y):score=score+((sizeY+2-y)*bonus):bonus=bonus+1:lines=lines+1
      if (lines\10)==0:level=level+1
        if dropSpeed>0:dropSpeed=dropSpeed*80/100:endif
      endif
      _drawStatusLine()
    else
      y=y-1
    endif
  wend
enddef
;
;** CHECK TOP TABLE **
def_checkTop()
  i=1
  while i<8 & score<topScore[i]
    i=i+1
  wend
  ; if in the top 7 then make space
  if i<=7
    _topScoreSound()
    j=7
    while j>i
      topScore[j]=topScore[j-1]
      topName$[j]=topName$[j-1]
      j=j-1
    wend
    topScore[i]=score
    topName$[i]="   "
    _drawTopScores()
    _enterTopScore(i)
  endif
enddef
;
; Top score sound effect
def_topScoreSound()
  local i
  play 1,0,0,0
  for i=300,50,-2:sound 1,i,15:next
  for i=50,300,2:sound 1,i,15:next
  play 0,0,0,0
enddef
;
; Enter initials in to top score table
def_enterTopScore(i)
  local y,pos,char
  pos=1:char=32:y=i*2+topY-1:reset timer
  tempName[1]=32:tempName[2]=32:tempName[3]=32
  _setAttr(12,3,21,23)
  plot topX,21,"  ENTER  ":plot topX,22,"INITIALS!"
  repeat
    if elapsed(timer) & 32
      plot topX+pos,y,solid
    else
      plot topX+pos,y,tempName[pos]
    endif
    if elapsed(timer) & 16
      if stick()
        reset timer
        plot topX+pos,y,tempName[pos]
        if stick()&1 & pos>1:pos=pos-1:endif
        if stick()&2 & pos<3:pos=pos+1:endif
        if stick()&4
          tempName[pos]=tempName[pos]-1
          if tempName[pos]==31:tempName[pos]=90:endif
          if tempName[pos]==64:tempName[pos]=32:endif
        endif
        if stick()&8
          tempName[pos]=tempName[pos]+1
          if tempName[pos]==91:tempName[pos]=32:endif
          if tempName[pos]==33:tempName[pos]=65:endif
        endif
      endif
    endif
  until stick()&16
  while stick()&16:wend
  plot topX,21,"         ":plot topX,22,"         "
  topName$[i]=chr(tempName[1])+chr(tempName[2])+chr(tempName[3])
  plot topX+1,y,topName$[i]
  plot topX,topY+21,"         ":plot topX,topY+22,"         "
enddef
;
;** INITIALISE **
def_init()
  score=0:lines=0:level=1
  nextX=6:nextY=5:gameX=14:gameY=4:sizeX=10:sizeY=20
  statX=3:statY=11:topX=28:topY=5
  offX=gameX-2:offY=gameY-2
  mapSizeX=sizeX+3:mapSizeY=sizeY+2
  borderBL=125:borderBR=126+128:borderTL=123:borderTR=124+128:borderSolid=23
  blank=16:solid=127
  ;
  dim gameMap$[mapSizeX,mapSizeY],lineCount[mapSizeY]
  mapBase=gameMap$-1-mapSizeX
  screenBase=0xbb80:screenWidth=40
  dim blankLine$[mapSizeX],fillLine$[mapSizeX]
  dim nextBlank$[5],statBlank$[8],topBlank$[11]
  ;  
  blankLine$=chr(borderSolid)
  for i=1,sizeX,1:blankLine$=blankLine$+chr(blank):next
  blankLine$=blankLine$+chr(borderSolid)
  nextBlank$=""
  for i=1,4,1:nextBlank$=nextBlank$+chr(blank):next
  statBlank$=""
  for i=1,7,1:statBlank$=statBlank$+chr(blank):next
  topBlank$=""
  for i=1,10,1:topBlank$=statBlank$+chr(32):next
  fillLine$="************"
  dim shapeP[7]
  dim sprX0[7,4],sprX1[7,4],sprX2[7,4],sprX3[7,4]
  dim sprY0[7,4],sprY1[7,4],sprY2[7,4],sprY3[7,4]
  dim mapOff0[7,4],mapOff1[7,4],mapOff2[7,4],mapOff3[7,4]
  dim stat[7],topName$[4,7],topScore[7],tempName[3]
  for s=1,7,1
    topName$[s]="ORI":topScore[s]=(8-s)*100
    read shapeP[s]
    for r=1,4,1
	  read ox,oy
	  sprX0[s,r]=ox:sprY0[s,r]=oy
	  mapOff0[s,r]=ox+oy*mapSizeX
	  read ox,oy
	  sprX1[s,r]=ox:sprY1[s,r]=oy
	  mapOff1[s,r]=ox+oy*mapSizeX
	  read ox,oy
	  sprX2[s,r]=ox:sprY2[s,r]=oy
	  mapOff2[s,r]=ox+oy*mapSizeX
	  read ox,oy
	  sprX3[s,r]=ox:sprY3[s,r]=oy
	  mapOff3[s,r]=ox+oy*mapSizeX
    next
  next
  ; Custom graphics definitions
  repeat
    read s
    if s<>0
      for r=0,7,1
        read d
        poke 0xb400+(s*8)+r,d
      next
    endif
  until s==0
  ; Load music data
  dim oct[120],note[120],delay[120]
  noteIndex=1
  repeat
    read note[noteIndex],delay[noteIndex]
	oct[noteIndex]=2
	if (note[noteIndex]<>999)&(note[noteIndex]>11)
		note[noteIndex]=note[noteIndex]-12
		oct[noteIndex]=oct[noteIndex]+1
	endif
    noteIndex=noteIndex+1
  until note[noteIndex-1]==999
  musicFlag=0
  ; Scroll message
  dim message$[255]
  message$=           "                                    "
  message$=message$ + "Tetris by @6502Nerd, programmed in d"
  message$=message$ + "flat, a custom BASIC for the Oric-1 "
  message$=message$ + "and Atmos micromputers.        Speci"
  message$=message$ + "al hello to the lovely Twitter retro"
  message$=message$ + " community..thanks for your support!"
  message$=message$ + "                                    "
  msgLen=len(message$)-36
  msgIdx=1:msgDelay=4:reset msgTimer
enddef
;shape data
;shape 1: long bar (red)
data 17
data 0,0,1,0,2,0,3,0
data 0,0,0,1,0,2,0,3
data 0,0,1,0,2,0,3,0
data 0,0,0,1,0,2,0,3
;shape 2: square (green)
data 18
data 0,0,1,0,0,1,1,1
data 0,0,1,0,0,1,1,1
data 0,0,1,0,0,1,1,1
data 0,0,1,0,0,1,1,1
;shape 3: L (yellow)
data 19
data 0,0,1,0,2,0,0,1
data 0,0,1,0,1,1,1,2
data 2,0,0,1,1,1,2,1
data 0,0,0,1,0,2,1,2
;shape 3: reverse L (blue)
data 20
data 0,0,0,1,1,1,2,1
data 0,0,0,1,0,2,1,0
data 0,0,1,0,2,0,2,1
data 1,0,1,1,1,2,0,2
;shape 4: S (cyan)
data 21
data 1,0,2,0,0,1,1,1
data 0,0,0,1,1,1,1,2
data 1,0,2,0,0,1,1,1
data 0,0,0,1,1,1,1,2
;shape 5: Z (magenta)
data 22
data 0,0,1,0,1,1,2,1
data 1,0,1,1,0,1,0,2
data 0,0,1,0,1,1,2,1
data 1,0,1,1,0,1,0,2
;shape 6: T (red)
data 17
data 0,0,1,0,2,0,1,1
data 1,0,1,1,1,2,0,1
data 1,0,0,1,1,1,2,1
data 0,0,0,1,0,2,1,1
;custom graphics
;Char 33-37
;top left round
data 123
data 0b00000111
data 0b00011111
data 0b00011111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
;top right round
data 124
data 0b00000111
data 0b00000001
data 0b00000001
data 0b00000000
data 0b00000000
data 0b00000000
data 0b00000000
data 0b00000000
;bottom left round
data 125
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00011111
data 0b00011111
data 0b00000111
;bottom right round
data 126
data 0b00000000
data 0b00000000
data 0b00000000
data 0b00000000
data 0b00000000
data 0b00000001
data 0b00000001
data 0b00000111
; solid block
data 127
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
data 0b00111111
; end of UDG
data 0
;
;
data 13,4, 11,4, 13,4, 8,4, 4,2, 8,6, 1, 8
data 13,4, 11,4, 13,4, 8,4, 4,2, 8,6, 1, 8
data 13,4, 15,4, 16,4, 15,2, 16,6, 13,2, 15,6, 13,2, 15,6
data 11,4, 13,4, 11,2, 13,6, 9,4, 13,8
data 13,4, 11,4, 13,4, 8,4, 4,4, 8, 4, 1, 8
data 13,4, 11,4, 13,4, 8,4, 4,4, 8, 4, 1, 8
data 13,4, 15,4, 16,4, 15,4, 16,4, 13,4, 15,4, 13,4, 15,4
data 11,4, 13,4, 11,4, 13,4, 15,4, 16,8
data 20,4, 18,4, 20,4, 16,4, 11,4, 16,4, 8,8
data 20,4, 18,4, 20,4, 16,4, 11,4, 16,4, 8,8
data 20,4, 22,4, 23,4, 22,4, 23,4, 20,4, 22,4, 20,4, 22,4
data 18,4, 20,4, 18,4, 20,4, 16,4, 20,8
data 20,4, 18,4, 20,4, 16,4, 11,4, 16,4, 8,8
data 20,4, 18,4, 20,4, 16,4, 11,4, 16,4, 8,8
data 20,4, 22,4, 23,4, 22,4, 23,4, 20,4, 22,4, 20,4, 22,4
data 18,4, 20,4, 18,4, 15,4, 18,4, 20,8
data 999,0
_start()
