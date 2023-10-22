;
; Sprite Generator
def_generateSprites()
 ; 
 poke spriteMask,0xff
 ; Set up address pointer in sprite table 
 sprTable[1]=sprShip
 sprTable[2]=sprAlien8:sprTable[3]=sprAlien7:sprTable[4]=sprAlien6
 sprTable[5]=sprAlien5:sprTable[6]=sprAlien4:sprTable[7]=sprAlien4
 sprTable[8]=sprAlien3:sprTable[9]=sprAlien3:sprTable[10]=sprAlien2
 sprTable[11]=sprAlien2:sprTable[12]=sprAlien2:sprTable[13]=sprAlien1
 sprTable[14]=sprAlien1:sprTable[15]=sprAlien1
 sprTable[16]=sprBullet6:sprTable[17]=sprBullet6:sprTable[18]=sprBullet5
 sprTable[19]=sprBullet5:sprTable[20]=sprBullet5:sprTable[21]=sprBullet4
 sprTable[22]=sprBullet4:sprTable[23]=sprBullet4:sprTable[24]=sprBullet3
 sprTable[25]=sprBullet3:sprTable[26]=sprBullet2:sprTable[27]=sprBullet2
 sprTable[28]=sprBullet1:sprTable[29]=sprBullet1
 _processSpriteData(sprShip)
 _processSpriteData(sprAlien1):_processSpriteData(sprAlien2)
 _processSpriteData(sprAlien3):_processSpriteData(sprAlien4)
 _processSpriteData(sprAlien5):_processSpriteData(sprAlien6)
 _processSpriteData(sprAlien7):_processSpriteData(sprAlien8)
 _processSpriteData(sprBullet1):_processSpriteData(sprBullet2)
 _processSpriteData(sprBullet3):_processSpriteData(sprBullet4)
 _processSpriteData(sprBullet5):_processSpriteData(sprBullet6)
 _doUDG()
enddef
;
def_processSpriteData(spr)
 sprBase=spr
 read w,h,s
 poke sprBase, w:poke sprBase+1, h:poke sprBase+2, s
 read x1,y1,x2,y2
 poke sprBase+3,x1:poke sprBase+4,y1:poke sprBase+5,x2:poke sprBase+6,y2
 sprBase=sprBase+3+4+12
 ; h rows
 for j=1,h,1
  read v:poke sprBase,v
  read d$
  t=call(dataConvert,addr(d$)>>8,addr(d$)&0xff,0)
  for i=1,w,1
   t$="0b"+mid(d$,i*6-5,6)
   poke sprBase+i,val(t$)|0x40
  next
  sprBase=sprBase+w+1
 next
 t=call(makeSprite,spr>>8,spr&0xff,0)
enddef
;
; Make character graphics
def_doUDG()
 local c,d,i
 repeat
  read c
  if c<>0
   for i=0xb400+8*c,0xb400+8*c+7,1
    read d
    poke i,d
   next
  endif
 until c==0
enddef
;
;
; Data section
; Each sprite:
;  width, height, size (inc attribute)
;  box left, box top, box right, box bottom
; Total space = 7+12+size*6
;Total = 379
data 4, 12, 5*12
data 1,1,16,6
data 7, "        **              "
data 7, "  *    *  *    *        "
data 7, " *    * ** *    *       "
data 7, " *   *  **  *   *       "
data 7, "***  ********  ***      "
data 4, "* **** *  * **** *      "
data 7, "*   **********   *      "
data 7, "       ****             "
data 0, "                        "
data 0, " *    ******    *       "
data 0, "******************      "
data 0, "*    ********    *      "
;
;Total = 451
data 5, 12, 6*12
data 0,0,23,7
data 1, "         ******               "
data 1, "      ************            "
data 1, "     ** * **** * **           "
data 4, "  ********************        "
data 4, " ** * * * * * * * * * *       "
data 4, "******** * * * * *******      "
data 3, "      ************            "
data 3, "        ********              "
data 0, "                              "
data 0, "       **********             "
data 0, "************************      "
data 0, "         ******               "
;
;Total = 415
data 5, 11, 6*11
data 0,0,21,7
data 1, "        ******                "
data 1, "      **********              "
data 1, "     * * **** * *             "
data 4, "  ******************          "
data 4, " * * * * * * * * * **         "
data 4, "**********************        "
data 3, "      **********              "
data 3, "        ******                "
data 0, "                              "
data 0, " ********************         "
data 0, "       ********               "
;
;Total = 289
data 4, 9, 5*9
data 0,0,17,5
data 1, "      ******            "
data 1, "    ** *  * **          "
data 4, "  **************        "
data 4, " * * * * * * * **       "
data 4, "******************      "
data 3, "      ******            "
data 0, "                        "
data 0, " ****************       "
data 0, "      ******            "
;
;Total = 259
data 4, 8, 5*8
data 0,0,15,4
data 1, "      ****              "
data 1, "   *** ** ***           "
data 4, " ** * *  * * **         "
data 4, "****************        "
data 3, "     ******             "
data 0, "                        "
data 0, " **************         "
data 0, "     ******             "
;
;Total = 163
data 3, 6, 4*6
data 0,0,11,3
data 1, "    ****          "
data 1, "  ** ** **        "
data 4, "*** *  * ***      "
data 3, "   ******         "
data 0, "                  "
data 0, " **********       "
;
;Total = 115
data 3, 4, 4*4
data 0,0,7,2
data 1, "  ****            "
data 4, "** ** **          "
data 3, "  ****            "
data 0, " ******           "
;
;Total = 73
data 2, 3, 3*3
data 0,0,5,2
data 1, " ****       "
data 4, "* ** *      "
data 3, " ****       "
;
;Total = 55
data 2, 2, 3*2
data 0,0,3,1
data 1, " **         "
data 4, "****        "
;
;Total = 127
data 2,6,3*6
data 0,0,1,5
data 1, "**          "
data 1, "**          "
data 1, "**          "
data 1, "**          "
data 1, "**          "
data 1, "**          "
;
;Total = 109
data 2,5,3*5
data 0,0,1,4
data 1, "**          "
data 1, "**          "
data 1, "**          "
data 1, "**          "
data 1, "**          "
;
;Total = 91
data 2,4,3*4
data 0,0,1,3
data 1, "**          "
data 1, "**          "
data 1, "**          "
data 1, "**          "
;
;Total = 73
data 2,3,3*3
data 0,0,0,2
data 1, "*          "
data 1, "*          "
data 1, "*          "
;
;Total = 55
data 2,2,3*2
data 0,0,0,1
data 1, "*          "
data 1, "*          "
;
;Total = 37
data 2,1,3*1
data 0,0,0,0
data 1, "*          "
;
;UDG data char code, then 8 data bytes
;zero char code terminates
data '{'
data 0b00001000
data 0b00001000
data 0b00011100
data 0b00011100
data 0b00111110
data 0b00111110
data 0b00011100
data 0b00000000
data '}'
data 0b00100010
data 0b00011100
data 0b00101010
data 0b00111110
data 0b00001000
data 0b00010100
data 0b00100010
data 0b00000000
data 0
;
; Program starts here
def_start()
 reset t:t=rnd(t)
 _init()
 state=0
 repeat
  if state==stateInstruct:_instruct1(0):endif
  if state==stateAttract:_attract():endif
  if state==stateInitGame:_initGame():endif
  if state==statePlayGame:_playGame():endif
  if state==stateNextLevel:_nextLevel():endif
  if state==stateLoseLife:_loseLife():endif
 until 0
enddef
;
def_init()
 stateAttract=0
 stateInitGame=1
 statePlayGame=2
 stateNextLevel=3
 stateLoseLife=4
 stateInstruct=5
 scoreAddress=0xbf68+40*0+8
 livesAddress=0xbf68+40*0+20
 enemiesBar=0xbf68+40*1+24
 base=0xa000+199*40
 horizon=80
 perspective=4
 soundfx=1
 leftMargin=10:rightMargin=210
 ; Names of systems
 dim systemName$[40,9]
 ; 2400 bytes for code
; dim code[100,10]
 dim sprTable[40]
 dim sprShip[5*12*3+10]
 dim sprAlien1[6*12*3+10], sprAlien2[6*11*3+10]
 dim sprAlien3[5*9*3+10], sprAlien4[5*8*3+10]
 dim sprAlien5[4*6*3+10], sprAlien6[4*4*3+10]
 dim sprAlien7[3*3*3+10], sprAlien8[3*2*3+10]
 dim sprBullet1[3*6*3+10], sprBullet2[3*5*3+10]
 dim sprBullet3[3*4*3+10], sprBullet4[3*3*3+10]
 dim sprBullet5[3*2*3+10], sprBullet6[3*1*3+10]
 ; Temp strings
 dim d$[50],t$[50]
;
 hiScore=0
 systemName$[1]="Alpha System"
 systemName$[2]="Beta System"
 systemName$[3]="Gamma System"
 systemName$[4]="Delta System"
 systemName$[5]="Epsilon System"
 systemName$[6]="Zeta System"
 systemName$[7]="Eta System"
 systemName$[8]="Theta System"
 systemName$[9]="Omega System"
 _instruct1(1)
enddef
;
; First instruction page
def_instruct1(init)
  paper 0:ink 7:cursor 1:text
  _dplot("GALAXY DEFENDER 3023",0,1,10,1,3)
  _dplot("You are the last defender of our    ",4,0,8,7,7)
  _dplot("Milky Way galaxy, all our comrades  ",5,0,8,7,7)
  _dplot("have fallen to the evil Andromeda   ",6,0,8,7,7)
  _dplot("empire.                             ",7,0,8,7,7)
  _dplot("Incoming intelligence..",9,0,12,1,1)
  if init:_asm(0):else:wait 100:endif
  _dplot("Latest intelligence indicates that  ",9,0,8,7,7)
  _dplot("the Andromeda hordes are now at the ",10,0,8,7,7)
  _dplot("outer systems of our galaxy in full ",11,0,8,7,7)
  _dplot("annhilation mode. Billions of souls ",12,0,8,7,7)
  _dplot("are at risk and you must stop them. ",13,0,8,7,7)
  _dplot("Preparing your ship..",15,0,12,1,1)
  if init:_asm(0):else:wait 100:endif
  if init:_asm(2):else:wait 100:endif
  _dplot("Your ship is the latest Sabre class ",15,0,8,7,7)
  _dplot("and equipped with pulse torpedoes.  ",16,0,8,7,7)
  _dplot("To halt the enemy advance, you must ",17,0,8,7,7)
  _dplot("destroy at least 10 of their ships. ",18,0,8,7,7)
  _dplot("This will cause them to change their",19,0,8,7,7)
  _dplot("attack vector to another system and ",20,0,8,7,7)
  _dplot("you must follow them relentlessly.  ",21,0,8,7,7)
  _dplot("Control your ship using left+right &",22,0,8,7,7)
  _dplot("fire torpedoes with space.          ",23,0,8,7,7)
  _dplot("Loading weapons..",26,1,14,4,6)
  if init:_generateSprites():else:wait 100:endif
  _dplot("  PRESS ANY KEY!  ",26,1,14,7,3)
  k=get(1):hires
  state=stateAttract
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
def_drawScreen(l)
 local cx,cy,ax,bx,col
 cx=rnd(0)\(240-100-20)+62:cy=rnd(0)\(horizon-40)+60
 ax=cx-56:bx=cx+56
 pixmode 1
 for j=horizon,199,8:plot 0,j,16:next
 for j=0,horizon-1,2:poke 0xa000+40*j,16+((l)\3+4):next
 for j=0,50,1:point rnd(0)\228+6,rnd(0)\(horizon-5):next
 col=(l-1)\7+1
 for j=cy-50,cy+50,1
  if j<horizon
   plot ax,j,col:plot bx,j,7
  else
   plot ax,j,0
  endif
 next
 for j=1,50,1
  circle cx,cy,j
 next
 pixmode 0
 for j=1,18,2
  circle (cx+15)-(18-j)/3,(cy-25)+(18-j)/3,j
 next
 for j=horizon,cy+50,1:line cx-50,j,cx+50,j:next
 startOffset=0
 startColour=16+2:altColour=(16+6)^startColour
enddef
;
def_drawPanel()
  local i
  cls
  poke 0xbf68+40*0+1,4
  printat 2,0,"SCORE"
  poke scoreAddress-1,2
  printat 8,0,hex(score)
  poke 0xbf68+40*0+13,4
  printat 14,0,"LIVES"
  poke livesAddress-1,5
  for i=0,lives-1,1
   poke livesAddress+i,'{'
  next
  poke 0xbf68+40*0+27,4
  printat 28,0,"HISCORE"
  printat 36,0,hex(hiScore)
  poke 0xbf68+40*0+35,2
  poke 0xbf68+40*1+5,4
  printat 6,1,"ENEMIES REMAINING"
  poke enemiesBar-1,1
  for i=0,aliensLeft-1,1
   poke enemiesBar+i,'}'
  next
enddef
;
def_attract()
 paper 0:ink 4:hires:cursor 1
 _drawScreen(8)
 pixmode 1
 plot 30,155,"Press any key to begin battle!"
 plot 36,165,"(Press 'I' for instructions)"
 poke 0xbf68+40*1,14:poke 0xbf68+40*2,14
 poke 0xbf69+40*1,1:poke 0xbf69+40*2,3
 printat 6,1,"} } GALAXY DEFENDER 2023 } }"
 printat 6,2,"} } GALAXY DEFENDER 2023 } }"
; println hex(mem(0))," ",hex(mem(1))," ",hex(mem(2))," ",endAsm-startAsm
 repeat
  k=get(0):t=call(initLand,0,0,0):wait 2
 until k
 if (k=='q'):abort:endif
 if (k=='i'):state=stateInstruct:else:state=stateInitGame:endif
enddef
;
;
def_initGame()
  level=1
  startOffset=0
  startColour=16+5:altColour=(16+6)^startColour
  doke xPos,128<<8:doke yPos,185<<8
  lives=5
  score=0
  state=statePlayGame
enddef
;
def_playGame()
  doke xStep,32:doke yStep,32:doke friction,6
  doke bulletY,0
  poke alienLevelSpeed,(10-level)+1
  aliensLeft=10
  paper 0:ink 4:hires:cursor 1
  _drawPanel()
  _drawScreen(level)
  pixmode 1:z=len(systemName$[level])
  plot 120-3*z,0,systemName$[level]
  repeat
    play 4+2+1,4+1,0,0:sound 1,0,0:sound 2,0,0:sound 3,500,0:sound 0,30,0
    state=statePlayGame
    doke dx,0:doke dy,0
    poke spriteMask,0xff
    t=call(drawSprite,0,peek(xPos+1),peek(yPos+1))
    poke alien2Delay,(10-level)+2
    t=call(loopLand,0,0,0)
    if state==stateLoseLife
     _loseLife()
    endif
  until (state==stateNextLevel)|(lives==0)
  play 0,0,0,0
  if lives==0
   printat 2,2, " Bad luck pilot, the hordes got you!!"
   wait 100
   if score>hiScore
    printat 2,2," ** But you got the high score **    "
    hiScore=score
    printat 36,0,hex(hiScore)
    poke 0xbf68+40*0+27,12
    wait 100
   endif
   wait 50
   state=stateAttract
  endif
enddef
;
def_loseLife()
 local i,m
 poke livesAddress+lives,32
 printat 2,2,"   } } AN INVADER GOT THROUGH! } }   "
 poke spriteMask,0xff
 t=call(drawSprite,0,peek(xPos+1),peek(yPos+1))
 sound 0,63,0:sound 1,3000,0:play 1,1,0,6000
 for i=0,40,1
  poke 0xbfb9,(i\5)+3
  t=call(invertPaper,0x07,0,0)
  wait 3
 next
 t=call(invertPaper,0x07,0,0)
enddef
;
def_nextLevel()
 sound 1,400,0:sound 2,401,0:sound 3,402,0
 play 7,0,0,6000
 paper 5:ink 3:cls
 printat 2,0," Well done you, thwarted the hordes! "
 if level<9
  printat 2,1,"But the next system is under attack! "
  level=level+1
 else
  printat 2,1,"      But they have re-grouped!      "
 endif
 wait 75
 printat 2,2,"** The galaxy is depending on you ** "
 wait 100
 state=statePlayGame
enddef
;
def_asm(o)
 ztmp24=0x3d
 pointSetup=0xc000+0x0c*3
 kbstick=0xc000+0x09*3
 sndset=0xc000+0x08*3
;
 .opt o
.org deek(0x86):;Object code writes over dflat asm area!
.startAsm
; Hires bit mask to bit position look up
; 1=pos 5*2+7, 2=pos 4*2+7, 4=pos 3*2+7
; 8=pos 2*2+7,16=pos 1*2+7,32=pos 0*2+7
.maskLookUp
.db  0,17,15,0, 13, 0, 0, 0
.db  11,0, 0, 0, 0, 0, 0, 0
.db  9, 0, 0, 0, 0, 0, 0, 0
.db  0, 0, 0, 0, 0, 0, 0, 0
.db  7, 0, 0, 0, 0, 0, 0, 0
;
;Message handling
.msgSpeed:.db 6
.msgCount:.db 0
.msgIdx:.db 0
.msg
.db "Galaxy Defender 3023, written in dflat a custom language for "
.db "the Oric-1 & Atmos computers. Created by @6502nerd :-)       ",0
;
; Game Variables
.makeW:.db 0
.makeH:.db 0
.makeS:.db 0
.spriteMask:.db 0
.frameCount:.db 0
.heightCount:.db 0
.alienLevelSpeed:.db 0
.alienSpeed:.db 0
.alienSpeedCount:.db 0
.alienIdx:.db 0
.alien2Delay:.db 0
.alien2DelayCount:.db 0
.alienDX:.dw 0:.dw 0:.dw 0
.alienDY:.dw 0:.dw 0:.dw 0
.alienX:.dw 0:.dw 0:.dw 0
.alienY:.dw 0:.dw 0:.dw 0
.lastAlienFrame:.dw 0:.dw 0:.dw 0
.explodeX:.dw 0:.dw 0:.dw 0
.explodeY:.dw 0:.dw 0:.dw 0
.explodeVol:.dw 0:.dw 0:.dw 0
.explodeFrame:.dw 0:.dw 0:.dw 0
.xPos:.dw 0
.yPos:.dw 0
.dx:.dw 0
.dy:.dw 0
.xStep:.dw 0
.yStep:.dw 0
.friction:.dw 0
.ox:.dw 0
.oy:.dw 0
.bulletX:.dw 0
.bulletY:.dw 0
.bulletDX:.dw 0
.bulletDY:.dw 0
.lastBulletFrame:.dw 0
;
; A=xor mask
.invertPaper
 pha
 lda #0:sta invertLineEOR+1:sta invertLineSTA+1
 lda #0xa0:sta invertLineEOR+2:sta invertLineSTA+2
.invertCounter:ldy #200
.invertLine:pla:pha
.invertLineEOR:eor 0x1234:; Self modifying code
.invertLineSTA:sta 0x1234:; Self modifying code
 clc
 lda invertLineEOR+1:adc #40
 sta invertLineEOR+1:sta invertLineSTA+1
 lda invertLineEOR+2:adc #0
 sta invertLineEOR+2:sta invertLineSTA+2
 dey:bne invertLine
 pla
 rts
;
.initLand
 ; Start painting from bottom
 ldy #199
 lda addr(base):sta drawLineAbs+1
 lda addr(base)+1:sta drawLineAbs+2
 ; Initialise bar size
 jsr calcBarSize:cpx addr(startOffset):bcs skipInitReset
 lda #0:sta addr(startOffset)
 lda addr(startColour):eor addr(altColour):sta addr(startColour)
.skipInitReset
 sec:txa:sbc addr(startOffset):tax
 lda addr(startColour):sta addr(colour)
.drawLand
; tya:and #1:beq doLineColour
; lda addr(colour):eor #4
.doLineColour
 lda addr(colour)
 .drawLineAbs:sta 0x1234:; Self modifying code
 ; Decrement y
 dey
 ; Update line address
 sec
 lda drawLineAbs+1:sbc #40:sta drawLineAbs+1
 lda  drawLineAbs+2:sbc #0:sta  drawLineAbs+2
 ; Decrement bar size counter
 dex:bpl skipReset
 ; Reset current bar size in X
 jsr calcBarSize
 ; Flip colour
 lda addr(colour):eor addr(altColour):sta addr(colour)
.skipReset
 cpy addr(horizon):bcs drawLand
 inc addr(startOffset):inc addr(startOffset):inc addr(startOffset)
 rts
;
; Calculate new bar size (y-reg-horizon+2)/4
.calcBarSize
 sec:tya:sbc addr(horizon)
 clc:adc #2:lsr:lsr:tax
 rts
;
.initLoop
 ; Reset some variables
 lda #0:sta alienIdx:sta bulletY+1:ldx #4
.initAlien
 sta explodeVol,x
 sta alienY+1,x
 dex:dex
 bpl initAlien
 lda alien2Delay:adc #10:sta alien2DelayCount
 ; Alien speed based on level
 lda alienLevelSpeed:sta alienSpeed:sta alienSpeedCount
 ; Score tick is 5*level in BCD
 sed
 clc:lda #0:ldx #5
.mult5
 adc addr(level):dex:bne mult5
 sta addr(scoreTick)
 cld
 rts
;
; do one loop
.loopLand
 jsr initLoop:jsr initMsg:jsr initT2
.gameLoop
 jsr timeoutT2:jsr showMsg:jsr initLand
 jsr bulletUpdate
 jsr alienUpdate
 jsr checkBulletHit
 ; *This needs to be the last routine!*
 jsr playerUpdate
 ; Keep looping unless status has changed from 2
 lda addr(state):cmp #statePlayGame:beq gameLoop
 ; Erase aliens and bullets from screen
 lda #0x40:jsr drawBullet
 ldx #4:stx alienIdx
.eraseAliens
 lda #0x40:jsr drawAlien
 dec alienIdx:dec alienIdx
 bpl eraseAliens
 rts
;
; Play sound using A=joystick (channel A)
.playerSound
 ; Volume register A
 ldx #8:and #3:beq playerSilent
 lda #5:jmp sndset
.playerSilent
 lda #0:jmp sndset
 rts
;
; Play explosion sound (channel C)
.explodeSound
 ldx alienIdx
 lda explodeVol,x:beq explodeSkip
 dec explodeVol,x
 lda explodeVol,x
 ; Volume register C attenuated
 ldx #10:jsr sndset
 cmp #0:beq explodeErase
 and #1:beq explodeFlash
 lda #0b01110011:.db 0x2c
.explodeFlash
 lda #0b01001100
.explodeErase
 sta spriteMask
 ldx alienIdx
 lda explodeFrame,x:pha
 lda explodeX,x:ldy explodeY,x
 tax:pla
 jsr drawSprite
.explodeSkip
 rts
;
; Initialise Timer 2
.initT2
 lda #0x4009&255:sta 0x300+8
 lda #0x4009>>8:sta 0x300+9
 rts
;
; Wait for T2 timeout and re-start
.timeoutT2
 lda 0x300+13:and #0x20:beq timeoutT2
 jmp initT2
; Draw sprite indexed in A at pos x,y
.drawSprite
 grptr=0x11+0x15
 linemask=grptr+2
 lineoffset=0x32
 ztSprPtr=ztmp24+0
 ztWidth=ztmp24+2
 ztHeight=ztmp24+3
 ztSize=ztmp24+4
 pha
 ; Get line base and offset
 jsr pointSetup:sty lineoffset
 ; Get sprite address
 pla:asl:tax
 lda addr(sprTable),x:sta ztSprPtr:;         	+0 = sprite pointer L
 lda addr(sprTable)+1,x:sta ztSprPtr+1:;     	+1 = sprite pointer H
 ; Now save the width, height and size
 ldy #0:lda (ztSprPtr),y:sta ztWidth:;     	+2 = width
 iny:lda (ztSprPtr),y:sta ztHeight:;        	+3 = height
 iny:lda (ztSprPtr),y:sta ztSize:;        	+4 = size
 ; Get sprite position pointer
 ldx linemask:ldy maskLookUp,x
 ; Initialise self-modifying addresses
 lda (ztSprPtr),y:sta source1-2:sta source2-2
 iny
 lda (ztSprPtr),y:sta source1-1:sta source2-1
 ; Set up line base with offset
 clc:lda grptr:adc lineoffset
 sta dest1-2:sta dest2-2
 lda grptr+1:adc #0
 sta dest1-1:sta dest2-1
 ; Number of rows to do
 ldx ztHeight
.nextSpriteRow
 ; Poke from right to left starting at width
 ldy ztWidth
.plotSpriteRow
 lda 0xffff,y:.source1
 and spriteMask
 sta 0xffff,y:.dest1
 dey:bne plotSpriteRow
 ; Now plot the attribute
 lda 0xffff:.source2
 sta 0xffff:.dest2
 ; Increment sprite pointer by width+1
 sec
 lda source1-2:adc ztWidth
 sta source1-2:sta source2-2
 lda source1-1:adc #0
 sta source1-1:sta source2-1
 ; Move screen down by 40
 clc
 lda dest1-2:adc #40
 sta dest1-2:sta dest2-2
 lda dest1-1:adc #0
 sta dest1-1:sta dest2-1
 ; Keep going until all rows done
 dex:bne nextSpriteRow
 rts
;
; Create a hitbox
; x,y = coord, A=sprite #
.makeHitBox
 ; Save x in +2 and +4
 ; Save y in +3 and +5
 stx ztmp24+2:stx ztmp24+4
 sty ztmp24+3:sty ztmp24+5
 ; Index in to sprite table
 asl:tax
 ; Get pointer to sprite
 lda addr(sprTable),x:sta ztmp24:;         	+0 = sprite pointer L
 lda addr(sprTable)+1,x:sta ztmp24+1:;     	+1 = sprite pointer H
 ; Point to left coord add to origin x			+2 = box left
 ldy #3:clc:lda ztmp24+2:adc (ztmp24),y:sta ztmp24+2
 ; Point to top coord add to origin y			+3 = box top
 iny:clc:lda ztmp24+3:adc (ztmp24),y:sta ztmp24+3
 ; Point to right coord add to origin x			+4 = box right +1
 iny:sec:lda ztmp24+4:adc (ztmp24),y:sta ztmp24+4
 ; Point to top coord add to origin y			+5 = box bottom +1
 iny:sec:lda ztmp24+5:adc (ztmp24),y:sta ztmp24+5
 rts
; check x,y against hitbox
; C=1 if inside, C=0 if outside
.checkHitBox
 cpx ztmp24+2:bcc hitBox0
 cpy ztmp24+3:bcc hitBox0
 cpx ztmp24+4:bcs hitBox0
 cpy ztmp24+5:bcs hitBox0
 sec:rts
.hitBox0
 clc:rts
;
;add scoreTick plus whatever in A (y coord of bullet)
.addToScore
; First adjust A to be number 1-9
; ycoord-horizon is range 0-100 ish
; div 8 then if >9 make equal to 9
 sec:sbc #80
 lsr:lsr:lsr:cmp #10
 bcc skipScoreAdj
 lda #9
.skipScoreAdj
 sed
 clc:adc addr(scoreTick):adc addr(score):sta addr(score)
 lda #0:adc addr(score)+1:sta addr(score)+1
 cld
 rts
; 
 .updateScore
 lda addr(score)+1:pha
 lsr:lsr:lsr:lsr
 ora #'0':sta scoreAddress
 pla:and #0xf:ora #'0':sta scoreAddress+1
 lda addr(score):pha
 lsr:lsr:lsr:lsr
 ora #'0':sta scoreAddress+2
 pla:and #0xf:ora #'0':sta scoreAddress+3
 rts
;
;
.initMsg
 lda #8:sta 0xbf68+40*2+0
 lda #3:sta 0xbf68+40*2+1
 lda #5:sta msgSpeed:sta msgCount
 lda #0:sta msgIdx
 rts
;
.showMsg
 dec msgCount:bne msgDone
 lda msgSpeed:sta msgCount
 ldy msgIdx:ldx #0
.showMsgLoop
 lda msg,y:bne writeMsg
 ldy #0:lda msg,y
.writeMsg
 iny
 sta 0xbf68+40*2+2,x
 inx:cpx #36:bne showMsgLoop
 ldy msgIdx:iny
 lda msg,y:bne msgSkipReset
 ldy #0
.msgSkipReset
 sty msgIdx
.msgDone
 rts
;
.playerUpdate
  lda xPos+1:sta ox+1
  lda yPos+1:sta oy+1
  jsr kbstick:pha
  jsr playerSound
  pla:pha:and #1:beq skipLeft
  ; Do left dx only if not -2
  lda dx+1:cmp #-3&0xff:beq dxLowerLimit
  sec:lda dx:sbc xStep:tay
  lda dx+1:sbc xStep+1
  bpl limitMinusDX:cmp #-3&0xff:bcs limitMinusDX
.dxLowerLimit
  lda #-3&0xff:ldy #0x80
.limitMinusDX
  sta dx+1:sty dx
.skipLeft
  pla:pha:and #2:beq skipRight
  ; Do right dx only if not 2
  lda dx+1:cmp #2:beq dxUpperLimit
  clc:lda dx:adc xStep:tay
  lda dx+1:adc xStep+1
  bmi limitPlusDX:cmp #3:bcc limitPlusDX
.dxUpperLimit
  lda #2:ldy #0x80
.limitPlusDX
  sta dx+1:sty dx
.skipRight
  pla:pha:and #16:beq skipFire
  lda bulletY+1:bne skipFire
  jsr startBullet
.skipFire
  ; Do friction in x
  ldy dx+1:bmi frictionXPlus
  sec:lda dx:sbc friction:sta dx
  tya:sbc friction+1:sta dx+1
  bcs frictionY
.frictionXZero:lda #0:sta dx:sta dx+1
  beq frictionY
.frictionXPlus
  clc:lda dx:adc friction:sta dx
  tya:adc friction+1:sta dx+1
  bcs frictionXZero
.frictionY
  ; Add x velocity to position
  clc:lda xPos:adc dx:tya
  lda xPos+1:adc dx+1
  cmp #leftMargin:bcs skipLeftMargin
  lda #leftMargin:ldy #0x80:jsr dxNegate
.skipLeftMargin
  cmp #rightMargin+1:bcc skipRightMargin
  lda #rightMargin:ldy #0x80:jsr dxNegate
.skipRightMargin
  sta xPos+1:sty xPos
  ; Add y velocity to position
  clc:lda yPos:adc dy:sta yPos
  lda yPos+1:adc dy+1
  cmp #100:bcs skipTopMargin:lda #100
.skipTopMargin
  cmp #185:bcc skipBottomMargin:lda #185
.skipBottomMargin
  sta yPos+1
  ; Check old and new positions
  lda xPos+1:cmp ox+1:bne doUpdate
  lda yPos+1:cmp oy+1:bne doUpdate
  ; first get stick status off stack
  pla
  rts
.dxNegate
  pha:sec
  lda #0:sbc dx:sta dx
  lda #0:sbc dx+1:sta dx+1
  pla:rts
.doUpdate
  lda #0x40:sta spriteMask
  ldx ox+1:ldy oy+1:lda #0
  jsr drawSprite
  lda #0xff:sta spriteMask
  ldx xPos+1:ldy yPos+1:lda #0
  jsr drawSprite
  ; first get stick status off stack
  pla
  rts
;
; Calculate bullet pos and vel from bottom
; x=position
.initBulletPos
 stx bulletX+1:stx bulletX:;	Scaled integer x pos - use MSB
 txa:sec:sbc #10:sta bulletDX:; C still set as x >= 10
 lda #(rightMargin-leftMargin)/2:sbc bulletDX:sta bulletDX
 lda #0:sbc #0:sta bulletDX+1:; Scaled integer dx
 lda yPos+1:sec:sbc #6
 sta bulletY+1:; Y is at ship position-6 use MSB
 rts
;
; Start a bullet and show it
.startBullet
 lda xPos+1:adc #8:tax:jsr initBulletPos:tay:; A=Bullet Y pos
 ldx #255:jsr drawBullet
.bulletInactive
 rts
;
; Draw bullet based on mask in A
.drawBullet
 sta spriteMask
 ; Y is vert pos
 ldy bulletY+1:beq bulletInactive
 jsr calcBulletFrame
 ldx bulletX+1:ldy bulletY+1
 jmp drawSprite
;
; Check bullet for hit with each alien
.checkBulletHit
 lda #0:sta alienIdx
.checkAlienHit
 ldx alienIdx
 ldy bulletY+1:beq skipBulletHit
 ldy alienY+1,x:beq skipBulletHit
 lda lastAlienFrame,x:pha
 lda alienX+1,x:tax:pla
 jsr makeHitBox
 ldx bulletX+1:ldy bulletY+1
 jsr checkHitBox
 bcs doBulletHit
 iny:jsr checkHitBox
 bcs doBulletHit
 dey:dey:jsr checkHitBox
 bcc skipBulletHit
.doBulletHit
 ldx alienIdx
 lda alienY+1,x:sta explodeY,x:pha
 lda alienX+1,x:sta explodeX,x
 lda lastAlienFrame,x:sta explodeFrame,x
 lda #0:sta alienY+1,x
 lda #15:sta explodeVol,x
 lda #0x40:jsr drawBullet
 lda bulletY+1:jsr clearBullet
 pla:jsr addToScore:jsr updateScore
 lda #' ':dec addr(aliensLeft)
 ldx addr(aliensLeft):sta enemiesBar,x
 beq levelComplete
.skipBulletHit
 jsr explodeSound
 ldx alienIdx:inx:inx:stx alienIdx
 cpx #6:bne checkAlienHit
 rts
.levelComplete
 lda #stateNextLevel:sta addr(state)
 rts
 ;
 ;
.bulletUpdate
 ldy bulletY+1:beq skipBulletUpdate
 lda #0x40:jsr drawBullet
 lda bulletY+1:jsr calcDY:sty bulletDY+1
 jsr moveBulletUp
 ldy addr(horizon):cpy bulletY+1:bcs clearBullet
 lda #0xff:jsr drawBullet:jmp bulletSound
.clearBullet
 lda #0:sta bulletY+1
 jmp bulletSilent
.skipBulletUpdate
 rts
; Play laser sound using bullet Y pos (channel B)
.bulletSound
 lda bulletY+1:sec:lda #255:sbc bulletY+1
 ; Fine freq register B
 ldx #2:jsr sndset
 ; Volume register B attenuated
 ldx #9:lda bulletY+1:lsr:lsr:lsr:lsr:jmp sndset
;
.bulletSilent
 ; Set volume B to zero
 ldx #9:lda #0:jmp sndset
;
; Move bullet by Y rows up, adjusting X position
; y=how many rows
.moveBulletUp
 clc:lda bulletX:adc bulletDX:sta bulletX
 lda bulletX+1:adc bulletDX+1:sta bulletX+1
 dec bulletY+1:dey:bne moveBulletUp
 rts
;
; Calculate bullet frame, Y=coordinate
; output A=frame index
.calcBulletFrame
 jsr calcSpriteFrame
 clc:adc #15:sta lastBulletFrame
 rts
;
; Calculate bullet pos and vel from bottom
; x=position
.initAlienPos
 txa:ldx alienIdx
 sta alienX+1,x:sta alienX,x:   ;	Scaled integer x pos - use MSB
 sec:sbc #120:sta alienDX,x:    ; x-midpoint
 lda #0:sbc #0:sta alienDX+1,x:; Scaled integer dx
 asl alienDX,x:rol alienDX+1,x:; dx=dx*2
 asl alienDX,x:rol alienDX+1,x:; dx=dx*2
 asl alienDX,x:rol alienDX+1,x:; dx=dx*2
 lda addr(horizon):sta alienY+1,x:; Y is at horizon use MSB
 rts
;
; Draw alien based on mask in A. If y=0 then do nothing
.drawAlien
  sta spriteMask
  ldx alienIdx
  ldy alienY+1,x:beq alienInactive
  lda alienX+1,x:sta drawAlienTmp
  jsr calcSpriteFrame
  clc:adc #1:ldx alienIdx:sta lastAlienFrame,x
  ldx drawAlienTmp:jmp drawSprite
.alienInactive
  rts
.drawAlienTmp:.db 0
;
.alienUpdate
 ; Only move alien based on speed counter
 dec alienSpeedCount:bne skipAlienMove
 lda alienSpeed:sta alienSpeedCount
 ldy alien2DelayCount:beq skipAlien2Dec
 dey:sty alien2DelayCount
.skipAlien2Dec
 lda #0x40:jsr drawAllAliens
 lda #4:sta alienIdx
.move1Alien
 jsr moveAlienDown:jsr spawnAlien
 dec alienIdx:dec alienIdx
 bpl move1Alien
.skipAlienMove
 lda #0xff:jsr drawAllAliens
 rts
;
; A=sprite mask
.drawAllAliens
 pha
 lda #0:sta alienIdx:pla:pha:jsr drawAlien
 lda #2:sta alienIdx:pla:pha:jsr drawAlien
 lda #4:sta alienIdx:pla:pha:jsr drawAlien
 pla
 rts
;
.spawnAlien
 ldx alienIdx:lda alienY+1,x:bne noSpawn
 ldy alien2DelayCount:bne noSpawn
.doSpawn
 ldy alien2Delay:sty alien2DelayCount
 jmp restartAlien
.noSpawn
 rts
;
.restartAlien
  lda 0x08:; LSB of interrupt timer
  and #63:clc:adc #(120-32):tax
  jsr initAlienPos
  lda #0xff:jmp drawAlien  
;
; Move alien by Y rows down, adjusting X position
; y=how many down
.moveAlienDown
 ldx alienIdx
 lda alienY+1,x:beq skipLoseLife:jsr calcDY:tya:sta alienDY+1,x
.alienDown1Row
 clc:lda alienX,x:adc alienDX,x:sta alienX,x
 lda alienX+1,x:adc alienDX+1,x:sta alienX+1,x
 inc alienY+1,x:dey:bne alienDown1Row
 ; bounce check
 lda #24:cmp alienX+1,x:bcs alienBounce
 lda #200:cmp alienX+1,x:bcc alienBounce
 bcs alienAtBottomCheck
 ; Stop alien going off the side!
 ; 2s complement DX
.alienBounce
 lda alienDX+1,x:eor #0xff:sta alienDX+1,x
 lda alienDX,x:eor #0xff
 clc:adc #1:sta alienDX,x
 lda alienDX+1,x:adc #0:sta alienDX+1,x
 ; Bounds check
.alienAtBottomCheck
 lda alienY+1,x:cmp #186:bcc skipLoseLife
 lda #0:sta alienY+1,x:;Disable alien
 ; Lost a life if alien got to bottom!
 lda #' ':dec addr(lives)
 ldx addr(lives):sta livesAddress,x
 lda #stateLoseLife:sta addr(state)
.skipLoseLife
 rts
;
; Calculate Y velocity based on A=Y coordinate
; Output in Y
.calcDY
 sec:sbc addr(horizon):lsr:lsr:lsr:lsr
 tay:iny
 rts
;
; Calculate sprite frame, Y=coordinate
; output A=frame index
.calcSpriteFrame
 tya:sec:sbc addr(horizon):;			y-horizon
 lsr:lsr:lsr:;						/8
 rts
;
; Convert * to 1 and space to 0
; X,A=Low/High of string
.dataConvert
 stx ztmp24:sta ztmp24+1
 ldy #0
.dataConvertLoop
 lda (ztmp24),y:beq dataConvertDone
 cmp #'*':beq dataConvert1
 cmp #'X':beq dataConvert1
 cmp #'1':beq dataConvertDo
 lda #'0':bne dataConvertDo
.dataConvert1
 lda #'1'
.dataConvertDo
 sta (ztmp24),y
 iny:bne dataConvertLoop
.dataConvertDone
 ; Return number of chars
 tya:tax:lda #0
 rts
;
; Create shifted sprite data from first image
; X,A=Low/High of sprite databank start
.makeSprite
 stx ztmp24:sta ztmp24+1
 ldy #0:lda (ztmp24),y:sta makeW
 iny:lda (ztmp24),y:sta makeH
 iny:lda (ztmp24),y:sta makeS
 ; Get to sprite data section
 ; 6 frames, starting at base+3+4+12
 clc:lda ztmp24:adc #19:sta ztmp24+2
 lda ztmp24+1:adc #0:sta ztmp24+3
 ldx #6
 ldy #6
.makeSpriteTable
 iny:lda ztmp24+2:sta (ztmp24),y
 iny:lda ztmp24+3:sta (ztmp24),y
 ; Shift data section by size
 clc:lda ztmp24+2:adc makeS:sta ztmp24+2
 lda ztmp24+3:adc #0:sta ztmp24+3 
 dex:bne makeSpriteTable
 ; Get the destination address (2nd table entry)
 ldy #9:lda (ztmp24),y:tax
 iny:lda (ztmp24),y
 stx ztmp24+2:sta ztmp24+3
 ; Get the source address (1st table entry)
 ldy #7:lda (ztmp24),y:tax
 iny:lda (ztmp24),y
 stx ztmp24:sta ztmp24+1
 ; need to shift a further 5 bits
 lda #5:sta frameCount
.makeSpriteFrame
 ldy #0
 ; Count the rows to do
 ldx makeH:stx heightCount
.makeSpriteRow
 ; Now rotate W bytes of sprite row data
 ldx makeW
 ; First copy the attribute
 lda (ztmp24),y:sta (ztmp24+2),y
 ; Clear carry on first byte
 clc
.makeSpriteRowData
 ; Point to next byte of data
 iny
 ; Get source, only low 6 bits
 lda (ztmp24),y:and #0x3f
 bcc noCarryIn
 ora #0x40
.noCarryIn
 ; shift right in to destination, put back bit 6
 lsr:ora #0x40:sta (ztmp24+2),y
 dex:bne makeSpriteRowData
 iny
 dec heightCount:bne makeSpriteRow
 ; Move source & dest by S bytes
 clc:lda ztmp24:adc makeS:sta ztmp24
 lda ztmp24+1:adc #0:sta ztmp24+1
 clc:lda ztmp24+2:adc makeS:sta ztmp24+2
 lda ztmp24+3:adc #0:sta ztmp24+3 
 ; Until all shifted frames
 dec frameCount:bne makeSpriteFrame
 rts
.endAsm
enddef
