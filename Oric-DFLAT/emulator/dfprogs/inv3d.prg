; Program starts here
def_start()
 hires
 reset t:t=rnd(t)
 _init()
 state=0
 repeat
  if state==0:_attract():endif
  if state==1:_initGame():endif
  if state==2:_playGame():endif
 until state==3
enddef
;
; Allocate storage for asm
; and sprites.
; Generate sprite and asm.
def_init()
;
 base=0xa000+199*40
 horizon=80
 perspective=4
 leftMargin=10:rightMargin=210
 ; 4000 bytes of space for machine code
 dim code[200,10]
 ; Sprite table
 dim sprTable[40]
 ; ship
 dim sprShip[5*12*3+10]
 ; alien
 dim sprAlien1[6*12*3+10], sprAlien2[6*11*3+10]
 dim sprAlien3[5*9*3+10], sprAlien4[5*8*3+10]
 dim sprAlien5[4*6*3+10], sprAlien6[4*4*3+10]
 dim sprAlien7[3*3*3+10], sprAlien8[3*2*3+10]
 ; bullet
 dim sprBullet1[3*6*3+10], sprBullet2[3*5*3+10]
 dim sprBullet3[3*4*3+10], sprBullet4[3*3*3+10]
 dim sprBullet5[3*2*3+10], sprBullet6[3*1*3+10]
 ; Temp strings
 dim d$[50],t$[50]
;
 _instruct1()
 _doAsm()
 _instruct2()
 _generateSprites()
enddef
;
; First instruction page
def_instruct1()
enddef
;
; Second instruction page
def_instruct2()
enddef
;
; Draw game screen
def_drawScreen(l)
 if l==1:l=2
 elif l==2:l=6
 elif l==3:l=3
 elif l==4:l=5
 elif l==5:l=1
 else:l=4
 endif
 paper 0:ink 4:hires:cursor 1
 for j=0,50,1:point rnd(0)\240,rnd(0)\horizon:next
 for j=1,50,1
  circle 140,horizon+10,j
  plot 140-50-5,horizon-j+10-1,l
  plot 140+50+5,horizon-j+10-1,7
 next
 pixmode 0
 for j=horizon,horizon+60,1:line 0,j,239,j:next
enddef
;
def_attract()
  print "Enter speed >=1 (0 = quit) ?":input alienLevelSpeed
 _drawScreen(l)
 startOffset=0
 startColour=16+5:altColour=(16+6)^startColour
 xPos=120<<8:yPos=185<<8:dx=0:dy=0
 xStep=32:yStep=32:friction=8
 t=call(drawSprite,0,xPos>>8,yPos>>8)
 repeat
  println "(In game, press up+down to exit)"
  if alienLevelSpeed==0:abort:endif
  cls
  bulletY=0:bulletYTemp=0
  play 4+2+1,4+1,0,0:sound 1,0,0:sound 2,0,0:sound 3,100,0:sound 0,20,0
  t=call(loopLand,0,rnd(0)\105+54,0)
;  _debug()
 until 0
enddef
;
;
def_initGame()
enddef
;
def_playGame()
enddef
;
def_doAsm()
 println "Assembling.."
 ; Three pass assembler
 ; 0=process silently but do not generate code
 ; 1=process with printout to screen
 ; 2=process silently and generate code
 ; 3=process with printout to screen and generate code
 print "Pass 1 ":_asm(0):println "Done"
 print "Pass 2 ":_asm(0):println "Done"
 print "Pass 3 ":_asm(2):println "Done"
 println "Code size=",endAsm-startAsm
; println "Press any key..":t=get(1)
enddef
;
def_asm(o)
 ztmp24=0x3d
 pointSetup=0xc000+0x0c*3
 kbstick=0xc000+0x09*3
 sndset=0xc000+0x08*3
;
 .opt o
 .org code
.startAsm
.drawLand
 lda addr(colour)
 .drawLineAbs:sta 0x1234:; Self modifying code
 ; Decrement y
 dey
 ; Update line address
 sec
 lda drawLineAbs+1:sbc #40:sta drawLineAbs+1
 lda  drawLineAbs+2:sbc #0:sta  drawLineAbs+2
 ; Decrement bar size counter
 dex
 ; Check if x<0
 bpl skipReset
 ; Reset current bar size in X
 jsr calcBarSize
 ; Flip colour
 lda addr(colour):eor addr(altColour)
 sta addr(colour)
.skipReset
 cpy addr(horizon)
 bcs drawLand
 inc addr(startOffset):inc addr(startOffset)
 rts
;
; Calculate new bar size (y-reg-horizon+2)/4
.calcBarSize
 sec:tya:sbc addr(horizon)
 clc:adc #2:lsr:lsr
 tax
 rts
;
; initialise landscape
.initLand
 ; Start painting from bottom
 ldy #199
 lda addr(base):sta drawLineAbs+1
 lda addr(base)+1:sta drawLineAbs+2
 ; Initialise bar size
 jsr calcBarSize
 cpx addr(startOffset):bcs skipInitReset
 lda #0:sta addr(startOffset)
 lda addr(startColour):eor addr(altColour)
 sta addr(startColour)
.skipInitReset
 sec:txa:sbc addr(startOffset):tax
 lda addr(startColour):sta addr(colour)
 rts
;
; do one loop
.loopLand
; jsr initMsg
 jsr initAlienPos
 jsr initScore
 jsr initT2
.gameLoop
 jsr timeoutT2
; jsr showMsg
 jsr initLand
 jsr drawLand
 jsr bulletUpdate
 jsr bulletSound
 jsr checkBulletHit
 jsr explodeSound
 jsr alienUpdate
 ; *This needs to be the last routine!*
 jsr playerUpdate
 pha
 ; Check stick status returned for up
 pla
 and #4+8:cmp #4+8
 bne gameLoop
 ; Return a known value
 ldx addr(bulletY)+1:lda addr(bulletX)+1
 rts
;
; Play sound using A=joystick (channel A)
.playerSound
 ; Volume register A
 ldx #8
 and #3:beq playerSilent
 lda #5:jmp sndset
.playerSilent
 lda #0:jmp sndset
 rts
;
; Play laser sound using bullet Y pos (channel B)
.bulletSound
 lda addr(bulletY)+1:beq bulletSilent
 sec:lda #255:sbc addr(bulletY)+1
 ; Fine freq register B
 ldx #2:jsr sndset
 ; Volume register B attenuated
 ldx #9:lda addr(bulletY)+1:lsr:lsr:lsr:lsr:jmp sndset
.bulletSilent
 ; Set volume B to zero
 ldx #9:lda #0:jmp sndset
 rts
;
; Play explosion sound (channel C)
.explodeSound
 ldx addr(explodeVol):beq explodeSkip
 dex:stx addr(explodeVol)
 txa
 ; Volume register C attenuated
 ldx #10:jsr sndset
 cmp #0
 beq explodeErase
 and #1:beq explodeFlash
 lda #0b01110011
 .db 0x2c
.explodeFlash
 lda #0b01001100
.explodeErase
 sta addr(spriteMask)
 lda addr(explodeFrame)
 ldx addr(explodeX):ldy addr(explodeY)
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
 lda 0x300+13:and #0x20
 beq timeoutT2
 jmp initT2
;
; Hires bit mask to bit position look up
; 1=pos 5*2+7, 2=pos 4*2+7, 4=pos 3*2+7
; 8=pos 2*2+7,16=pos 1*2+7,32=pos 0*2+7
.maskLookUp
.db  0,17,15,0, 13, 0, 0, 0
.db  11,0, 0, 0, 0, 0, 0, 0
.db  9, 0, 0, 0, 0, 0, 0, 0
.db  0, 0, 0, 0, 0, 0, 0, 0
.db  7, 0, 0, 0, 0, 0, 0, 0
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
 jsr pointSetup
 sty lineoffset
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
; brk:.db 0
 ; Number of rows to do
 ldx ztHeight
.nextSpriteRow
 ; Poke from right to left starting at width
 ldy ztWidth
.plotSpriteRow
 lda 0xffff,y:.source1
 and addr(spriteMask)
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
 ; Decrement height
 dex
 ; Keep going until all rows done
 bne nextSpriteRow
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
;
; After creating a hit box, check if a point
; falls inside this box - x,y defines the point
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
;
.initScore
 lda #'0'
 ldx #4
.initScoreDigit
 sta 0xbf6a,x
 dex
 bne initScoreDigit
 rts
;
;
.updateScore
 ldx #4
 lda 0xbf6a,x
 cmp #'5'
 beq doScoreIncrement
 lda #'5'
 sta 0xbf6a,x
 rts
.doScoreIncrement
 lda #'0'
 sta 0xbf6a,x
 dex
.updateScoreDigit
 lda 0xbf6a,x
 clc:adc #1:cmp #'9'+1:bcc updateScoreNoCarry
 lda #'0'
.updateScoreNoCarry
 sta 0xbf6a,x
 dex
 cmp #'0'
 beq updateScoreDigit
.updateScoreDone
 rts
;
;
.initMsg
 lda #10:sta 0xbf90:sta 0xbf90+40
 lda #3:sta 0xbf91:lda #2:sta 0xbf91+40
 lda #5:sta msgSpeed:sta msgCount
 lda #0:sta msgIdx
 rts
.msgSpeed:.db 0
.msgCount:.db 0
.msgIdx:.db 0
.msg
 .db "#FF for these Fantastic Folks!...  "
 .db "Annatar34381343 daph2theb SharpworksMZ leelegionsmith "
 .db "JonnBlanchard JAMOGRAD 0xC0DE6502 jimblimey CodingAndThings "
 .db "KevEdwardsRetro AdamJWilson1995 alannakelly_ie electron_greg "
 .db "JoachimFroholt loudscotsbloke AtticThe  ",0
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
 sta 0xbf92,x:sta 0xbf92+40,x
 inx:cpx #36
 bne showMsgLoop
 ldy msgIdx:iny
 lda msg,y:bne msgSkipReset
 ldy #0
.msgSkipReset
 sty msgIdx
.msgDone
 rts
;
.playerUpdate
  lda addr(xPos)+1:sta addr(ox)+1
  lda addr(yPos)+1:sta addr(oy)+1
  jsr kbstick:pha
  jsr playerSound
  pla:pha:and #1:beq skipLeft
  ; Do left dx only if not -2
  lda addr(dx)+1:cmp #-3&0xff:beq dxLowerLimit
  sec
  lda addr(dx):sbc addr(xStep):tay
  lda addr(dx)+1:sbc addr(xStep)+1
  bpl limitMinusDX:cmp #-3&0xff:bcs limitMinusDX
.dxLowerLimit
  lda #-3&0xff:ldy #0x80
.limitMinusDX
  sta addr(dx)+1:sty addr(dx)
.skipLeft
  pla:pha:and #2:beq skipRight
  ; Do right dx only if not 2
  lda addr(dx)+1:cmp #2:beq dxUpperLimit
  clc
  lda addr(dx):adc addr(xStep):tay
  lda addr(dx)+1:adc addr(xStep)+1
  bmi limitPlusDX:cmp #3:bcc limitPlusDX
.dxUpperLimit
  lda #2:ldy #0x80
.limitPlusDX
  sta addr(dx)+1:sty addr(dx)
.skipRight
  pla:pha:and #16:beq skipFire
  lda addr(bulletY)+1:bne skipFire
  jsr startBullet
.skipFire
  ; Do friction in x
  ldy addr(dx)+1:bmi frictionXPlus
  sec:lda addr(dx):sbc addr(friction):sta addr(dx)
  tya:sbc addr(friction)+1:sta addr(dx)+1
  bcs frictionY
.frictionXZero:lda #0:sta addr(dx):sta addr(dx)+1
  beq frictionY
.frictionXPlus
  clc:lda addr(dx):adc addr(friction):sta addr(dx)
  tya:adc addr(friction)+1:sta addr(dx)+1
  bcs frictionXZero
.frictionY
  ; Add x velocity to position
  clc:lda addr(xPos):adc addr(dx):tya
  lda addr(xPos)+1:adc addr(dx)+1
  cmp #leftMargin:bcs skipLeftMargin
  lda #leftMargin:ldy #0x80:jsr dxNegate
.skipLeftMargin
  cmp #rightMargin+1:bcc skipRightMargin
  lda #rightMargin:ldy #0x80:jsr dxNegate
.skipRightMargin
  sta addr(xPos)+1:sty addr(xPos)
  ; Add y velocity to position
  clc:lda addr(yPos):adc addr(dy):sta addr(yPos)
  lda addr(yPos)+1:adc addr(dy)+1
  cmp #100:bcs skipTopMargin:lda #100
.skipTopMargin
  cmp #185:bcc skipBottomMargin:lda #185
.skipBottomMargin
  sta addr(yPos)+1
  ; Check old and new positions
  lda addr(xPos)+1:cmp addr(ox)+1:bne doUpdate
  lda addr(yPos)+1:cmp addr(oy)+1:bne doUpdate
  ; first get stick status off stack
  pla
  rts
.dxNegate
  pha:sec
  lda #0:sbc addr(dx):sta addr(dx)
  lda #0:sbc addr(dx)+1:sta addr(dx)+1
  pla:rts
.doUpdate
  lda #0x40:sta addr(spriteMask)
  ldx addr(ox)+1:ldy addr(oy)+1:lda #0
  jsr drawSprite
  lda #0xff:sta addr(spriteMask)
  ldx addr(xPos)+1:ldy addr(yPos)+1:lda #0
  jsr drawSprite
  ; first get stick status off stack
  pla
  rts
;
; Calculate bullet pos and vel from bottom
; x=position
.initBulletPos
 stx addr(bulletX)+1:stx addr(bulletX):;	Scaled integer x pos - use MSB
 txa:sec:sbc #10:sta addr(bulletDX):; C still set as x >= 10
 lda #(rightMargin-leftMargin)/2:sbc addr(bulletDX):sta addr(bulletDX)
 lda #0:sbc #0:sta addr(bulletDX)+1:; Scaled integer dx
 lda addr(yPos)+1:sec:sbc #6
 sta addr(bulletY)+1:; Y is at ship position-6 use MSB
 rts
;
; Start a bullet and show it
.startBullet
 lda addr(xPos)+1:adc #8:tax:jsr initBulletPos:tay:; A=Bullet Y pos
 ldx #255:jsr drawBullet
 rts
;
; Draw bullet based on mask in A
.drawBullet
 sta addr(spriteMask)
 ; Y is vert pos
 ldy addr(bulletY)+1
 jsr calcBulletFrame
 ldx addr(bulletX)+1:ldy addr(bulletY)+1
 jmp drawSprite
;
; Check bullet for hit
.checkBulletHit
; Do bullet update
 ldy addr(bulletY)+1:beq skipBulletHit
 ldy addr(alienY)+1:beq skipBulletHit
 ldx addr(alienX)+1
 lda addr(lastAlienFrame)
 jsr makeHitBox
 ldx addr(bulletX)+1:ldy addr(bulletY)+1
 jsr checkHitBox
 bcc skipBulletHit
 iny
 jsr checkHitBox
 lda addr(alienY)+1:sta addr(explodeY)
 lda addr(alienX)+1:sta addr(explodeX)
 lda addr(lastAlienFrame):sta addr(explodeFrame)
 lda #0x40:jsr drawBullet
 jsr clearBullet
 jsr restartAlien
 jsr updateScore
 lda #15:sta addr(explodeVol)
.skipBulletHit
 rts
 ;
 ;
.bulletUpdate
 ldy addr(bulletY)+1:beq skipBulletUpdate
 lda #0x40:jsr drawBullet
 lda addr(bulletY)+1:jsr calcDY:sty addr(bulletDY)+1
 jsr moveBulletUp
 ldy addr(horizon):cpy addr(bulletY)+1:bcs clearBullet
 lda #0xff:jsr drawBullet
.skipBulletUpdate
 rts
.clearBullet
 lda addr(bulletY)+1:sta addr(bulletYTemp)
 lda #0:sta addr(bulletY)+1
 rts
;
; Move bullet by Y rows up, adjusting X position
; y=how many rows
.moveBulletUp
 clc:lda addr(bulletX):adc addr(bulletDX):sta addr(bulletX)
 lda addr(bulletX)+1:adc addr(bulletDX)+1:sta addr(bulletX)+1
 dec addr(bulletY)+1:dey:bne moveBulletUp
 rts
;
; Calculate bullet frame, Y=coordinate
; output A=frame index
.calcBulletFrame
 jsr calcSpriteFrame
 clc:adc #15
 sta addr(lastBulletFrame)
 rts
;
; Calculate bullet pos and vel from bottom
; x=position
.initAlienPos
 stx addr(alienX)+1:stx addr(alienX):;	Scaled integer x pos - use MSB
 txa:sec:sbc #54:					 ; x-54
 sec:sbc #(169-54)/2:sta addr(alienDX):; (x-54)-midpoint
 lda #0:sbc #0:sta addr(alienDX)+1:; Scaled integer dx
 asl addr(alienDX):rol addr(alienDX)+1:; dx=dx*2
 lda addr(horizon):sta addr(alienY)+1:; Y is at horizon use MSB
 ; Alien speed = half game speed
 lda addr(alienLevelSpeed):sta addr(alienSpeed):sta addr(alienSpeedCount)
 rts
;
; Draw alien based on mask in A
.drawAlien
  sta addr(spriteMask)
  ldx addr(alienX)+1:ldy addr(alienY)+1
  jsr calcAlienFrame
  jmp drawSprite
;
.alienUpdate
  lda #0x40:jsr drawAlien
  ; Only move alien based on speed counter
  dec addr(alienSpeedCount)
  bne skipAlienMove
  lda addr(alienSpeed)
  sta addr(alienSpeedCount)
  lda addr(alienY)+1:jsr calcDY:sty addr(alienDY)+1
  jsr moveAlienDown
  ; Bounds check
  lda addr(alienY)+1:cmp #186:bcc skipAlienMove
.restartAlien
  lda 0x08:; LSB of interrupt timer
  and #63:clc:adc #74:tax
  jsr initAlienPos
.skipAlienMove
  lda #0xff:jsr drawAlien
  rts
;
; Move alien by Y rows down, adjusting X position
; y=how many down
.moveAlienDown
 clc:lda addr(alienX):adc addr(alienDX):sta addr(alienX)
 lda addr(alienX)+1:adc addr(alienDX)+1:sta addr(alienX)+1
 inc addr(alienY)+1:dey:bne moveAlienDown
 rts
;
; Calculate alien frame, Y=coordinate
; output A=frame index
.calcAlienFrame
 jsr calcSpriteFrame
 clc:adc #1
 sta addr(lastAlienFrame)
 rts
;
; Calculate Y velocity based on A=Y coordinate
; Output in Y
.calcDY
; ldy #1:rts
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
; Small routine to convert * to 1 and space to 0
; X,A=Low/High of string
.dataConvert
 stx ztmp24:sta ztmp24+1
 ldy #0
.dataConvertLoop
 lda (ztmp24),y
 beq dataConvertDone
 cmp #'*':beq dataConvert1
 cmp #'X':beq dataConvert1
 cmp #'1':beq dataConvertDo
 lda #'0'
 bne dataConvertDo
.dataConvert1
 lda #'1'
.dataConvertDo
 sta (ztmp24),y
 iny
 bne dataConvertLoop
.dataConvertDone
 ; Return number of chars
 tya:tax:lda #0
 rts
;
; Create shifted sprite data from first image
; X,A=Low/High of sprite databank start
.makeSprite
 ; save sprite base
 stx ztmp24:sta ztmp24+1
 ; get width, height, size
 ldy #0
 lda (ztmp24),y:sta makeW
 iny:lda (ztmp24),y:sta makeH
 iny:lda (ztmp24),y:sta makeS
 ; Get to sprite data section
 ; 6 frames, starting at base+3+4+12
 ; Put this in ztemp+2,3
 clc:lda ztmp24:adc #19:sta ztmp24+2
 lda ztmp24+1:adc #0:sta ztmp24+3
 ldx #6
 ; Put 6 table entries table is 7 bytes in
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
 ; If no carry then fine
 bcc noCarryIn
 ; If carry in then OR with 0b01000000 (0x40)
 ora #0x40
.noCarryIn
 ; shift right in to destination, put back bit 6
 lsr:ora #0x40:sta (ztmp24+2),y
 ; Keep going for W bytes
 dex:bne makeSpriteRowData
 ; Ready to point to next row
 iny
 ; Keep going for H rows
 dec heightCount:bne makeSpriteRow
 ; Done a whole frame, next shift
 ; Move source by S bytes
 clc:lda ztmp24:adc makeS:sta ztmp24
 lda ztmp24+1:adc #0:sta ztmp24+1
 ; Move dest by S bytes
 clc:lda ztmp24+2:adc makeS:sta ztmp24+2
 lda ztmp24+3:adc #0:sta ztmp24+3 
 ; Keep going until all shifted frames done
 dec frameCount:bne makeSpriteFrame
 ; Done!
 rts
; Variables
.makeW:.db 0
.makeH:.db 0
.makeS:.db 0
.frameCount:.db 0
.heightCount:.db 0
; 
;
;* This label is at the end *
.endAsm
enddef
;
;
; Sprite Generator
def_generateSprites()
 ; 
 println "Generating Sprites.."
 spriteMask=0xff
 ; Set up address pointer in sprite table 
 sprTable[1]=sprShip
 sprTable[2]=sprAlien8
 sprTable[3]=sprAlien7
 sprTable[4]=sprAlien6
 sprTable[5]=sprAlien5
 sprTable[6]=sprAlien4
 sprTable[7]=sprAlien4
 sprTable[8]=sprAlien3
 sprTable[9]=sprAlien3
 sprTable[10]=sprAlien2
 sprTable[11]=sprAlien2
 sprTable[12]=sprAlien2
 sprTable[13]=sprAlien1
 sprTable[14]=sprAlien1
 sprTable[15]=sprAlien1
 sprTable[16]=sprBullet6
 sprTable[17]=sprBullet6
 sprTable[18]=sprBullet5
 sprTable[19]=sprBullet5
 sprTable[20]=sprBullet5
 sprTable[21]=sprBullet4
 sprTable[22]=sprBullet4
 sprTable[23]=sprBullet4
 sprTable[24]=sprBullet3
 sprTable[25]=sprBullet3
 sprTable[26]=sprBullet2
 sprTable[27]=sprBullet2
 sprTable[28]=sprBullet1
 sprTable[29]=sprBullet1
 _processSpriteData(sprShip)
 _processSpriteData(sprAlien1)
 _processSpriteData(sprAlien2)
 _processSpriteData(sprAlien3)
 _processSpriteData(sprAlien4)
 _processSpriteData(sprAlien5)
 _processSpriteData(sprAlien6)
 _processSpriteData(sprAlien7)
 _processSpriteData(sprAlien8)
 _processSpriteData(sprBullet1)
 _processSpriteData(sprBullet2)
 _processSpriteData(sprBullet3)
 _processSpriteData(sprBullet4)
 _processSpriteData(sprBullet5)
 _processSpriteData(sprBullet6)
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
  println "Row ",j
  ; Attribute
  read v:poke sprBase,v
  ; Bit pattern for w*6
  read d$
  t=call(dataConvert,d$/256,d$\256,0)
  for i=1,w,1:t$="0b"+mid(d$,i*6-5,6)
   poke sprBase+i,val(t$)|0x40
  next
  ; Ready for next line
  sprBase=sprBase+w+1
 next
 ; Ok meta data and first frame poked in to memory
 ; go make the whole sprite
 t=call(makeSprite,spr/256,spr\256,0)
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


xx will not be processed xx
def_processSpriteData(spr)
 sprBase=spr
 read w,h,s
 poke sprBase, w:poke sprBase+1, h:poke sprBase+2, s
 sprBase=sprBase+3
 ; Now poke the destination addresses
 for b=0,5,1
  doke sprBase, spr+3+12+s*b
  sprBase=sprBase+2
 next
 ; h rows
 for j=1,h,1
  println "Row ",j
  ; Attribute
  read v
  ; Bit pattern for w*6
  read d$
  t=call(dataConvert,d$/256,d$\256,0)
  ; Do bit position 0 to 5
  for b=0,5,1
   ; Attribute for each position
   poke sprBase+s*b,v
   ; Extract bit image for each byte row
   ; Put each shifted row in to right area
   ; OR with 0x40 to ensure it shows up
   for i=1,w,1:t$="0b"+mid(d$,i*6-5,6)
	poke sprBase+s*b+i,val(t$)|0x40
   next
   ; Generate shifted pattern by prepending 0
   t$="0"+d$
   d$=t$
  next
  ; Ready for next line
  sprBase=sprBase+w+1
 next
enddef

def_debug()
  println hex(t)," ",bulletYTemp
  for alienX=18,120,6
  alienY=horizon+2
  repeat
   spriteMask=0x00
   t=call(drawSprite,(alienY-horizon)>>3+1,alienX,alienY)
   alienY=alienY+(alienY-horizon)>>4+1
   spriteMask=0xff
   t=call(drawSprite,(alienY-horizon)>>3+1,alienX,alienY)
   println alienX,"  ",alienY
   wait 20
  until alienY > 180
  next
enddef
