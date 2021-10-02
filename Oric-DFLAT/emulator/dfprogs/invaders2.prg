def_start(d)
 dim code[300]
 _asm(0):_asm(0):_asm(3)
 println hex(code)
 println codeEnd-codeStart
 if get(1)==13
  a=call(initGame,0,0,0)
 endif
enddef
;
def_asm(o)
 .opt o
 .org code
 basePosition=25
 score=0xbba4
 ztmp24=0xb0
 pointSetup=0xcfe5
 kbstick=0xc737
 sndset=0xc000
 grcls=0xcd20
 grplot=0xcd76
 grget=0xcd89
 grxybase=0xcd66
 grptr=0x27
 dfarga=0xa1
 dfargb=0xa3
 dfargc=0xa5
 dfplotstr=0xebfe
 grsprinit=0xd1fb
 grsprerase=0xd209
 grsprnew=0xd21b
 grsprdraw=0xd24d
 grsprchar=0xd26e
 grsprpos=0xd27e
 dfrtsprupdate=0xf50a
 sprcurX=0xb400
 sprnewX=0xb420
 sprcurY=0xb440
 sprnewY=0xb460
 sprchr=0xb480
 sprbgnd=0xb4a0
.codeStart
;
.moveAliens
 .lda #0:.sta alienNewDir
 .ldx #31
.moveSingleAlien
 .ldy sprcurX,x
 .bmi skipRowChange
 .lda sprcurY,x
 .cmp #basePosition
 .bne notDead
 .inc dead
.notDead
; .sta sprnewY,x
 .lda alienDir
 .bne goLeft
 .iny
 .db 0x24:; BIT zero page to skip the dex
.goLeft
 .dey
 .tya
 .sta sprnewX,x
 .cmp #3
 .beq changeDir
 .cmp #37
 .bne skipRowChange
.changeDir
 .inc alienNewDir
.skipRowChange
 .dex
 .cpx #1
 .bne moveSingleAlien
.checkDir
 .lda alienNewDir:.beq skipDirChange
 .lda #1:.eor alienDir:.sta alienDir
 .ldx #31
.changeRow
 .lda sprcurX,x:.sta sprnewX,x
 .inc sprnewY,x
 .dex:.cpx #1:.bne changeRow
.skipDirChange
 .rts
;
.movePlayer
 .jsr kbstick:.pha
 .ldy sprcurX
 .and #16:.beq skipPlayerShoot
 .ldx sprcurX+1:.bpl skipPlayerShoot
 .sty sprnewX+1
 .ldx #basePosition:.stx sprnewY+1
.skipPlayerShoot
 .pla:.pha
 .and #1:.beq skipPlayerLeft
 .cpy #2:.beq updatePlayer
 .dey:.bne updatePlayer
.skipPlayerLeft
 .pla:.pha
 .and #2:.beq updatePlayer
 .cpy #38:.beq updatePlayer
 .iny
.updatePlayer
 .pla
 .sty sprnewX
 .rts
;
.moveBullet
 .ldx sprcurY+1
 .ldy sprcurX+1:.bmi bulletInactive
 .dex:.bne bulletDone
.bulletEnd
 .ldy #0xff
.bulletDone
 .sty sprnewX+1
 .stx sprnewY+1
.bulletInactive
 .rts
;
.checkCollisions
 .ldy sprcurX+1:.bmi noCollision
 .ldx #24
.checkHitAlien
 .dex:.bmi noCollision
 .lda sprcurX+2,x:.cmp sprcurX+1
 .bne checkHitAlien
 .lda sprcurY+2,x:.cmp sprcurY+1
 .bne checkHitAlien
 .dec alienHit
 .lda #' ':.sta sprchr+2,x
 .lda #255:.sta sprnewX+2,x:.sta sprnewX+1
 .ldy #3
.updateScore
 .dey
 .ldx score,y:.inx
 .cpx #'9'+1:.bcc noOverflow
 .ldx #'0'
.noOverflow
 .txa:.sta score,y
 .bcs updateScore
.noCollision
 .rts
;
.gameLoop
 vdpcounter=0x08
; .dec bulletSpeed:.bpl gameLoop
; .ldx #1:.stx bulletSpeed
 .jsr moveBullet
.skipMoveBullet
 .dec playerSpeed:.bpl skipMovePlayer
 .ldx #2:.stx playerSpeed
 .jsr movePlayer
.skipMovePlayer
 .dec alienSpeed:.bpl skipMoveAliens
 .lda alienHit:.sta alienSpeed
 .jsr moveAliens
.skipMoveAliens
 .jsr checkCollisions
 .ldy #0
 .ldx addr(d)+1
.tick
 .dey
 .bne tick
 .dex
 .bne tick
 .jsr dfrtsprupdate
 .lda dead
 .bne gameOver
 .lda alienHit
 .bne gameLoop
.gameOver
 .rts
;
 level=ztmp24+0
 alienDir=ztmp24+3:alienNewDir=ztmp24+4
 alienHit=ztmp24+5
 tmpLevel=ztmp24+6
 tmpCount=ztmp24+7
 dead=ztmp24+8
 bulletSpeed=ztmp24+9
 playerSpeed=ztmp24+10
 alienSpeed=ztmp24+11
.alienInitCol
 .db 14,17,20,23,26,29
.initGame
 .jsr grcls
 .jsr grsprinit:			; Reset sprite engine
 .lda #'0'
 .sta score:.sta score+1:.sta score+2
 .ldx #0
 .stx level:.stx dead
.initLevel
 .inc level:.inc level
 .ldx #24:.stx alienHit
 .ldx #20:.stx sprnewX
 .ldx #basePosition:.stx sprnewY
 .ldx #'@':.stx sprchr
 .ldx #'|':.stx sprchr+1
; Initialise alien rank and file
 .ldx level:.dex:.stx tmpLevel
 .ldx #23
.initAlienRow
 .ldy #6
 .inc tmpLevel:.inc tmpLevel
.initAlien
 .dey:.bmi initAlienRow
 .lda alienInitCol,y
 .sta sprnewX+2,x
 .lda tmpLevel
 .sta sprnewY+2,x
 .lda #'v'
 .sta sprchr+2,x
 .dex:.bpl initAlien
;
 .jsr dfrtsprupdate
 .jsr gameLoop
 .lda dead
 .beq initLevel
 .rts
;
.codeEnd
enddef
