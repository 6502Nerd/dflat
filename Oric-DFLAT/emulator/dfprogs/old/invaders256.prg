def_start()
 dim code[200]
 _asm(0):_asm(0):_asm(3)
 println hex(code)
 println codeEnd-codeStart
 a=get(1)
 a=call(initGame,0,0,0)
 repeat
  a=call(moveAliens,0,0,0)
  a=call(movePlayer,0,0,0)
  a=call(moveBullet,0,0,0)
  wait 10
 until 0
enddef
;
def_asm(o)
 .opt o
 .org code
 basePosition=22
 ztmp24=0xb0
 pointSetup=0xcfe5
 kbstick=0xc737
 sndset=0xc000
 grcls=0xcd20
 grplot=0xcd76
 grxybase=0xcd66
 grptr=0x27
.codeStart
;
;
;.colour
; .db 5,7,4,7,3,7,2,7,1,7
;
; Draw aliens from map
.drawMap
 .ldx alienY:.stx alienTempY
 .ldx #7:.stx mapRow
 .lda #0:.sta mapOff:.sta alienNewDir
.drawMapRow
 .ldx alienTempY:.jsr grxybase
 ; Set colour attribute (x already set)
; .lda colour,x
 .ldy #1:;.sta (grptr),y
 .lda #64:.pha
 .ldx #0:.stx mapCol
.drawMapChar
 .iny
 .pla:.lsr:.bne doPlot
 .ror
 .inc mapCol:.inc mapOff
.doPlot
 .pha
 .ldx mapCol:.cpx #5:.beq nextMapRow
 .ldx mapOff
 .and gameMap,x
 .bne plotAlienChar
 .lda #' '
 .db 0x2c
.plotAlienChar
 .lda #'v'
 .sta (grptr),y
 .cmp #'v':.bne drawMapChar
 .lda #1
 .cpy #4:.bne skipLeftCheck
 .sta alienNewDir
.skipLeftCheck
 .cpy #36:.bne drawMapChar
 .sta alienNewDir
 .beq drawMapChar
.nextMapRow
 .pla
 .inc alienTempY
 .dec mapRow:.ldx mapRow
 .bpl drawMapRow
.rts
;
.moveAliens
 .lda alienDir
 .beq skipRight
 .ldx #0:.ldy #40:.clc
.shiftRightLoop
 .ror gameMap,x
 .inx:.dey
 .bne shiftRightLoop
 .beq drawAliens
.skipRight
 .ldx #39:.clc
.shiftLeftLoop
 .rol gameMap,x
 .dex
 .bpl shiftLeftLoop
.drawAliens
 .jsr drawMap
 .lda alienNewDir:.beq skipRowChange
 .eor alienDir:.sta alienDir
 .inc alienY
.skipRowChange
 .rts
;
.movePlayer
 .jsr kbstick:.pha
 .ldy playerX
 .and #16:.beq skipPlayerShoot
 .lda bulletY:.bne skipPlayerShoot
 .iny:.sty bulletX:.dey
 .ldx #basePosition:.stx bulletY
.skipPlayerShoot
 .pla:.pha
 .and #1:.beq skipPlayerLeft
 .cpy #2:.beq updatePlayer
 .dey:.bne updatePlayer
.skipPlayerLeft
 .pla:.pha
 .and #2:.beq updatePlayer
 .cpy #37:.beq updatePlayer
 .iny
.updatePlayer
 .pla
 .sty playerX
 .ldx #basePosition:.jsr grxybase
 .lda #' ':.sta (grptr),y:.iny
 .lda #'@':.sta (grptr),y:.iny
 .lda #' ':.sta (grptr),y
 .rts
;
.moveBullet
 .ldx bulletY:.beq bulletDone
 .jsr grxybase:.ldy bulletX
 .lda #' ':.sta (grptr),y
 .dex:.beq bulletDone
 .jsr grxybase
 .lda (grptr),y:.pha
 .lda #'|':.sta (grptr),y
 .pla:.cmp #'v':.bne bulletDone
 .dec alienHit:.ldx #0
.bulletDone
 .stx bulletY
 .rts
;
 level=ztmp24+0:score=ztmp24+1
 playerRowBase=ztmp24+3
 alienDir=ztmp24+5:alienNewDir=ztmp24+6
 playerX=ztmp24+7:alienY=ztmp24+8
 bulletX=ztmp24+9:bulletY=ztmp24+10
 alienHit=ztmp24+11
 dataSize=12
 mapCol=ztmp24+12:mapRow=ztmp24+13:mapOff=ztmp24+14
 alienTempY=ztmp24+15
.initData
 .db 1, 0, 0:				;level,score
 .db 48920&255,48920>>8:	;playerRowBase row 23
 .db 1,0:					;alienDir,alienNewDir
 .db basePosition,2:		;playerX,alienY
 .db 0,0:					;bulletX,bulletY
 .db 32:					;alienHit
.initMap
 .db 0b00000000,0b00000000,0b00000000,0b00000000,0b00000000
 .db 0b00000000,0b00000101,0b01010101,0b01010000,0b00000000
.initGame
 .ldx #0
.initLevel
 .lda initData,x:.sta ztmp24,x
 .inx:.cpx #dataSize
 .bne initLevel
;
 .ldx #0
.initLine
 .ldy #0
.initBit
 .lda initMap,y:.sta gameMap,x
 .iny:.inx
 .cpy #10:.bne initBit
 .cpx #40:.bne initLine
 .jsr grcls
 .ldx #2:.stx alienY
 .rts
;
.codeEnd
.gameMap:.ds 40
enddef
