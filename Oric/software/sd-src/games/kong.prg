; copilot graphics
def_test()
 dim r1$[3,2],r2$[3,2]
 r1$[1]="ac":r2$[1]="bd"
 r1$[2]="eg":r2$[2]="fh"
 ink 1:paper 16+9
 text
 poke 0xbb80,16
 println "Donkey Kong"
 println
 println
 println
 _udg()
 x=2:a=0
 repeat
  plot x,2,"  ":plot x,3,"  "
  x=x+1:if x>38:x=2:endif
  plot x,2,r1$[a+1]:plot x,3,r2$[a+1]
  a=a^1
  wait 10
 until 0
enddef
;
;
def_udg()
 repeat
  read a
  if a>0
    i=0xb800+8*a
    for i=i,i+7,1
     read b
     poke i,b
    next
  endif
 until a<=0
enddef
;
; Blank
data 32
data 0b000000
data 0b000000
data 0b000000
data 0b000000
data 0b000000
data 0b000000
data 0b000000
data 0b000000
;Facing right frame 1
data 97
data 0b000111
data 0b001111
data 0b001010
data 0b001010
data 0b000100
data 0b011011
data 0b100000
data 0b101000
;
data 98
data 0b101000
data 0b101111
data 0b101111
data 0b010001
data 0b001001
data 0b001001
data 0b001000
data 0b001111
;
data 99
data 0b100000
data 0b111100
data 0b110000
data 0b001000
data 0b010000
data 0b101000
data 0b000100
data 0b010100
;
data 100
data 0b010100
data 0b110100
data 0b110100
data 0b011000
data 0b010000
data 0b010000
data 0b101000
data 0b111100
;
;Facing right frame 2
data 101
data 0b000111
data 0b001111
data 0b001010
data 0b001010
data 0b000100
data 0b011011
data 0b100000
data 0b100100
;
data 102
data 0b010100
data 0b010011
data 0b001011
data 0b000100
data 0b001001
data 0b010010
data 0b010010
data 0b011111
;
data 103
data 0b100000
data 0b111100
data 0b110000
data 0b001000
data 0b010000
data 0b101000
data 0b000100
data 0b010100
;
data 104
data 0b010100
data 0b110100
data 0b111000
data 0b011000
data 0b001000
data 0b101000
data 0b100100
data 0b111110
;
data -1

