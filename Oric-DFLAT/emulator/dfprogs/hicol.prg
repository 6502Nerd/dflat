def_test()
text
_outputText()
_initColours()
enddef
;
def_outputText()
 for i=0,10,1
  println "Hello world ",i
 next
enddef
;
def_initColours()
 for i=0,127,1
  poke 0xa000+40*i,(i\7)+1
  poke 0xa001+40*i,26
 next
 for i=0,15,1
  poke 0xbb80+40*i+39,31
 next
enddef
;
