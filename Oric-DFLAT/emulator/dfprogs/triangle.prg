; Sierpinksi Triangle generator
;  _start(l,x,y,s)
;  l=depth level
;  x,y=top/left start
;  s=size
; try _start(5,10,10,180)
def_start(l,x,y,s)
 hires
 _triangle(l,x,y,s)
enddef
;
def_triangle(l,x,y,s)
 local s2
 if l<1:enddef:endif
 line x,y,x+s,y
 line x,y,x,y+s
 line x+s,y,x,y+s
 s2=s>>1
 _triangle(l-1,x,y,s2)
 _triangle(l-1,x+s2,y,s2)
 _triangle(l-1,x,y+s2,s2)
enddef
println "Try _start(5,10,10,180)"
