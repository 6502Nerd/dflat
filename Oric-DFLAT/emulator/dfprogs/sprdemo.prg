def_start(max)
text
reset a:a=rnd(a)
dim x[32],y[32]
dim dx[32],dy[32]
for a=1,99,1
 plot rnd(0)\38+2,rnd(0)\28,42
next
for a=1,32,1
 x[a]=-1:x[a]=-1
next
for a=1,max,1
 x[a]=rnd(0)\30+5
 y[a]=rnd(0)\20+4
 dx[a]=1-2*(rnd(0)\2)
 dy[a]=1-2*(rnd(0)\2)
 sprchar a-1,47+a+128
next
sprmulti x,y:sprupd
repeat
 for a=1,max,1
  x[a]=x[a]+dx[a]
  if(x[a]<3)|(x[a]>38)
   dx[a]=0-dx[a]
  endif
  y[a]=y[a]+dy[a]
  if(y[a]<1)|(y[a]>26)
   dy[a]=0-dy[a]
  endif
 next
 sprmulti x,y:sprupd
until 0
enddef
println "_start(max) max=# of sprites 1..32"
