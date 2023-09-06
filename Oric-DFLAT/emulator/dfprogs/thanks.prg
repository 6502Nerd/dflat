def_start(delay)
_init()
x=9:y=9:dx=1:dy=1:m=1
reset msgTimer:reset plotTimer
repeat
if elapsed(plotTimer)>(delay*2)
 reset plotTimer
 _plot(x,y)
 x=x+dx:if (x<3)|(x>21):dx=0-dx:endif
 y=y+dy:if (y<2)|(y>17):dy=0-dy:endif
endif
if elapsed(msgTimer)>delay
 reset msgTimer
_message():m=m+1:if m>length:m=1:endif
endif
until 0
enddef
;
def_init()
ink 7:paper 0:cls
dim a$[18,8],m$[255],t$[255]
m$=""
for i=1,8,1
 read a$[i]
next
for i=1,7,1
 read t$:m$=m$+t$
next
length=len(m$)-37
for i=0,27,1:plot 1,i,(i\6)+1:next
plot 0,26,10:plot 0,27,10:plot 1,26,2:plot 1,27,3
enddef
;
def_plot(x,y)
for i=1,8,1
 plot x,y+i-1,a$[i]
next
enddef
;
def_message()
 t$=mid(m$,m,37):plot 2,26,t$:plot 2,27,t$
enddef
;
data "                 "
data "  *   *   *   *  "
data " * * * * * * **  "
data "   * * * * *  *  "
data "  *  * * * *  *  "
data " *   * * * *  *  "
data " ***  *   *  *** "
data "                 "
data "                                    "
data "Dear Twitter.. Never thought that I "
data "would ever reach 2000+ follows, but"
data " thanks to each and every one for yo"
data "ur support in making this the best n"
data "erdy place to be! From @6502Nerd :-)"
data "                                    "

