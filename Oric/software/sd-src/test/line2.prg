10 def_start(max)
11 dim i1[max],j1[max],i2[max],j2[max]
15 reset t:t=rnd(t)
20 hires:pixmode -1
30 x1=0:y1=0:dx1=2:dy1=3
40 x2=128:y2=96:dx2=3:dy2=-2
45 p=1:q=1:d=max-1
60 repeat
70 line x1,y1,x2,y2
71 i1[p]=x1:i2[p]=x2:j1[p]=y1:j2[p]=y2
72 p=p+1:if p>max:p=1:endif
75 tx1=x1:tx2=x2:ty1=y1:ty2=y2
80 t=x1+dx1
90 if t>239:t=239:dx1=0-(rnd(0)\5+2):endif
100 if t<0:t=0:dx1=(rnd(0)\5+2):endif
110 x1=t
120 t=y1+dy1
130 if t>199:t=199:dy1=0-(rnd(0)\5+2):endif
140 if t<0:t=0:dy1=(rnd(0)\5+2):endif
150 y1=t
160 t=x2+dx2
170 if t>239:t=239:dx2=0-(rnd(0)\5+2):endif
180 if t<0:t=0:dx2=(rnd(0)\5+2):endif
190 x2=t
200 t=y2+dy2
210 if t>199:t=199:dy2=0-(rnd(0)\5+2):endif
220 if t<0:t=0:dy2=(rnd(0)\5+2):endif
230 y2=t
231 if d>0
232  d=d-1
233 else
235  line i1[q],j1[q],i2[q],j2[q]
236  q=q+1:if q>max:q=1:endif
237 endif
240 until 0
250 endif

