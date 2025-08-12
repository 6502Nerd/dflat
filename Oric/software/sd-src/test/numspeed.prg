def_test()
 dim a$[10]:a$="12345"
 reset t
 for i=0,1000,1
  n=val(a$)
 next
 println elapsed(t)
 println n
enddef
