; Reproducing PCW BASIC
; Benchmarks from the 80s
; See this for ref:
; http://www.cpcwiki.eu/index.php/BASIC_Benchmark
; Benchmark 8 is missed as
; dflat does not do fp/trig.
; Tried to be faitful to
; these tests without trying
; to optimise or use dflat
; speed ups.
; Despite this, dflat seems
; to be around 3x faster
; than the Atmos timings for
; BM6 and 7!
;Test 1
def_test1()
 println "Start"
 for k=1,1000,1
 next
 println "Stop"
enddef
;
;Test 2
def_test2()
 println "Start"
 k=1
 repeat
  k=k+1
 until k>=1000
 println "Stop"
enddef
;
;Test 3
def_test3()
 println "Start"
 k=1
 repeat
  k=k+1
  a=k/k*k+k-k
 until k>=1000
 println "Stop"
enddef
;
;Test 4
def_test4()
 println "Start"
 k=1
 repeat
  k=k+1
  a=k/2*3+4-5
 until k>=1000
 println "Stop"
enddef
;
;Test 5
def_test5()
 println "Start"
 k=1
 repeat
  k=k+1
  a=k/2*3+4-5
  _sub()
 until k>=1000
 println "Stop"
enddef
;
def_sub()
enddef
;
;Test 6
def_test6()
 dim m[5]
 println "Start"
 k=1
 repeat
  k=k+1
  a=k/2*3+4-5
  _sub()
  for l=1,5,1
  next
 until k>=1000
 println "Stop"
enddef
;
;Test 7
def_test7()
 dim m[5]
 println "Start"
 k=1
 repeat
  k=k+1
  a=k/2*3+4-5
  _sub()
  for l=1,5,1
   m[l]=a
  next
 until k>=1000
 println "Stop"
enddef
println "_testX() X=1..7 for PCW Benchmark"
