def_start(speed,rep)
 println "Parsing RTTL data.."
 _initMusic()
 musLen=len(musStr$)
 println "RTTL length=",musLen
 println musStr$
 println "Press any key.."
 z=get(1)
 repeat
     musIdx=1
	 repeat
      println musIdx," ",octave[musIdx]," ",note[musIdx]," ",duration[musIdx]
	  music 1,octave[musIdx],note[musIdx],0:play 1,0,1,1000
	  wait duration[musIdx]
	  musIdx=musIdx+1
	 until note[musIdx]==-1
     println
 until rep
 println "Done!"
enddef
;
def_initMusic()
 dim musStr$[255],musChar$[2]
;Popcorn2:d=8,o=6,b=140:
 musStr$="c,a#5,c,g5,16d#5,g5,4c5,c,a#5,c,g5,16d#5,g5,4c5,c,d,d#,16d,d#,"
 musStr$=musStr$+"16d#,c,d,16c,d,16d,a#5,c,16a#5,c,16c,g#5,4c,c,a#5,c,g5,d#5,g5,"
 musStr$=musStr$+"4c5,c,a#5,c,g5,16d#5,g5,4c5,c,d,d#,16d,d#,16d#,c,d,16c,d,16d,"
 musStr$=musStr$+"a#5,c,16a#5,c,16c,d,4d#"
 musDefOct=6
 musBeat=3000/100
 musDefDur=32/8
;
;TakeOnMe:d=16,o=5,b=100:
; musStr$="8p,a#,a#,a#,8f#,8d#,8g#,8g#,g#,c6,c6,c#6,d#6,c#6,c#6,c#6,"
; musStr$=musStr$+"8g#,8f#,8a#,8a#,a#,g#,g#,a#,g#,a#,a#,a#,8f#,"
; musStr$=musStr$+"8d#,8g#,8g#,g#,c6,c6,c#6,d#6,c#6,c#6,c#6,8g#,8f#,8a#,8a#"
; musDefOct=5
; musBeat=3000/100
; musDefDur=32/16
;
 dim note[200],octave[200],duration[200]
 curIdx=1
 musIdx=1
 repeat
  octave[curIdx]=musDefOct
  duration[curIdx]=musDefDur
  musChar$=mid(musStr$,musIdx,1)
  if (musChar$>"0") & (musChar$<"9")
   duration[curIdx]=32/(asc(musChar$)-'0')
   if musChar$=="1"
    musIdx=musIdx+1
    musChar$=mid(musStr$,musIdx,1)
    if musChar$=="6"
	 duration[curIdx]=32/16
	else
	 musIdx=musIdx-1
	endif
   elif musChar$=="3"
    duration[curIdx]=32/32
	musIdx=musIdx+1
   endif
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if (musChar$>="a") & (musChar$<="p")
   if   musChar$=="c":note[curIdx]=0
   elif musChar$=="d":note[curIdx]=2
   elif musChar$=="e":note[curIdx]=4
   elif musChar$=="f":note[curIdx]=5
   elif musChar$=="g":note[curIdx]=7
   elif musChar$=="a":note[curIdx]=9
   elif musChar$=="b":note[curIdx]=11
   else:note[curIdx]=12
   endif
   musIdx=musIdx+1
   musChar$=mid(musStr$,musIdx,1)
   if musChar$=="#"
	note[curIdx]=note[curIdx]+1
   else
    musIdx=musIdx-1
   endif
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if (musChar$>="4") & (musChar$<="7")
   octave[curIdx]=asc(musChar$)-'0'
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if musChar$=="."
   duration[curIdx]=duration[curIdx]+duration[curIdx]/2
   musIdx=musIdx+1
  endif
  musChar$=mid(musStr$,musIdx,1)
  if musChar$==","
   musIdx=musIdx+1
  endif
  duration[curIdx]=duration[curIdx]*musBeat/speed
; println musIdx," ",curIdx," ",note[curIdx]," ",octave[curIdx]," ",duration[curIdx]
  curIdx=curIdx+1
 until musIdx>len(musStr$)
 note[curIdx]=-1
enddef
println "_start(speed,rep)"
;
