; * PIX in DFLAT! *
; * 01/2020 rax *
def_s()
    poke 0x5a,0
    paper 4: ink 7: hires: cursor(1)
    x=120: y=100
    dim s$[40]
    dim modetxt$[10,5]
    modetxt$[0]="none"
    modetxt$[1]="point"
    modetxt$[2]="circle"
    modetxt$[3]="line"
    modetxt$[4]="text"
    _draw()
enddef
def_draw()
    pixmode 1
    plot x-2,y-3,43
    pixmode -1
    type = 1
    x0=0:y0=0
    _setOldPos()
    repeat
        st = stick()
        c= get(1)
        plot x-2,y-3,43
        if type >1: _action(type,-1): endif
        if (st&1): x=x-1: endif
        if (st&2): x=x+1: endif
        if (st&4): y=y-1: endif
        if (st&8): y=y+1: endif
        if (st&16)
            _action(type,1)
            if type > 1
                type = 1
                _setOldPos()
            endif
        endif
        if (c=='1'): type=1 : _setOldPos()
        elif (c=='2'): type=2 : _setOldPos()
        elif (c=='3'): type=3 : _setOldPos()
        elif (c=='4'): _printText(): _setOldPos(): type = 1
        endif
        if type >1: _action(type,-1): endif
        plot x-2,y-3,43
    until c==27
enddef
def_setOldPos()
    cls
    print "mode: ":print modetxt$[type] 
    println 
    println "1:Point   2:Circle   3:Line   4:Text"
    print "PIX | keys: arrows, space, 1, 2, 3, 4"
    poke 49080,22
    poke 49081,0
    x0=x:y0=y
enddef
def_action(type,mode)
    if type==1 
        pixmode mode
        point x,y
        pixmode -1
    elif type==2
        l=x-x0
        if l<0:l=x0-x:endif
        pixmode mode
        circle x0,y0,l
        pixmode -1
    elif type==3
        pixmode mode
        line x0,y0,x,y
        pixmode -1
    endif
enddef
def_printText()
    cls
    cursor(0)
    print "Text:"
    input s$
    plot x,y,s$
    cursor(1)
enddef
_s()
