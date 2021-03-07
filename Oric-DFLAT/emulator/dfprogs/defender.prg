; * DEFENDER in DFLAT! *
; * 01/2021 rax *
def_start()
	reset r:r=rnd(r)
	cursor(1)
	;spr
	spPlayerNormal=33: spPlayerNormalSeq=44
	spPlayerStone=34: spPlayerStoneSeq=46
	spBug=37: spBugSeq=38
	spLadder=36+128
	spBrick=35+128
	spStone=42
	;animator
	animatorState = 0
	animatorSpeed = 3
	animatorPlayerState=0
	animatorBugState=0
	animatorPlayerNormalAddr = 0xb800+spPlayerNormal*8
	animatorPlayerNormalSeqAddr = 0xb800+spPlayerNormalSeq*8
	animatorPlayerStoneAddr = 0xb800+spPlayerStone*8
	animatorPlayerStoneSeqAddr = 0xb800+spPlayerStoneSeq*8
	animatorBugAddr=0xb800+spBug*8
	animatorBugSeqAddr=0xb800+spBugSeq*8
	;
	playerX = 15
	playerY = 9
	playerArmed=0
	bugsState = 0
	score = 0
	gameSpeed = 0
	hiScore = 0
	;
	dim enemies[40]
	gameState=0
	src1=6: src2=20: src3=34
	_init()
	_title()
	_hlp()
	repeat
		_game()
		if score > hiScore: hiScore = score: endif
		cls
		printat 15,8,"GAME OVER"
		printat 15,13,"Score: "
		printat 22,13,score
		printat 13,16,"Hi-score: "
		printat 23,16,hiScore
		c=get(1)
	until 0
enddef
def_init()
	paper 0: ink 6: cls
	i=0
	repeat
		read,v
		poke 0xb900+i,v
		i=i+1
	until v>250
enddef
def_title()
	i=0
	repeat
		read v
		poke 0xbba8+i,v
		i=i+1
	until v>200
	printat 2,27,"Press any key..."
	plot 1,27,12
	c=get(1)
enddef
def_game()
	poke 0x5a,0
	_board()
	gameState = 0
	playerArmed=0
	gameSpeed = 5
	score = 0
	;;set strites
	sprchar 1,spPlayerNormal
	repeat
		;c=get(1)
		_animator()
		_enemy(rnd(0)\29+6)
		_player()
		sprupd
		wait 2
	until gameState
	poke 0x5a,32
enddef
def_player()
	s=stick()
	if (s&1) & (playerX>6) : playerX = playerX - 1:endif
	if (s&2) & (playerX<34) : playerX = playerX + 1:endif
	if s&24
		if playerArmed == 1
			sprchar 1,spPlayerNormal
			sprupd
			_shoot(playerX)
			playerArmed = 0
		endif
		_printScore()
	endif
	if s&4
		if playerX == src1: playerArmed = 1: endif
		if playerX == src2: playerArmed = 1: endif
		if playerX == src3: playerArmed = 1: endif
		if playerArmed: sprchar 1,spPlayerStone: endif
	endif
	sprpos 1, playerX, playerY
enddef
def_enemy(n)
	if bugsState > 0
		bugsState = bugsState-1
		enddef
	endif
	bugsState = gameSpeed
	play 1,0,4,100
	if enemies[n] == 0: printat 0,0, n :endif
	plot n,enemies[n],spLadder
	enemies[n] = enemies[n]-1
	plot n,enemies[n],spBug
	if (enemies[n]==10): gameState=1: endif
enddef
def_board()
	cls
	;;lores
	for y=1,27,1:plot 3,y,9:next
	;; stars
	for a=1,50,1:x=rnd(0)\30+5: y=rnd(0)\7+1: plot x,y,"+":next
	;;wall
	for y=10,23,1:for x=5,35,1:plot x,y,163:next:next
	plot 5,9,163:plot 35,9,163
	plot 5,8,163:plot 35,8,163
	plot src1,8,163: plot src2,8,163: plot src3,8,163
	;; init enemies
	for i=0,40,1: enemies[i]=23: next
	;;moon
	_moon(30,2)
enddef
def_hlp()
	cls
	printat 1,4,"Hello Captain,"
	printat 1,6,"A year ago, strange creatures crawled"
	printat 1,7,"out of the ground and began attacking"
	printat 1,8,"cities. All cities have built"
	printat 1,9,"firewalls to protect people."
	printat 1,11,"You are the last survivor of the"
	printat 1,12,"northern defense. The only weapons"
	printat 1,13,"left are stones from the catapults."
	printat 1,14,"The stones are at both ends of the"
	printat 1,15,"wall and in the middle. You have to"
	printat 1,16,"take a stone with an 'arrow up' and"
	printat 1,17,"throw it with an 'space'."
	printat 1,19,"Good luck"
	printat 1,27,"Press any key..."
	c= get(1)
enddef
def_moon(x,y)
	printat x,y,"01"
	printat x,y+1,"23"
enddef
def_printScore()
	printat 28,0,"Score:"
	printat 35,0,score
enddef
def_shoot(n)
	play 0,1,3,2345
	if (enemies[n]) < 23
		score = score + 10
		if (score == 100) | (score == 400) | (score == 800) | (score == 2000) 
			gameSpeed = gameSpeed - 1
		endif
	endif
	enemies[n]=23
	for y=10,22,1:plot n,y,spBrick:plot n,y+1,spStone:wait 2:next
	plot n,23,spBrick
enddef
def_animator()
	if animatorState > 0
		animatorState = animatorState-1
		enddef
	endif
	animatorState = animatorSpeed
	for i=0,7,1
		poke animatorPlayerNormalAddr+i, peek(animatorPlayerNormalSeqAddr+i+animatorPlayerState*8)
		poke animatorPlayerStoneAddr+i, peek(animatorPlayerStoneSeqAddr+i+animatorPlayerState*8)
		poke animatorBugAddr+i, peek(animatorBugSeqAddr+i+animatorBugState*8)
	next
	animatorPlayerState=animatorPlayerState+1
	animatorBugState=animatorBugState+1
	if animatorPlayerState == 2: animatorPlayerState = 0:endif
	if animatorBugState == 2: animatorBugState = 0:endif
enddef
data 0,0,0,0,0,0,0,0
data 0,12,12,30,45,12,18,51
data 63,63,45,30,12,12,18,51
data 0,62,62,62,0,55,55,55
data 0,54,58,54,0,59,55,59
data 0,12,18,2,12,0,8,0
data 18,44,30,13,44,30,13,0
data 18,13,30,44,13,30,44,0
data 18,45,30,12,45,30,12,0
data 18,44,30,13,44,30,13,0
data 0,12,22,47,63,30,12,0
data 0,0,0,0,4,0,0,0
data 0,12,12,30,45,28,50,3
data 0,12,12,30,45,14,19,48
data 63,63,45,30,12,28,50,3
data 63,63,45,30,12,14,19,48
data 0,0,2,0,0,0,0,32
data 0,0,24,28,14,14,7,7
data 0,32,24,31,15,3,0,0
data 15,15,62,62,60,48,0,0
data 255
data 18,18,18,21,19,19,21,21,19,19,19,21,19,19,19,21,19,19,19,21
data 21,19,19,21,19,19,21,21,19,19,19,21,19,19,21,21,18,18,18,18
data 18,18,18,21,19,21,19,21,19,21,21,21,19,21,21,21,19,21,21,21
data 19,21,19,21,19,21,19,21,19,21,21,21,19,21,19,21,21,23,23,18
data 18,18,21,21,19,21,19,21,19,19,21,21,19,19,21,21,19,19,21,21
data 19,21,19,21,19,21,19,21,19,19,21,21,19,19,21,21,21,23,23,23
data 18,18,21,21,19,19,21,21,19,19,19,21,19,21,21,21,19,19,19,21
data 19,21,19,21,19,19,21,21,19,19,19,21,19,21,19,21,21,23,17,17
data 18,18,18,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21
data 21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,17,17,17,17
data 18,18,18,18,18,18,16,22,22,22,22,23,23,23,23,18,18,16,23,23
data 23,23,23,23,23,23,23,17,17,17,17,17,22,17,17,17,17,22,17,17
data 18,16,23,23,23,16,22,22,20,22,22,22,22,23,23,23,23,23,23,23
data 23,17,17,17,17,17,17,17,17,22,17,17,17,17,22,17,17,17,17,17
data 18,16,23,16,22,22,22,22,20,20,20,22,20,22,22,17,17,17,22,17
data 17,17,22,17,17,17,17,17,17,17,17,17,17,17,17,17,22,17,17,17
data 16,23,16,22,22,20,20,22,22,22,22,22,20,20,22,22,17,17,17,17
data 17,22,17,17,17,22,17,17,17,17,17,22,17,17,17,17,17,17,17,22
data 23,16,22,22,20,20,22,22,20,20,22,22,22,22,22,17,17,17,22,17
data 17,17,17,17,17,17,17,17,22,17,17,17,17,17,16,23,23,17,16,23
data 17,16,22,22,22,22,22,22,22,22,17,17,17,17,17,17,22,17,17,17
data 17,22,17,17,17,17,17,17,17,17,17,17,17,17,16,23,23,23,16,23
data 22,17,16,22,22,22,17,17,17,17,17,17,16,21,21,17,17,17,17,22
data 17,17,17,17,16,23,23,23,23,17,22,17,17,16,23,23,23,23,23,23
data 17,17,17,17,17,17,17,17,16,21,17,17,16,21,21,21,17,22,17,17
data 17,17,17,22,16,23,23,23,23,17,16,23,23,23,23,23,23,23,23,20
data 17,17,16,21,21,21,17,16,21,21,21,17,17,16,21,21,17,17,17,17
data 17,17,17,16,23,23,23,23,23,23,23,23,23,23,23,20,23,20,20,23
data 17,22,16,21,21,21,17,16,21,21,19,17,17,16,21,21,17,16,23,23
data 23,23,16,23,23,23,23,23,23,23,23,20,20,23,20,20,23,23,23,17
data 17,17,17,16,21,21,17,16,21,21,21,21,21,21,21,21,16,23,23,23
data 23,23,23,23,23,23,23,23,20,20,20,20,23,23,23,23,17,17,23,17
data 17,17,22,16,21,21,21,21,21,21,21,21,21,21,17,17,16,23,23,23
data 23,23,23,23,23,20,20,20,20,23,23,23,17,17,17,17,17,17,23,17
data 17,17,17,17,16,21,21,21,21,21,21,21,17,17,16,23,23,23,23,23
data 23,20,20,20,20,23,23,23,23,17,17,17,17,17,17,17,19,17,17,17
data 17,17,17,16,23,23,23,16,21,21,21,21,23,23,23,23,23,23,23,20
data 20,20,23,23,23,23,17,17,17,17,17,17,17,23,16,19,19,19,23,23
data 22,17,16,23,23,23,23,16,21,23,23,23,23,23,23,20,20,20,20,23
data 23,23,23,17,17,17,17,17,23,23,17,17,17,17,17,16,19,19,19,17
data 17,17,16,23,23,23,23,23,23,23,23,23,23,23,20,20,23,23,23,17
data 17,17,17,23,17,17,17,23,23,17,16,19,19,19,17,23,16,19,19,19
data 17,16,23,23,23,23,23,23,23,20,20,20,23,23,23,23,17,17,17,17
data 17,23,23,23,17,17,17,17,17,16,19,19,19,18,19,17,16,19,19,19
data 23,23,23,23,23,23,23,23,20,20,23,23,23,17,17,17,17,17,23,17
data 17,17,17,17,17,17,17,23,23,16,19,18,19,19,19,19,19,19,19,19
data 23,23,20,20,23,20,23,23,23,23,17,17,17,17,17,23,23,23,23,17
data 17,17,17,17,23,23,16,19,17,17,16,19,19,19,19,19,19,19,19,17
data 20,20,20,23,23,23,17,17,17,23,23,23,17,17,17,17,17,17,17,17
data 23,23,23,17,16,19,19,19,19,19,23,16,19,19,19,19,19,19,19,17
data 23,23,23,23,17,17,17,23,23,23,17,17,17,17,17,23,17,17,17,23
data 23,17,17,17,17,17,16,19,19,19,19,19,19,19,19,19,19,19,19,19
data 17,17,17,17,23,23,17,17,17,17,17,17,17,23,23,17,17,17,17,17
data 17,17,23,23,17,17,23,16,19,19,19,19,19,17,16,19,19,19,19,19
data 255