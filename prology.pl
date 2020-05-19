blok(a,2,2). % name of the block, width, heigth
blok(b,1,2).
blok(c,5,1).
blok(d,2,1).
blok(e,4,1).
blok(f,1,1).
blok(g,2,1).
blok(h,1,2).
blok(i,4,1).
blok(j,2,1).
blok(k,5,1).
blok(l,1,2).
blok(m,2,1).
blok(n,1,2).
blok(o,3,1).
blok(p,1,3).
blok(r,7,4).
blok(s,3,1).
blok(t,1,2).
blok(u,2,1). 

pol(a,5,1). %placement of the block
pol(b,6,3). 
pol(c,6,5).
pol(d,10,4).
pol(e,10,3).
pol(f,10,2).
pol(g,9,1). 
pol(h,13,1). 
pol(i,10,6).
pol(j,13,5).
pol(k,12,4).
pol(l,14,2).
pol(m,14,1). 
pol(n,14,6). 
pol(o,14,8).
pol(p,16,5).
pol(r, 2,1).
pol(s,11,9).
pol(t,16,2).
pol(u,11,8).

prop(H,Place, BlockWidth, Name) :- pol(Name,Place,H), 
   				   blok(Name, BlockWidth,_).

give([DLW,DLH],[DPW,DPH],[GLW,GLH],[GPW,GPH], Name) :- 
    pol(Name,DLW,DLH), blok(Name,X,Y),
    DPH is DLH,
    DPW is DLW+X-1,
    GLH is DLH+Y-1,
    GLW is DLW,
    GPH is GLH,
    GPW is DPW.
    
write(Name,Name1,DLW,DPW) :- 
    give([DLW,_],[DPW,_],_,_,Name), 
    give([DLW1,_],[DPW1, _],_,_,Name1),
    Name\=Name1, (DLW==DLW1; DPW==DPW1), 
    (pol(Name,_,1);pol(Name1,_,1)). 

%checks if the given point is occupied by given block
isThereABlock(X,Y,Name):-
    pol(Name,W,H), blok(Name,Width,Height),
    RCX is W+Width-1, 
    X>=W, X=<RCX,
    UH is H + Height -1,
    Y>=H, Y=<UH.

%checks if there is a block on given coordinates
%false - there is no block, true - there is a block
%TO CHANGE, NOT INTUITIONAL WAY
isThePlaceFree(X,H) :-
    pol(Name,_,_),
    isThereABlock(X,H,Name).

%checks if the line of blocks from LeftX coordinate to 
%RightX coordinate is free
isTheLineFree(LX,LX,H) :- not(isThePlaceFree(LX,H)).
isTheLineFree(LX,RX,H) :- 
    not(isThePlaceFree(LX,H)), NLX is LX+1,
    isTheLineFree(NLX,RX,H).

%checks if space under the bar is free
isThereFreePlaceUnderTheBar(_,_,H,Y) :- H==Y.
isThereFreePlaceUnderTheBar(LX, PX, H, Y) :- 
    H\=Y,
    isTheLineFree(LX,PX,H), NH is H-1,
    isThereFreePlaceUnderTheBar(LX,PX,NH,Y).

%checks if the block of given name is a bar
bar(Bar) :-
    pol(Bar,X,Y),
    blok(Bar, Width, _),
    Width >=3, Y >=2,
    LX is X + 1, %Left corner of the free space under the bar on X ->
    PX is X + Width - 2, %Right = || =
    H is Y - 1, % Height
    isThereFreePlaceUnderTheBar(LX, PX, H,1). 

findBars(Name) :-
    pol(Name,_,_),
    bar(Name).

giveNameOfTheBlock(X,Y, Name) :-
    pol(Name,_,_), isThereABlock(X,Y,Name).

areThereFillars(X,1) :- isThePlaceFree(X,1).
areThereFillars(X,Y) :-
    Y > 1,
    isThePlaceFree(X,Y),
    NY is Y -1,
    areThereFillars(X,NY).

findSupports(Bar, LS,RS) :-
    pol(Bar,X,Y), blok(Bar,Width,_),
	RX is X + Width-1,
    areThereFillars(X,Y),
    areThereFillars(RX,Y),
    FY is Y-1,
    giveNameOfTheBlock(X,FY,LS),
    giveNameOfTheBlock(RX,FY,RS).

findGate(Bar,LeftSupport,RightSupport) :- 
    findBars(Bar),
	findSupports(Bar, LeftSupport,RightSupport).

gates :-
    findGate(Bar,LeftSupport,RightSupport),
    write('Found gate. Left support: '), write(LeftSupport),
    write(', rigth support: '),write(RightSupport),
    write(', bar: '),write(Bar),nl().
%---------------FINDING-GATES------------------------------
giveHeightOfTheWindow(X,Y,L,W) :-
    Y > 1,
	not(isThePlaceFree(X,Y)), 
    NY is Y- 1, NL is L + 1,
    giveHeightOfTheWindow(X,NY,NL,W).
giveHeightOfTheWindow(X,Y,L,W) :- W is L,isThePlaceFree(X,Y).

lookForUp(Name,X,Y) :-
    pol(Name,X,Y), blok(Name,Width,_),
    Y >= 3, Width >= 3.

findUp(Up,H) :-
	lookForUp(Up,X,Y), 
    NX is X +1, NY is Y -1,
    giveHeightOfTheWindow(NX,NY,0,H),
    blok(Up,Width,_), W is Width-2,
	PNX is NX +W-1, NH is NY -H+1,
    isThereFreePlaceUnderTheBar(NX,PNX,NY,NH).

isThereABottom(X,X,F):- isThePlaceFree(X,F).
isThereABottom(LX,RX,F) :- 
    isThePlaceFree(LX,F), NLX is LX+1,
    isThereABottom(NLX,RX,F).

isTheFillar(X,F,F) :- isThePlaceFree(X,F).
isTheFillar(X,Y,F) :- 
    Y > 1,
    isThePlaceFree(X,Y), NY is Y -1,
    isTheFillar(X,NY,F).

areThereBlockAround(Name,F):-
    pol(Name,X,Y), blok(Name,Width,_),
    isTheFillar(X,Y,F),
    PX is X+Width-1, isTheFillar(PX,Y,F),
    isThereABottom(X,PX,F).

findWindows(Name,Left,Right,Down,Downx2) :- 
    pol(Name,X,Y),blok(Name,Width,_),findUp(Name,H),
    FillarHeight is Y - H -1, 
    areThereBlockAround(Name,FillarHeight),
	LY is Y -1,
	giveNameOfTheBlock(X,LY,Left),
    RX is X +Width-1,
    giveNameOfTheBlock(RX,LY,Right),
    giveNameOfTheBlock(X,FillarHeight,Down),
    giveNameOfTheBlock(RX,FillarHeight,Downx2).

writeDown(D,D) :- 
    write(', down: '), write(D).
writeDown(D1,D2) :- 
    D1 \= D2,
    write(', down: '), write(D1),
	write(', '), write(D2).

windows :-
    findWindows(Up,Left,Right,D1,D2),
    write('Found window. Up: '), write(Up),
    write(', left: '), write(Left), 
    write(', right: '), write(Right),
	writeDown(D1,D2)
%--------------UNSOPPORTED-BLOCK-------------------------------  
 check(Name, X,Y,Width) :-
	NX is X + Width-1, NY is Y -1,  
    not(isThereABlock(NX,NY)),
    not(isThereABlock(X,NY)). 
 check(Name, X,Y,Width) :-
	NX is X + Width-1, NY is Y -1,  
    not(isThereABlock(NX,NY));
    not(isThereABlock(X,NY)),
	MiddleX is X + Width/2, not(isThereABlock(MiddleX,NY)).
	
 
findUnsupportedBlocks(Name) :-
    pol(Name,X,Y), blok(Name,Width,_), Y > 1,
	Check(Name,X,Y,Width).


unsupportedBlocks :-
	findUnsupportedBlocks(Name),
	write('Found unsupported block: '), write(Name).

%------------------------------------------------------------------------------
%gates, windows, unsupportedBlocks.

