%dynamic variables
:-dynamic blackList/1. %Black played balls list.
:-dynamic whiteList/1. %White played balls list.
:-dynamic turn/1. %Indicates whose the turn is: Black(1) or White(2).
:-dynamic scoring/2. %Indicates the scoring of the black and the white players.
:-dynamic graphicScoring/2.%keeps the scoring text.
:-dynamic moveList/1.%keeps list of chosen balls to move.
:-dynamic buttonFlag/1.%indicates whether the "ConfirmStep" button is on(1) or off(0).
:-dynamic tempCell/2.
:-dynamic arrangementType/1.
:-dynamic tempWin/1.
:-dynamic broadSideList/1.
:-dynamic pushList/1.


%assisting functions and facts:

    %finds the following or the previous char.
next_char(Char,NChar):-
	char_code(Char,CharCode),
	NCharCode is CharCode+1,
	char_code(NChar,NCharCode).

previous_char(Char,NChar):-
	char_code(NChar,NCharCode),
	CharCode is NCharCode-1,
	char_code(Char,CharCode).

    %information for each line (line_Index,initXVal,FinalXVal,graphical_CoordX_of_init_cell,graphical_CoordY_of_init_cell)
line_info('A',1,5,199,100).
line_info('B',1,6,165,154).
line_info('C',1,7,136,213).
line_info('D',1,8,102,271).
line_info('E',1,9,71,327).
line_info('F',2,9,102,384).
line_info('G',3,9,136,441).
line_info('H',4,9,165,496).
line_info('I',5,9,199,554).

    %converts board cell indexes to graphical coordinates.
findGraphicalCoords(X,Y,CoordX,CoordY):-
	line_info(Y,X0,_,CoordX0,CoordY),
	DeltaX is X-X0,
	CoordX is CoordX0+DeltaX*66.

    %prints error messages.
open_error_dialog_choice:-
	writeln('illegal'),
	new(D,dialog('')),
	send(D,background(bitmap('dialogue.bmp'))),
	send(D, append, text('Your choice is illegal!',center,font(times,bold,30))),
	send(D, append, button(quit, message(D, destroy))),
	send(D,open).

open_error_dialog_stuck:-
	new(D,dialog('')),
	send(D,background(bitmap('dialogue.bmp'))),
	send(D, append, text('You are stuck!',center,font(times,bold,30))),
	send(D, append, button(quit, message(D, destroy))),
	send(D,open).

subList(List,Len,[]):-
	length(List,LLen),
	LLen<Len.
subList(_,0,[]).
subList(List,Len,[Elem|B]):-
	nth1(Len,List,Elem),
	select(Elem,List,NList),
	NLen is Len-1,
	subList(NList,NLen,B).

     %retrieves appended list of black and white balls.
appended_black_and_white(AL):-
	blackList(BL),
	whiteList(WL),
	append(BL,WL,AL).

findHigherValCell(Cell1,Cell2,Max):-
	append([Cell1],[Cell2],List),
	sort(List,SList),
	last(SList,Max).

legal_Y_Indexes(['A','B','C','D','E','F','G','H','I']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start:-
	init_board(W),
	init_dyn_par,
	init_balls,
	drawWithRecBlack(W),
	drawNoRecWhite(W),
	print_scoring(W).

%initialize dynamic parameters.
init_dyn_par:-
	retractall(turn(_)),
	assert(turn(1)),
	retractall(moveList(_)),
	assert(moveList([])),
	retractall(buttonFlag(_)),
	assert(buttonFlag(0)),
	retractall(scoring(_,_)),
	assert(scoring(0,0)),
	retractall(graphicScoring(_,_)),
	new(ScoreB,text(0,center,font(times,bold,30))),
	new(ScoreW,text(0,center,font(times,bold,30))),
	assert(graphicScoring(ScoreB,ScoreW)),
	retractall(whiteList(_)),
	retractall(blackList(_)),
	retractall(arrangementType(_)),
	assert(arrangementType(0)),
	retractall(tempCell(_,_)),
	assert(tempCell(0,0)),
	retractall(broadSideList(_)),
	assert(broadSideList([])),
	retractall(pushList(_)),
	assert(pushList([])),
	retractall(tempWin(_)),
	assert(tempWin(0)).

%initialize graphic board.
init_board(W):-
	new(W,window('Abalone',size(700,700))),

	new(Background,bitmap('board.bmp')),
	send(W,display,Background,point(0,0)),

	send(W,open).

view_board(W):-
	new(Background,bitmap('board.bmp')),
	send(W,display,Background,point(0,0)).

% set both scoring value and scoring text objects according to the sent
% values.
set_scoring(Black,White):-
	retract(scoring(_,_)),
	retract(graphicScoring(_,_)),

	assert(scoring(Black,White)),
	new(ScoreB,text(Black,center,font(times,bold,30))),
	new(ScoreW,text(White,center,font(times,bold,30))),
	assert(graphicScoring(ScoreB,ScoreW)).

%prints the current scoring.
print_scoring(W):-
	graphicScoring(ScoreB,ScoreW),
	send(W,display,ScoreB,point(70,640)),
	send(W,display,ScoreW,point(620,50)).

%deletes the previous scoring.
delete_scoring:-
	graphicScoring(PrevScoreB,PrevScoreW),
	free(PrevScoreB),
	free(PrevScoreW).

view_scoring(W):-
	scoring(Black,White),
	delete_scoring,
	set_scoring(Black,White),
	print_scoring(W).

init_balls:-
	createBallList(1,5,'A',WhiteList1),
	createBallList(1,6,'B',WhiteList2),
	createBallList(3,5,'C',WhiteList3),
	append(WhiteList1,WhiteList2,WhiteAppendedList),
	append(WhiteAppendedList,WhiteList3,WhiteList),

	createBallList(5,9,'I',BlackList1),
	createBallList(4,9,'H',BlackList2),
	createBallList(5,7,'G',BlackList3),
	append(BlackList1,BlackList2,BlackAppendedList),
	append(BlackAppendedList,BlackList3,BlackList),

	assert(whiteList(WhiteList)),
	assert(blackList(BlackList)).


createBallList(Start,End,_,[]):-
	 Start is End+1.

createBallList(Start,End,Index,[(Start,Index)|B]):-
	 NextStart is Start+1,
	 createBallList(NextStart,End,Index,B).


drawNoRecWhite(W):-
	whiteList(WL),
	drawNoRec(W,WL,white).

drawNoRecBlack(W):-
	blackList(BL),
	drawNoRec(W,BL,black).

drawWithRecWhite(W):-
	whiteList(WL),
	drawWithRec(W,WL,white).

drawWithRecBlack(W):-
	blackList(BL),
	drawWithRec(W,BL,black).

drawNoRec(_,[],_).
drawNoRec(W,[(X,Y)|NL],Colour):-
	((Colour=='white',new(Pic,bitmap('greenball.bmp')));(Colour=='black',new(Pic,bitmap('redBall.bmp')))),
	findGraphicalCoords(X,Y,CoordX,CoordY),
	send(W,display,Pic,point(CoordX,CoordY)),
	drawNoRec(W,NL,Colour).

drawWithRec(_,[],_).
drawWithRec(W,[(X,Y)|NL],Colour):-
	((Colour=='white',new(Pic,bitmap('greenball.bmp')));(Colour=='black',new(Pic,bitmap('redBall.bmp')))),
	findGraphicalCoords(X,Y,CoordX,CoordY),
	send(W,display,Pic,point(CoordX,CoordY)),
	send(Pic,recogniser,click_gesture(left,'',single,and(message(@prolog,select_balls,W,X,Y)))),
	drawWithRec(W,NL,Colour).


select_balls(W,_,_):-
	turn(1),
	 view_control_buttons(W),fail.

select_balls(W,X,Y):-
	turn(1),moveList(CurrList),

	((member((X,Y,HlBall),CurrList),
         select((X,Y,HlBall),CurrList,NNList),
	 free(HlBall),
	 retract(moveList(_)),
	 assert(moveList(NNList)));

	(new(HLBall,bitmap('hlRedBall.bmp')),
	 findGraphicalCoords(X,Y,CoordX,CoordY),
	 send(W,display,HLBall,point(CoordX,CoordY)),
	 append([(X,Y,HLBall)],CurrList,NList)),
	 retract(moveList(_)),
	 assert(moveList(NList))).

select_balls(W,_,_):-
	turn(2),
	writeln('pp'),
	whiteList(WL),
	length(WL,Len),
	D is Len-1,
	Nth is random(D)+1,
	nth1(Nth,WL,(X,Y)),
	retract(moveList(_)),
	assert(moveList([(X,Y,_)])),
	mark_move_and_push_options(W).


view_control_buttons(W):-
	turn(1),buttonFlag(0),
	retract(buttonFlag(_)),
	assert(buttonFlag(1)),
	new(ConfirmStep,bitmap('confirmStepRight.bmp')),
	send(W,display,ConfirmStep,point(580,568)),
	send(ConfirmStep,recogniser,click_gesture(left,'',single,and(message(@prolog,show_move_options,ConfirmStep,W)))).

view_control_buttons(W):-
	turn(2),buttonFlag(0),
	retract(buttonFlag(_)),
	assert(buttonFlag(1)),
	new(ConfirmStep,bitmap('confirmStepLeft.bmp')),
	send(W,display,ConfirmStep,point(0,0)),
	send(ConfirmStep,recogniser,click_gesture(left,'',single,and(message(@prolog,show_move_options,ConfirmStep,W)))).

show_move_options(Button,W):-
	free(Button),
	retract(buttonFlag(_)),
	assert(buttonFlag(0)),

	check_legality,

	send(W,clear),
	view_board(W),
	view_scoring(W),
	drawNoRecWhite(W),
	drawNoRecBlack(W),
	restart_hlBalls_maat(W),

	mark_move_and_push_options(W).


%In case of check_legality>false.
show_move_options(_,_):-
	open_error_dialog_choice,

	moveList(ML),
	clear_hlBalls(ML),
	reset_moveList.

reset_moveList:-
	retract(moveList(_)),
	assert(moveList([])).

clear_hlBalls([]).

clear_hlBalls([(_,_,CurrBall)|B]):-
	free(CurrBall),
	clear_hlBalls(B).

restart_hlBalls_maat(W):-
	moveList(ML),
	restart_hlBalls(ML,NML,W),
	retract(moveList(_)),
	assert(moveList(NML)).

restart_hlBalls([],[],_).

restart_hlBalls([(X,Y,_)|B],[(X,Y,HLBall)|C],W):-
	turn(1),
	new(HLBall,bitmap('hlRedBall.bmp')),
	findGraphicalCoords(X,Y,CoordX,CoordY),
	send(W,display,HLBall,point(CoordX,CoordY)),
	restart_hlBalls(B,C,W).

restart_hlBalls([(X,Y,_)|B],[(X,Y,HLBall)|C],W):-
	turn(2),
	new(HLBall,bitmap('hlGreenBall.bmp')),
	findGraphicalCoords(X,Y,CoordX,CoordY),
	send(W,display,HLBall,point(CoordX,CoordY)),
	restart_hlBalls(B,C,W).

check_legality:-
	moveList(MList),
	sort(MList,SMList),
	retract(moveList(_)),
	assert(moveList(SMList)),


	length(MList,Len),
	Len>0,Len=<3,

	((horiz_leg(SMList),retract(arrangementType(_)),assert(arrangementType(1)));
	(numDiag_leg(SMList),retract(arrangementType(_)),assert(arrangementType(2)));
	(mixDiag_leg(SMList)),retract(arrangementType(_)),assert(arrangementType(3))).


horiz_leg([_|[]]).

horiz_leg([(X,Y,_)|B]):-
	nth1(1,B,(NX,Y,_)),
	NX is X+1,
	horiz_leg(B).


numDiag_leg([_|[]]).

numDiag_leg([(X,Y,_)|B]):-
	nth1(1,B,(X,NY,_)),
	next_char(Y,NY),
	numDiag_leg(B).


mixDiag_leg([_|[]]).

mixDiag_leg([(X,Y,_)|B]):-
	nth1(1,B,(NX,NY,_)),
	NX is X+1,
	next_char(Y,NY),
	mixDiag_leg(B).

%$$$$$$$$$$$$$$$$$$$$$$$$$$$

%marking move options for a single ball.
mark_move_and_push_options(W):-
	turn(1),
	moveList([(X,Y,_)]),
	list_of_free_cells_around_ball(X,Y,FCL),

	((length(FCL,0),stuck(W));
	(highLightAvailCell(W,FCL))).

mark_move_and_push_options(W):-
	turn(2),
	writeln('llll'),
	moveList([(X,Y,_)]),
	list_of_free_cells_around_ball(X,Y,FCL),
		writeln(FCL),

	length(FCL,Len),
	((Len is 0,stuck(W));
	(LLen is Len-1,((LLen is 0,Random is 1);(Random is random(LLen)+1)),writeln("Random:"+Random),nth1(Random,FCL,(NX,NY)),inline(W,NX,NY))).

%marking move and push options for set of balls.

mark_move_and_push_options(W):-
	turn(1),
	scan_inline(W,SBroadsideMove,InLineListLen),
	scan_broadside(W,SBroadsideMove,CheckValBM),
	scan_push(W,CheckValPush),
	writeln('InLine Options:'+InLineListLen+'BroadSide:'+CheckValBM+'Push:'+CheckValPush),
	(((InLineListLen \= 0);(CheckValBM \= 0);(CheckValPush \= 0));(stuck(W))).

scan_inline(W,SBroadsideMove,InLineListLen):-
	moveList(ML),
	list_of_free_cells_around_set(ML,FCL),

	((arrangementType(1),findall((X,Y),(member((X,Y),FCL),append([(X,Y,_)],ML,CML),sort(CML,SCML),horiz_leg(SCML)),InlineMove));
	(arrangementType(2),findall((X,Y),(member((X,Y),FCL),append([(X,Y,_)],ML,CML),sort(CML,SCML),numDiag_leg(SCML)),InlineMove));
	(arrangementType(3),findall((X,Y),(member((X,Y),FCL),append([(X,Y,_)],ML,CML),sort(CML,SCML),mixDiag_leg(SCML)),InlineMove))),
	highLightAvailCell(W,InlineMove),
	subtract(FCL,InlineMove,BroadsideMove),
	sort(BroadsideMove,SBroadsideMove),
	length(InlineMove,InLineListLen).

scan_push(W,CheckVal):-
	turn(1),
	moveList(ML),
	whiteList(WL),

	((arrangementType(1),findall((X,Y),(member((X,Y),WL),append([(X,Y,_)],ML,CML),sort(CML,SCML),horiz_leg(SCML)),PushOpt));
	(arrangementType(2),findall((X,Y),(member((X,Y),WL),append([(X,Y,_)],ML,CML),sort(CML,SCML),numDiag_leg(SCML)),PushOpt));
	(arrangementType(3),findall((X,Y),(member((X,Y),WL),append([(X,Y,_)],ML,CML),sort(CML,SCML),mixDiag_leg(SCML)),PushOpt))),

	((length(PushOpt,0),CheckVal is 0);
	(build_push_list_maat(PushOpt,PushList),
	fail_Push_List_combined_colour(PushList,NPushList),
	fail_Push_List_too_long(ML,NPushList,NNPushList),
	((length(NNPushList,0),CheckVal is 0);
	(retract(pushList(_)),
	 assert(pushList(NNPushList)),
	 view_Push_options(W,NNPushList,1),CheckVal is 1)))).

scan_push(W,CheckVal):-
	turn(2),
	moveList(ML),
	blackList(BL),

	((arrangementType(1),findall((X,Y),(member((X,Y),BL),append([(X,Y,_)],ML,CML),sort(CML,SCML),horiz_leg(SCML)),PushOpt));
	(arrangementType(2),findall((X,Y),(member((X,Y),BL),append([(X,Y,_)],ML,CML),sort(CML,SCML),numDiag_leg(SCML)),PushOpt));
	(arrangementType(3),findall((X,Y),(member((X,Y),BL),append([(X,Y,_)],ML,CML),sort(CML,SCML),mixDiag_leg(SCML)),PushOpt))),

	((length(PushOpt,0),CheckVal is 0);
	(build_push_list_maat(PushOpt,PushList),
	fail_Push_List_combined_colour(PushList,NPushList),
	fail_Push_List_too_long(ML,NPushList,NNPushList),
	((length(NNPushList,0),CheckVal is 0);
	(retract(pushList(_)),
	 assert(pushList(NNPushList)),
	 view_Push_options(W,NNPushList,1),CheckVal is 1)))).


build_push_list_maat([],[]).
build_push_list_maat([Opt|OtherOpts],[CurrList|B]):-
	appended_black_and_white(AL),
	moveList(ML),
	findall((X,Y),member((X,Y,_),ML),NML),
	subtract(AL,NML,SubList),

	build_push_list(SubList,Opt,PushList),
	append(PushList,[Opt],CurrList),
	build_push_list_maat(OtherOpts,B).


build_push_list(SubList,(X,Y),[(NX,NY)|B]):-
	((arrangementType(1),member((NX,NY),SubList),append([(NX,NY,_)],[(X,Y,_)],Check),sort(Check,SCheck),horiz_leg(SCheck));
	(arrangementType(2),member((NX,NY),SubList),append([(NX,NY,_)],[(X,Y,_)],Check),sort(Check,SCheck),numDiag_leg(SCheck));
	(arrangementType(3),member((NX,NY),SubList),append([(NX,NY,_)],[(X,Y,_)],Check),sort(Check,SCheck),mixDiag_leg(SCheck))),
	  select((X,Y),SubList,NSubList),
	  build_push_list(NSubList,(NX,NY),B).

build_push_list(_,_,[]).

fail_Push_List_combined_colour(PushList,NPushList):-
	findall(List,(member(List,PushList),not_comb_colour(List)),NPushList).

not_comb_colour(List):-
	blackList(BL),
	whiteList(WL),
	intersection(BL,List,Int1),
	intersection(WL,List,Int2),
	((length(Int1,0),length(Int2,Len2),Len2\=0);(length(Int2,0),length(Int1,Len1),Len1\=0)).

fail_Push_List_too_long(ML,NPushList,NNPushList):-
	length(ML,MLLen),
	findall(List,(member(List,NPushList),length(List,Len),Len<MLLen),NNPushList).

view_Push_options(_,[],_).
view_Push_options(W,[Opt|OtherOpt],N):-
	(member((X,Y),Opt),
	((turn(1), new(BMP,bitmap('PushGreenBall.bmp')));
	(turn(2), new(BMP,bitmap('PushRedBall.bmp')))),
	findGraphicalCoords(X,Y,CoordX,CoordY),
	send(W,display,BMP,point(CoordX,CoordY)),
	send(BMP,recogniser,click_gesture(left,'',single,and(message(@prolog,push,W,N)))),
	fail);
	NN is N+1,
	view_Push_options(W,OtherOpt,NN).

push(W,N):-
	turn(1),
	tempWin(TW),
	free(TW),
	moveList(ML),
	pushList(PL),
	blackList(BL),
	whiteList(WL),

	nth1(N,PL,ChosenPushOpt),
	findall((MLX,MLY),member((MLX,MLY,_),ML),NML),
	sort(ChosenPushOpt,SCPO),
	last(NML,LNML),last(SCPO,LSCPO),
	findHigherValCell(LNML,LSCPO,Max),
	((member(Max,NML),(reverse(NML,RNML),reverse(SCPO,RSCPO),pushedList1(RNML,PNML),pushedList1(RSCPO,PSCPO)));
	(member(Max,SCPO),pushedList2(NML,PNML),pushedList2(SCPO,PSCPO))),

	subtract(BL,NML,NBL),append(NBL,PNML,FinalBL),
	subtract(WL,SCPO,NWL),append(NWL,PSCPO,FinalWL),

	subtract(FinalBL,[('*','*')],NFinalBL),
	subtract(FinalWL,[('*','*')],NFinalWL),

	retract(blackList(_)),
	assert(blackList(NFinalBL)),
	retract(whiteList(_)),
	assert(whiteList(NFinalWL)),
	switchPlayers(W).

push(W,N):-
	turn(2),
	tempWin(TW),
	free(TW),
	moveList(ML),
	pushList(PL),
	blackList(BL),
	whiteList(WL),

	nth1(N,PL,ChosenPushOpt),
	findall((MLX,MLY),member((MLX,MLY,_),ML),NML),
	sort(ChosenPushOpt,SCPO),
	last(NML,LNML),last(SCPO,LSCPO),
	findHigherValCell(LNML,LSCPO,Max),

	((member(Max,NML),(reverse(NML,RNML),reverse(SCPO,RSCPO),pushedList1(RNML,PNML),pushedList1(RSCPO,PSCPO)));
	(member(Max,SCPO),pushedList2(NML,PNML),pushedList2(SCPO,PSCPO))),

	subtract(WL,NML,NWL),append(NWL,PNML,FinalWL),
	subtract(BL,SCPO,NBL),append(NBL,PSCPO,FinalBL),

	subtract(FinalBL,[('*','*')],NFinalBL),
	subtract(FinalWL,[('*','*')],NFinalWL),

	retract(blackList(_)),
	assert(blackList(NFinalBL)),
	retract(whiteList(_)),
	assert(whiteList(NFinalWL)),
	switchPlayers(W).


pushedList1([(X,Y)],[(Xn,Yn)]):-
	((arrangementType(1),ValX is X-1,ValY=Y);
	(arrangementType(2),ValX is X,previous_char(ValY,Y));
	(arrangementType(3),ValX is X-1,previous_char(ValY,Y))),

	((not(validCell(ValX,ValY)),Xn = '*',Yn = '*',scoring(BS,WS),
	((turn(1),NBS is BS+1,set_scoring(NBS,WS));(turn(2),NWS is WS+1,set_scoring(BS,NWS))));
	(Xn is ValX, Yn = ValY)).


pushedList1([_|Other],[(NX,NY)|B]):-
	nth1(1,Other,(NX,NY)),
	pushedList1(Other,B).

pushedList2([(X,Y)],[(Xn,Yn)]):-
	((arrangementType(1),ValX is X+1,ValY=Y);
	(arrangementType(2),ValX is X,next_char(Y,ValY));
	(arrangementType(3),ValX is X+1,next_char(Y,ValY))),

	((not(validCell(ValX,ValY)),Xn = '*',Yn = '*',scoring(BS,WS),
	((turn(1),NBS is BS+1,set_scoring(NBS,WS));(turn(2),NWS is WS+1,set_scoring(BS,NWS))));
	(Xn is ValX, Yn = ValY)).

pushedList2([_|Other],[(NX,NY)|B]):-
	nth1(1,Other,(NX,NY)),
	pushedList2(Other,B).

validCell(ValX,ValY):-
	legal_Y_Indexes(LY),
	member(ValY,LY),
	line_info(ValY,MinX,MaxX,_,_),
	ValX>=MinX,ValX=<MaxX.


scan_broadside(W,SBroadsideMove,CheckVal):-
	arrangementType(1),
	moveList([(_,Y,_)|_]),
	next_char(Y,NY),
	previous_char(PY,Y),
	findall((ValX,PY),member((ValX,PY),SBroadsideMove),RightList),
	findall((ValX,NY),member((ValX,NY),SBroadsideMove),LeftList),
	broadside_option_list(W,RightList,LeftList,CheckVal).

scan_broadside(W,SBroadsideMove,CheckVal):-
	arrangementType(2),
	moveList([(X,_,_)|_]),
	NX is X+1,
	PX is X-1,
	findall((NX,ValY),member((NX,ValY),SBroadsideMove),RightList),
	findall((PX,ValNY),member((PX,ValNY),SBroadsideMove),LeftList),
	broadside_option_list(W,RightList,LeftList,CheckVal).

scan_broadside(W,SBroadsideMove,CheckVal):-
	arrangementType(3),
	moveList(ML),
	findall((ValX,ValY),(member((ValX,ValY),SBroadsideMove),((next_char(ValY,YML),member((ValX,YML,_),ML));(XML is (ValX-1),member((XML,ValY,_),ML)))),RightList),
	subtract(SBroadsideMove,RightList,LeftList),
	broadside_option_list(W,RightList,LeftList,CheckVal).


broadside_option_list(W,RightList,LeftList,CheckVal):-
	sort(RightList,SRightList),
	sort(LeftList,SLeftList),

	moveList(ML),
	length(ML,MLlen),
	subList(SRightList,MLlen,Opt1),
	subList(SLeftList,MLlen,Opt2),

	reverse(SRightList,RevSRightList),
	reverse(SLeftList,RevSLeftList),

	subList(RevSRightList,MLlen,Opt3),
	subList(RevSLeftList,MLlen,Opt4),

	sort(Opt1,SOpt1),
	sort(Opt2,SOpt2),
	sort(Opt3,SOpt3),
	sort(Opt4,SOpt4),

	append([SOpt1],[SOpt2],List1),
	append(List1,[SOpt3],List2),
	append(List2,[SOpt4],FinalList),

	sort(FinalList,SFinalList),
	delete(SFinalList,[],SSFinalList),

	find_only_valid(SSFinalList,SSSSFinalList),

	((not(highLight_broadside_options_maat(W,SSSSFinalList)),CheckVal is 0);(CheckVal is 1)).

find_only_valid(List,OnlyValidOptList):-
	findall(L,(member(L,List),compatibilityConvertion(L,NL),(horiz_leg(NL);numDiag_leg(NL);mixDiag_leg(NL))),OnlyValidOptList).

compatibilityConvertion(L,NL):-
	findall((X,Y,_),member((X,Y),L),NL).

highLight_broadside_options_maat(W,FinalList):-
	length(FinalList,N),
	not(N is 0),
	WinLen is 231*N,
	new(TempWin,window('Choose An Option:',size(WinLen,231))),
	send(TempWin,open),
	retract(tempWin(_)),
	assert(tempWin(TempWin)),
	retract(broadSideList(_)),
	assert(broadSideList(FinalList)),
	highLight_broadside_options(W,TempWin,FinalList,0).

highLight_broadside_options(_,_,[],_).

highLight_broadside_options(W,TempWin,[CurrList|OtherLists],N):-
	  ((turn(1),Colour = 'red');(turn(2),Colour = 'green')),
	   moveList(ML),
	   ValX is 231*N,
	   NN is N+1,
	   new(Board,bitmap('minBoard.bmp')),
	   send(TempWin,display,Board,point(ValX,0)),
	   new(Box,box(232,232)),
	   send(TempWin,display,Box,point(ValX,0)),
	   draw_minimized(TempWin,ML,Colour,N),
	   draw_minimized(TempWin,CurrList,'turquoise',N),
	   send(Box,recogniser,click_gesture(left,'',single,and(message(@prolog,broadSide,W,TempWin,N)))),
	   highLight_broadside_options(W,TempWin,OtherLists,NN).


draw_minimized(_,[],_,_).

draw_minimized(TempWin,[(X,Y)|B],Colour,N):-
	findGraphicalCoords(X,Y,CoordX,CoordY),
	NCoordX is (CoordX/3)+(231*N),
	NCoordY is (CoordY/3),
	new(Circle,circle(11)),
	send(Circle,fill_pattern(colour(Colour))),
	send(Circle,colour,Colour),
	send(TempWin,display,Circle,point(NCoordX,NCoordY)),
	draw_minimized(TempWin,B,Colour,N).

draw_minimized(TempWin,[(X,Y,_)|B],Colour,N):-
	findGraphicalCoords(X,Y,CoordX,CoordY),
	NCoordX is (CoordX/3)+(231*N),
	NCoordY is (CoordY/3),
	new(Circle,circle(11)),
	send(Circle,fill_pattern(colour(Colour))),
	send(Circle,colour,Colour),
	send(TempWin,display,Circle,point(NCoordX,NCoordY)),
	draw_minimized(TempWin,B,Colour,N).

broadSide(W,TempWin,N):-
	turn(1),
	free(TempWin),
	broadSideList(FinalList),
	blackList(BL),
	nth0(N,FinalList,BSList),
	moveList(ML),
	findall((X,Y),member((X,Y,_),ML),NML),
	subtract(BL,NML,NBL),
	append(NBL,BSList,NNBL),
	retract(blackList(_)),
	assert(blackList(NNBL)),
	switchPlayers(W).


broadSide(W,TempWin,N):-
	turn(2),
	free(TempWin),
	broadSideList(FinalList),
	whiteList(WL),
	nth0(N,FinalList,BSList),
	moveList(ML),
	findall((X,Y),member((X,Y,_),ML),NML),
	subtract(WL,NML,NWL),
	append(NWL,BSList,NNWL),
	retract(whiteList(_)),
	assert(whiteList(NNWL)),
	switchPlayers(W).


list_of_free_cells_around_ball(X,Y,FreeCellList):-
	appended_black_and_white(AL),
	retractall(tempCell(_,_)),

	PrevXVal is X-1,
	NextXVal is X+1,
	previous_char(PrevYVal,Y),
	next_char(Y,NextYVal),

	assert(tempCell(NextXVal,Y)),
	assert(tempCell(NextXVal,NextYVal)),
	assert(tempCell(X,NextYVal)),
	assert(tempCell(PrevXVal,Y)),
	assert(tempCell(PrevXVal,PrevYVal)),
	assert(tempCell(X,PrevYVal)),

	findall((ValX,ValY),(tempCell(ValX,ValY),availCell(ValX,ValY,AL)),FreeCellList).

list_of_free_cells_around_set([],[]).

list_of_free_cells_around_set([(X,Y,_)|B],FreeCellList):-
	list_of_free_cells_around_ball(X,Y,FreeCellListAroundSingleBall),
	list_of_free_cells_around_set(B,C),
	union(FreeCellListAroundSingleBall,C,FreeCellList).



availCell(ValX,ValY,AL):-
	legal_Y_Indexes(LY),
	member(ValY,LY),
	line_info(ValY,MinX,MaxX,_,_),
	ValX>=MinX,ValX=<MaxX,
	not((member((X,Y),AL),(ValX is X, ValY == Y))).


stuck(W):-
	turn(1),
	open_error_dialog_stuck,
	send(W,clear),
	view_board(W),
	view_scoring(W),
	drawNoRecWhite(W),
	drawWithRecBlack(W),
	reset_moveList.

stuck(W):-
	turn(2),
	send(W,clear),
	view_board(W),
	view_scoring(W),
	drawNoRecWhite(W),
	drawNoRecBlack(W),
	reset_moveList,
	select_balls(W,_,_).

highLightAvailCell(_,[]).
highLightAvailCell(W,[(X,Y)|B]):-
	findGraphicalCoords(X,Y,CoordX,CoordY),
	NCoordX is CoordX+12,
	NCoordY is CoordY+12,
	new(Circle,circle(15)),
	send(Circle,fill_pattern(colour(turquoise))),
	send(Circle,colour,turquoise),
	send(W,display,Circle,point(NCoordX,NCoordY)),
	send(Circle,recogniser,click_gesture(left,'',single,and(message(@prolog,inline,W,X,Y)))),
	highLightAvailCell(W,B).


inline(W,NX,NY):-
	turn(1),
	blackList(BL),
	moveList([(X,Y,_)]),
	select((X,Y),BL,NBL),
	append([(NX,NY)],NBL,NNBL),
	retract(blackList(_)),
	assert(blackList(NNBL)),
	switchPlayers(W).

inline(W,NX,NY):-
	turn(1),
	tempWin(TempWin),
	free(TempWin),
	blackList(BL),
	moveList(ML),
	append([(NX,NY,_)],ML,Temp),
	sort(Temp,STemp),
	nth1(1,STemp,(FX,FY,_)),
	last(STemp,(LX,LY,_)),
	append([(NX,NY)],BL,NBL),
	(((not((FX is NX, FY == NY)),select((FX,FY),NBL,NNBL));(not((LX is NX, LY == NY)),select((LX,LY),NBL,NNBL))),
	retract(blackList(_)),
	assert(blackList(NNBL))),
	switchPlayers(W).

inline(W,NX,NY):-
	turn(2),
	whiteList(WL),
	moveList([(X,Y,_)]),
	select((X,Y),WL,NWL),
	append([(NX,NY)],NWL,NNWL),
	retract(whiteList(_)),
	assert(whiteList(NNWL)),
	switchPlayers(W).

inline(W,NX,NY):-
	turn(2),
	tempWin(TempWin),
	free(TempWin),
	whiteList(WL),
	moveList(ML),
	append([(NX,NY,_)],ML,Temp),
	sort(Temp,STemp),
	nth1(1,STemp,(FX,FY,_)),
	last(STemp,(LX,LY,_)),
	append([(NX,NY)],WL,NWL),
	(((not((FX is NX, FY == NY)),select((FX,FY),NWL,NNWL));(not((LX is NX, LY == NY)),select((LX,LY),NWL,NNWL))),
	retract(whiteList(_)),
	assert(whiteList(NNWL))),
	switchPlayers(W).


switchPlayers(W):-
	turn(1),
	send(W,clear),
	view_board(W),
	not(checkWin(W)),
	view_scoring(W),
	drawNoRecWhite(W),
	drawNoRecBlack(W),
	reset_moveList,
	retract(turn(_)),
	assert(turn(2)),
	select_balls(W,_,_).


switchPlayers(W):-
	turn(2),
	send(W,clear),
	view_board(W),
	not(checkWin(W)),
	view_scoring(W),
	drawNoRecWhite(W),
	drawWithRecBlack(W),
	reset_moveList,
	retract(turn(_)),
	assert(turn(1)).


checkWin(W):-
	scoring(_,6),
	writeln('The winner is Comp').

checkWin(W):-
	scoring(6,_),
	writeln('The winner is Human').

