% Setting up

% pegtime = Check the execution time of the program with different boards (NOT AS EFFECTIVE IF DISPLAYING STEP BY STEP)

pegtime(X) :-
  statistics(walltime, [_| []]),
  peg(X), 
  statistics(walltime, [_ | [ExecutionTime]]),
  nl,write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

% UNCOMMENT IN ORDER TO TEST WITH OR WITHOUT INDEPENDENCE AND PAGODA (always comment two of the three).
/*
 peg(X):-
 1. solitaire_steps(...) Only independence
 2. solitaire_steps(...) Brute force
 3. solitaire_steps(...) Both pagoda and independence
*/

% Visualize answer, there exists two methods (always comment one of the two):
/*
1. write(X) = prints the list of moves  <---- RECOMMENDED FOR TIME TESTING
2. displayMoves = shows the execution of the obtained list of moves
   by applying move to the board step by step    <-- RECOMMENDED FOR STEP BY STEP CHECKING
*/
peg(crossbow) :-
 solitaire_steps([31,32,34,35,41,42,43,44,45,53], X,[], [3]),
 %solitaire_steps([31,32,34,35,41,42,43,44,45,53], X, [3]),
 %solitaire_steps(cross,[31,32,34,35,41,42,43,44,45,53], X,[], [3]),
 display_initialboard([31,32,34,35,41,42,43,44,45,53], [3]),
 nl,
 displayMoves([31,32,34,35,41,42,43,44,45,53],X).
 %write(X).

peg(longbow) :-
 solitaire_steps([20,26,30,31,33,35,36,41,43,45,52,53,54,63], X,[], [3]),
 %solitaire_steps([20,26,30,31,33,35,36,41,43,45,52,53,54,63], X, [3]),
 %solitaire_steps(long,[20,26,30,31,33,35,36,41,43,45,52,53,54,63], X,[], [3]),
 display_initialboard([20,26,30,31,33,35,36,41,43,45,52,53,54,63], [3]),
  nl,
 displayMoves([20,26,30,31,33,35,36,41,43,45,52,53,54,63],X).
 %write(X).

% notquitedead has no execution without independence or pagoda since it wont return the result in
% a reasonable amount of time (aka due date)
peg(notquitedead) :-
 %solitaire_steps([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], X,[], [33]),
  solitaire_steps(nqd,[2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], X,[], [33]),
 display_initialboard([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], [33]),
 nl,
 displayMoves([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64],X).
 %write(X).

peg(halfdead) :-
 %solitaire_steps([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64], X,[], [33]),
 %solitaire_steps([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64], X, [33]),
 solitaire_steps(half,[20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64], X,[], [33]),
 display_initialboard([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64], [33]),
 nl,
 displayMoves([20,22,23,24,30,34,35,40,41,42,43,44,45,52,54,62,64],X).
 %write(X).

% same as notquitedead, full takes an unreasonable mount of time to execute withou independe or pagoda
peg(full) :-
 %solitaire_steps([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], X,[], [33]),
 %solitaire_steps([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], X, [33]),
 solitaire_steps(fu,[2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], X,[], [33]),
 display_initialboard([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], [33]),
  nl,
  displayMoves([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64],X).
 %write(X).

%display the board
display_initialboard(BOARD, GOAL):-
   write("  | a | b | c | d | e | f | g |"), write("\n"),
   write("1 |   |   |"),checkline([2,3,4],BOARD, GOAL) ,write("   |   |"), write("\n"), 
   write("2 |   |   |"),checkline([12,13,14],BOARD, GOAL), write("   |   |"), write("\n"),
   write("3 |"),checkline([20,21,22,23,24,25,26],BOARD, GOAL), write("\n"), 
   write("4 |"),checkline([30,31,32,33,34,35,36],BOARD, GOAL), write("\n"),
   write("5 |"),checkline([40,41,42,43,44,45,46],BOARD, GOAL), write("\n"),
   write("6 |   |   |"),checkline([52,53,54],BOARD, GOAL), write("   |   |"), write("\n"),
   write("7 |   |   |"),checkline([62,63,64],BOARD, GOAL), write("   |   |"), write("\n"), write("\n"), write("\n").

display_board(BOARD):-
   write("  | a | b | c | d | e | f | g |"), write("\n"),
   write("1 |   |   |"),checkline2([2,3,4],BOARD) ,write("   |   |"), write("\n"), 
   write("2 |   |   |"),checkline2([12,13,14],BOARD), write("   |   |"), write("\n"),
   write("3 |"),checkline2([20,21,22,23,24,25,26],BOARD), write("\n"), 
   write("4 |"),checkline2([30,31,32,33,34,35,36],BOARD), write("\n"),
   write("5 |"),checkline2([40,41,42,43,44,45,46],BOARD), write("\n"),
   write("6 |   |   |"),checkline2([52,53,54],BOARD), write("   |   |"), write("\n"),
   write("7 |   |   |"),checkline2([62,63,64],BOARD), write("   |   |"), write("\n"), write("\n").

checkline([],_, _).
checkline([H|Rest],B, [G]):-
   (member(H,B) 
    -> write(' x |'),checkline(Rest,B, [G]);
    H is G
    -> write(' o |'),checkline(Rest,B, [G]));
    write('   |'), 
    checkline(Rest,B, [G]).

checkline2([],_).
checkline2([H|Rest],B):-
   (member(H,B) 
    -> write(' x |'),checkline2(Rest,B)
   );
    write('   |'), 
    checkline2(Rest,B).

displayMoves(_,[]).
displayMoves(SB, [Mv | T]) :-
    solitaire_move(SB, Mv, NB),
    display_board(NB),
    % write("Prompt"),nl, get(_),
    displayMoves(NB, T).  


%helper functions
onboard(N) :- 2 =< N, N =< 4.
onboard(N) :- 12 =< N, N =< 14.
onboard(N) :- 20 =< N, N =< 26.
onboard(N) :- 30 =< N, N =< 36.
onboard(N) :- 40 =< N, N =< 46.
onboard(N) :- 52 =< N, N =< 54.
onboard(N) :- 62 =< N, N =< 64.

% jump to the right
jump(Start, Jumped, End) :-
    Jumped is Start + 1,
    End is Start + 2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

%jump to the left
jump(Start, Jumped, End) :-
    Jumped is Start - 1,
    End is Start - 2,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

%jump down
jump(Start, Jumped, End) :-
    Jumped is Start + 10,
    End is Start + 20,
    onboard(Start),
    onboard(Jumped),
    onboard(End).

%jump up
jump(Start, Jumped, End) :-
    Jumped is Start - 10,
    End is Start - 20,
    onboard(Start), %checking bounds
    onboard(Jumped),
    onboard(End).



solitaire_move(SB, (Start, End), [End|SB2]) :-
    select(Start, SB, SB1),
    jump(Start, Jumped, End),
    select(Jumped, SB1, SB2),
    not(member(End,SB2)). % if the space End is empty

  
% TODO implement and add pagoda functions HERE

% 1. RUN WITHOUT INDEPENDENCE OR PAGODA
solitaire_steps(GB, [], GB).
solitaire_steps(SB, [Mv|Moves], GB) :-
    solitaire_move(SB, Mv, SB1),
    solitaire_steps(SB1, Moves, GB).

    

% 2. RUN WITH INDEPENDENCE BUT NO PAGODA

solitaire_steps(GB, [],_, GB).
solitaire_steps(SB, [Mv|Moves],Hist, GB) :-
    solitaire_move(SB, Mv, NewBoard),
    independent(Mv,Hist),
    solitaire_steps(NewBoard, Moves,[Mv|Hist], GB).

% 3. RUN WITH INDEPENDENSE AND PAGODA 

solitaire_steps(Gname,GB, [],_, GB).
solitaire_steps(Gname,SB, [Mv|Moves],Hist, GB) :-
    solitaire_move(SB, Mv, NewBoard),
    independent(Mv,Hist),
     findall((P,W),(member(P,[asym,asymR]), %list of pagoda functions
                wgt(P,NewBoard,W)), Wgts),
    check_wgts(Gname,Wgts),
    
    solitaire_steps(Gname,NewBoard, Moves,[Mv|Hist], GB).
/*
solitaire_steps(G,B,_,[]) :- final_board(G,B).
solitaire_steps(G,B,Hist,[Mv | Moves]) :-
    make_jump(B, Start, Jumped, End, NewBoard),
    mv = (Start,End),
    independence_check(Mv,Hist),
    findall((P,W),(member(P,[simple,...]), %list of pagoda functions
                wgt(P,NewBoard,W)), Wgts),
    check_wgts(G,Wgts),
    solitaire_steps(G,NewBoard,[Mv | Hist], Moves).
*/

% Pagoda Values Functions-------------------------------

% asymmectric center weigthing
pagoda(asym,13,1).
pagoda(asym,20,-1).
pagoda(asym,21,1).
pagoda(asym,23,1).
pagoda(asym,25,1).
pagoda(asym,26,-1).
pagoda(asym,31,2).
pagoda(asym,33,2).
pagoda(asym,35,2).
pagoda(asym,40,-1).
pagoda(asym,41,1).
pagoda(asym,43,1).
pagoda(asym,45,1).
pagoda(asym,46,-1).
pagoda(asym,53,1).

% asymmetric center weigthing rotated
pagoda(asymR,2,-1).
pagoda(asymR,4,-1).
pagoda(asymR,12,1).
pagoda(asymR,13,2).
pagoda(asymR,14,1).
pagoda(asymR,31,1).
pagoda(asymR,32,1).
pagoda(asymR,33,2).
pagoda(asymR,34,1).
pagoda(asymR,35,1).
pagoda(asymR,52,1).
pagoda(asymR,53,2).
pagoda(asymR,54,1).
pagoda(asymR,62,-1).
pagoda(asymR,64,-1).

goal_wgt(cross,asym,2).
goal_wgt(long,asym,2).
goal_wgt(nqd,asym,2).
goal_wgt(half,asym,2).
goal_wgt(fu,asym,2).

goal_wgt(cross,asymR,2).
goal_wgt(long,asymR,2).
goal_wgt(nqd,asymR,2).
goal_wgt(half,asymR,2).
goal_wgt(fu,asymR,2).


% wgt(P, B, Wgt) % -> wgt(+,+,-)
wgt(_,[], 0).
wgt(P,[Pos| Rest], Wgt) :-
    (pagoda(P,Pos,PWgt) ; PWgt = 0), !,
    wgt(P,Rest,WgtRest),
    Wgt is WgtRest + PWgt.

check_wgts(G,[]).
check_wgts(G,[(P,WgtP) | Rest]) :-
    goal_wgt(G,P,WgtGoal),
    WgtP >= WgtGoal,
    check_wgts(G,Rest).

% Independence-------------------------

overlap((A,B),(C,D)):-
   jump(A,JU1,B),
   jump(C,JU2,D),
   (A = C;
    A = JU1;
    A = D;
    JU1 = C;
    JU1 = JU2;
    JU1 = D;
    B = C;
    B = JU2;
    B = D).
 
lexorder((A,_),(C,_)):-
   A < C.


/*
lexorder((A,B),(C,D)):-
  jump(A,E,B),
  jump(C,F,D),
  X is A + B ,
  Y is C + D ,
  X =< Y.
*/

%Code from lectures
independent(_,[]).
independent(Mv,[H|_]) :-
   overlap(Mv, H), !.
independent(Mv,[H|T]) :-
   lexorder(Mv,H),
   independent(Mv,T).



/*

peg(crossbow)

project the starting board the goal

Display solution


Pagoda functions
----------------
Associate to each position a weight, the sum of the weigts where a peg sits.

weight(simple, Board, weight)
Wght = 4

Pagoda functions have the property that when a jump is performed 
weight of the board never increases.

If THE WEIGHT OF THE BOARD IS LESS THAN THE WEIGHT OF THE GOAL
position you can stop the search.

EXAMPLE OF PAGODA
pagoda(simple,13,1).
pagoda(simple,31,1).
pagoda(simple,33,1).
pagoda(simple,35,1).
pagoda(simple,43,1).

goal_wgt(full,simple,1).
goal_wgt(crossbow,simple,0).

Calculating the weight of a board 
wgt(P,B,Wgt)
wgt(P,[Pos|Rest],Wgt):-
  pagoda(P,Pos,PWgt);
  PWght = 0, !,
  wgt(P,REst,WgtRest)
  wgt is WgtRest+PWgt


goal_wgt(G,P, GoalWgt).

        name of game   Path to goal
                v      v
solitaire_steps(G,B,_,[]) :- final_board(G,B).
                    ^ 
                  THis starts empy (history of states)

solitare_steps (G,B,HList,[Mv|Moves]):-
   make_jump(B,Start,JUmped,End,NewBOard),
   MV = (Start, End),
   independence_Check(Mv, HList),
   findall((P,W), (member(P,[simple,...]),
   wgt(P,NewBoard,W)),Wgts),
   check_wgts(G,Wgts),
   solitare_steps(G,NewBoard,[Mv|HList]Moves).


check_wgts (G, []).
check_wgts(G,[P,wgts)|Rest) :-
  goal_wgt(G,P,WgtGoal),
  WgtP >= WgtGoal,
  check_wgts(G,Rest). 

*/
