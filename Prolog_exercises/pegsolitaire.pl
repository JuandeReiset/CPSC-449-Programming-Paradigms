% Setting up
/*  % FUll game

Questions:
depth first search
1.Representation of board for filled and empty positions
2. Specification for desired output. WHat is shown step by step?
every board move
3. Is the goal included in the initial state? or is it separate?
4. Explanation of the weighting in Pagoda functions
*/
 
%peg 
/*
peg(GAME,GOAL):-
  display_board(BOARD, GOAL),
  solitaire_steps(GAME,Moves,GOAL),
  write(Moves).
*/

peg(crossbow) :-
 display_initialboard([31,32,34,35,41,42,43,44,45,53], [3]),
 nl,
 solitaire_steps([31,32,34,35,41,42,43,44,45,53], X, [3]),
 write(X).

peg(longbow) :-
 display_initialboard([20,26,30,31,33,35,36,41,43,45,52,53,54,63], [3]),
  nl,
 solitaire_steps([20,26,30,31,33,35,36,41,43,45,52,53,54,63], X, [3]),
 write(X).

peg(notquitedead) :-
 display_initialboard([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], [33]),
 nl,
 solitaire_steps([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64], X, [33]),
 write(X).

peg(full) :-
 display_initialboard([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], [33]),
  nl,
  solitaire_steps([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64], X, [33]),
  write(X).

%display the board
display_initialboard(BOARD, GOAL):-
   write("  | a | b | c | d | e | f | g |"), write("\n"),
   write("1 |   |   |"),checkline([2,3,4],BOARD, GOAL) ,write("   |   |"), write("\n"), 
   write("2 |   |   |"),checkline([12,13,14],BOARD, GOAL), write("   |   |"), write("\n"),
   write("3 |"),checkline([20,21,22,23,24,25,26],BOARD, GOAL), write("\n"), 
   write("4 |"),checkline([30,31,32,33,34,35,36],BOARD, GOAL), write("\n"),
   write("5 |"),checkline([40,41,42,43,44,45,46],BOARD, GOAL), write("\n"),
   write("6 |   |   |"),checkline([52,53,54],BOARD, GOAL), write("   |   |"), write("\n"),
   write("7 |   |   |"),checkline([62,63,64],BOARD, GOAL), write("   |   |"), write("\n").

display_board(BOARD):-
   write("  | a | b | c | d | e | f | g |"), write("\n"),
   write("1 |   |   |"),checkline2([2,3,4],BOARD) ,write("   |   |"), write("\n"), 
   write("2 |   |   |"),checkline2([12,13,14],BOARD), write("   |   |"), write("\n"),
   write("3 |"),checkline2([20,21,22,23,24,25,26],BOARD), write("\n"), 
   write("4 |"),checkline2([30,31,32,33,34,35,36],BOARD), write("\n"),
   write("5 |"),checkline2([40,41,42,43,44,45,46],BOARD), write("\n"),
   write("6 |   |   |"),checkline2([52,53,54],BOARD), write("   |   |"), write("\n"),
   write("7 |   |   |"),checkline2([62,63,64],BOARD), write("   |   |"), write("\n").

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
    
    
%Crossbow

crossbow([31,32,34,35,41,42,43,44,45,53]).
crossgoal([3]).

%Longbow
longbow([20,26,30,31,33,35,36,41,43,45,52,53,54,63]).
longgoal([3]).

%Not quite dead
nqd([2,3,4,12,14,20,21,22,23,24,25,26,30,32,35,36,40,41,42,43,44,45,46,52,54,62,64]).
nqdgoal([33]).

%full game
full([2,3,4,12,13,14,20,21,22,23,24,25,26,30,31,32,34,35,36,40,41,42,43,44,45,46,52,53,54,62,63,64]).
fullgoal([33]).

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
  
% TODO implement and add independence and pagoda functions HERE

solitaire_steps(GB, [], GB).
solitaire_steps(SB, [Mv|Moves], GB) :-
    solitaire_move(SB, Mv, SB1),
    solitaire_steps(SB1, Moves, GB).

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
