% SOLVING PUZZLES

solver(Name,Solution) :- get_puzzle(Name,P), solve_puzzle(P,Solution).
solve_puzzle(puzzle(Clues, Queries, Solution),Solution),Solution) :-
   solve(Clues),
   solve(Queries).

solve([Clue|CLues]) :- Clue, solve(Clues).
solve([]).

get_puzzle(Name,puzzle(Clues,Queries,Solution)) :-
  structure(Name,Structures),
  clues(Name, Structure,Clues),
  queries(Name,Structure,Queries,Solution).
  
/*
Three friends come 1st, 2dn, 3rd in a checkers competition. THey all have distinnct
names, like different sports, and come from different cities.

Mike likes ice hockey and did better than the edmontonian.

Simon who was from Vancouver, did better than the rower.
However the soccer player won the comptetition.

What sport did JUlia play?
Who is from Calgary?
Who won the competition?

REpreseent each friend as a structure
  friend(Name, City, Sport)

*/  

structure(friend,[friend(_,_,_),friend(_,_,_),friend(_,_,)]).

clues(friends,Friends,
  % the clues
  [(did_better(P1Clue1,P2Clue1,Friends),
    name(P1Clue1,mike),sport(P1Clue1,icehockey),
    city(P2CLue1,edmonton))
  % MIke who plays icehockey did better than the Edmontonian
   , (did_better(Man1Clue2, Man2Clue2,vancouver),
      name(Man1CLue2,simon), city(Man1Clue2,vancouver),
      sport(Man2Clue2,rowing))
  % Simon who is from Vancouver did better than the rower
