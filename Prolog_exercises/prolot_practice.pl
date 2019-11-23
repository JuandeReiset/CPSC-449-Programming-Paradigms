
% ancestor(X,Y) is true when X is ancestor of Y

mother(sally, tina). % sally is mother of tina
father(doug, tina). % doug is father of tina
mother(mary, sally). 
father(john, sally).
mother(clara, mary).
father(san, mary).
mother(betty,john).
father(sam,john).
ancestor(X,Y) :- mother(X,Y).
ancestor(X,Y) :- father (X,Y).
ancestor(X,Y) :- mother(Z,Y) , ancestor (X,Z).
ancestor(X,Y) :- father (Z,Y), ancestor (X,Z).
