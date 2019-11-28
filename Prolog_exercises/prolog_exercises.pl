% Exercise #1 predicates

% myappend(X,Y,Z) DETERMINES if Z is Y with X appended at the beginning
myappend([],X,X).
myappend([X|Y],Z, [X|W]) :- myappend(Y,Z,W).

% mymember(X,Y). Determines if single element X is part of Y.
mymember(A,[A|Rest]).
mymember(A,[Y|Rest]) :- mymember(A,Rest).

% Reverse list Y to list X and viceversa
myreverse(Xs, Ys) :-
  myreverse(Xs, [], Ys, Ys).
myreverse([], Ys, Ys, []).
myreverse([X|Xs], Rs, Ys, [_|Bound]) :-
  myreverse(Xs, [X|Rs], Ys, Bound).

% converts  list of lists to a single list
myflatten(List,FlatList) :- 
  myflatten(List, [],FlatList).
myflatten(Var, T, [Var|T]) :-
  var(Var), !.
myflatten([], T, T) :- !.
myflatten([H|T], TailList, List) :-
   !,
   myflatten(H, FlatTail, List),
   myflatten(T, TailList, FlatTail).
myflatten(NonList, T , [NonList|T]).

% Removes one instance of A from second argument list.
myremove(A,[A|B],B).
myremove(A,[B,C|D],[B|E]) :- myremove(A,[C|D],E).

 
% Exercise #2 member2, makes sure that X is present exactly two times on Y

member2(X,[X|Rest]) :- member2b(X,Rest).
member2(X,[Y|Rest]) :- X \= Y, member2(X,Rest).

member2b(X,[X|Rest]) :- not(mymember(X,Rest)).
member2b(X,[Y|Rest]) :- X \= Y,  member2b(X,Rest).


% Exercise #3 returns true when X is a consecutive substring of Y
mysubstring(X,Y) :- myappend(_,T, Y), myappend(X,_,T) , X \= [].

% Exercise # 4   returns all possibble sublist from a given list
mysublist([], [] ).
mysublist([X|XS], [X|YS]) :- mysublist(XS, YS).
mysublist([_|XS], YS) :- mysublist(XS, YS).



% Exercise #5 returns or indicates if the second argument is a permutation of the first

mypermutation([],[]).
mypermutation([H|T], S) :-
  ','(mypermutation(T,P), ','(myappend(X,Y,P),
  myappend(X, [H|Y], S))).


% Exercise #6  family tree and queries.
daugther('Henri de Reiset','Maria Jimenez','Paula de Reiset').
daugther('Jose Jimenez','Maria Carbo','Maria Jimenez').
daugther('Jose Jimenez','Maria Carbo','Noni Jimenez').
daugther('Fernando Uraga','Noni Jimenez','Valentina Uraga').
son('Henri de Reiset','Maria Jimenez','Juan de Reiset').
son('Phillipe de Reiset', 'Olga Margary', 'Henri de Reiset').
husband('Maria Jimenez','Henri de Reiset').
husband('Noni Jimenez','Fernando Uraga').

wife(X,Y) :- husband(Y,X).

% parent of X is Y
parent(X,Y) :- father(X,Y); mother(X,Y).
father(X,Y) :- daugther(Y,W,X); son(Y,W,X).
mother(X,Y) :- daugther(W,Y,X) ; son(W,Y,X).

% grandparent of X is Y
grandfather(X,Y) :- father(X,Z), father(Z,Y);mother(X,Z), father(Z,Y).
grandmother(X,Y) :- mother(X,Z) , mother(Z,Y); father(X,Z) , mother(Z,Y).

% The sibling of X is Y
sibling(X,Y) :- father(X,Z), father(Y,Z), X \= Y ; mother(X,Z), mother(Y,Z) , X \= Y.
brother(X,Y) :- sibling(X,Y), son(W,Z,Y).
sister(X,Y) :- sibling(X,Y), daugther(W,Z,Y).

% The cousin of X is Y
cousin(X,Y) :- parent(X,Z), parent(Y,W), sibling(Z,W).

% The uncle|aunt of X is Y
uncle(X,Y) :- parent(X,Z) , brother(Z,Y); parent(X,Z) , sister(Z,W), husband(W,Y).
aunt(X,Y) :- parent(X,Z) , sister(Z,Y); parent(X,Z) , brother(Z,W), wife(W,Y).


%Exercise #7 path and shortespath of an unweighted directed graph
edge(a,b).
edge(a,c).
edge(a,d).
edge(e,a).
edge(c,a).
edge(z,x).
edge(x,y).
edge(z,y).

path(X,Y) :- edge(X,Y); edge(X,Z), edge(Z,Y).

%------------------------------------------
% Based on example found in : https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15A.pl
edge(a,b,1).
edge(a,c,1).
edge(a,d,1).
edge(e,a,1).
edge(c,a,1).
edge(z,x,1).
edge(x,y,1).
edge(z,y,1).

connected(X,Y,L) :- edge(X,Y,L) .

path2(A,B,Path,Len) :-
       travel(A,B,[A],Q,Len), 
       reverse(Q,Path).

travel(A,B,P,[B|P],L) :- 
       connected(A,B,L).
travel(A,B,Visited,Path,L) :-
       connected(A,C,D),           
       C \== B,
       \+mymember(C,Visited),
       travel(C,B,[C|Visited],Path,L1),
       L is D+L1.  

shortestpath(A,B,Path,Length) :-
   setof([P,L],path2(A,B,P,L),Set),
   Set = [_|_], % fail if empty
   minimal(Set,[Path,Length]).

minimal([F|R],M) :- min(R,F,M).

% The minimun path out of a list of paths
min([],M,M).
min([[P,L]|R],[_,M],Min) :- L < M, !, min(R,[P,L],Min). 
min([_|R],M,Min) :- min(R,M,Min).

%Exercise # 8

%logical puzzle
%based on https://stackoverflow.com/questions/7338225/why-cant-i-get-the-answer-to-the-zebra-puzzle-in-prolog

%solverhouses(Houses), returns the house arrangement with nationalities,pets,sports,drinks 
% and  house color
% solverjuice(X), returns who drinks juice
%s olverhamster(X), returns who owns a hamster

exists(A,(A,_,_,_,_)).
exists(A,(_,A,_,_,_)).
exists(A,(_,_,A,_,_)).
exists(A,(_,_,_,A,_)).
exists(A,(_,_,_,_,A)).

rightOf(A,B,(B,A,_,_,_)).
rightOf(A,B,(_,B,A,_,_)).
rightOf(A,B,(_,_,B,A,_)).
rightOf(A,B,(_,_,_,B,A)).

middleHouse(A,(_,_,A,_,_)).
firstHouse(A,(A,_,_,_,_)).

nextTo(A,B,(B,A,_,_,_)).
nextTo(A,B,(_,B,A,_,_)).
nextTo(A,B,(_,_,B,A,_)).
nextTo(A,B,(_,_,_,B,A)).
nextTo(A,B,(A,B,_,_,_)).
nextTo(A,B,(_,A,B,_,_)).
nextTo(A,B,(_,_,A,B,_)).
nextTo(A,B,(_,_,_,A,B)).

solverjuice(JuiceDrinker) :-
  Houses = (house(N1,P1,S1,D1,C1),house(N2,P2,S2,D2,C2),house(N3,P3,S3,D3,C3),house(N4,P4,S4,D4,C4),house(N5,P5,S5,D5,C5)),
  firstHouse(house(irish,_,_,_,_),Houses),          %irishman lives in the first house            
  nextTo(house(_,_,baseball,_,_),house(_,tiger,_,_,_),Houses), %man who plays baseball next to house with tiger
  nextTo(house(_,_,soccer,_,_),house(_,horse,_,_,_), Houses),  %next to house with horse, plays soccer 
  exists(house(_,_,squash,gin,_),Houses),           %squash player drins gin
  exists(house(french,_,rugger,_,_),Houses),        %Frenchman plays rugger
  nextTo(house(irish,_,_,_,_),house(_,_,_,_,blue), Houses), %Irishman lives next to blue house
  exists(house(english,_,_,_,red),Houses),            %englishman lives in red house
  exists(house(spanish,dog,_,_,_),Houses),          %spaniard has dog
  exists(house(_,_,_,beer,green), Houses),          % beer in green house
  exists(house(scottish,_,_,whiskey,_), Houses),    %scottsman drinks whiskey
  rightOf(house(_,_,_,_,green),house(_,_,_,_,white),Houses),  %green house is to the right of the white house
  exists(house(_,snakes,tennis,_,_),Houses),        %tennis player owns snakes
  exists(house(_,_,soccer,_,yellow),Houses),        % soccer is played in the yellow house
  middleHouse(house(_,_,_,wine,_),Houses),          % Wine is consumed in middle house
  exists(house(_,hamster,_,_,_),Houses),
  exists(house(JuiceDrinker,_,_,juice,_),Houses).

solverhamster(HamsterOwner) :-
  Houses = (house(N1,P1,S1,D1,C1),house(N2,P2,S2,D2,C2),house(N3,P3,S3,D3,C3),house(N4,P4,S4,D4,C4),house(N5,P5,S5,D5,C5)),
  firstHouse(house(irish,_,_,_,_),Houses),          %irishman lives in the first house            
  nextTo(house(_,_,baseball,_,_),house(_,tiger,_,_,_),Houses), %man who plays baseball next to house with tiger
  nextTo(house(_,_,soccer,_,_),house(_,horse,_,_,_), Houses),  %next to house with horse, plays soccer 
  exists(house(_,_,squash,gin,_),Houses),           %squash player drins gin
  exists(house(french,_,rugger,_,_),Houses),        %Frenchman plays rugger
  nextTo(house(irish,_,_,_,_),house(_,_,_,_,blue), Houses), %Irishman lives next to blue house
  exists(house(english,_,_,_,red),Houses),            %englishman lives in red house
  exists(house(spanish,dog,_,_,_),Houses),          %spaniard has dog
  exists(house(_,_,_,beer,green), Houses),          % beer in green house
  exists(house(scottish,_,_,whiskey,_), Houses),    %scottsman drinks whiskey
  rightOf(house(_,_,_,_,green),house(_,_,_,_,white),Houses),  %green house is to the right of the white house
  exists(house(_,snakes,tennis,_,_),Houses),        %tennis player owns snakes
  exists(house(_,_,soccer,_,yellow),Houses),        % soccer is played in the yellow house
  middleHouse(house(_,_,_,wine,_),Houses),          % Wine is consumed in middle house
  exists(house(HamsterOwner,hamster,_,_,_),Houses),
  exists(house(_,_,_,juice,_),Houses).

solverhouses(Houses) :-
  Houses = (house(N1,P1,S1,D1,C1),house(N2,P2,S2,D2,C2),house(N3,P3,S3,D3,C3),house(N4,P4,S4,D4,C4),house(N5,P5,S5,D5,C5)),
  firstHouse(house(irish,_,_,_,_),Houses),          %irishman lives in the first house            
  nextTo(house(_,_,baseball,_,_),house(_,tiger,_,_,_),Houses), %man who plays baseball next to house with tiger
  nextTo(house(_,_,soccer,_,_),house(_,horse,_,_,_), Houses),  %next to house with horse, plays soccer 
  exists(house(_,_,squash,gin,_),Houses),           %squash player drins gin
  exists(house(french,_,rugger,_,_),Houses),        %Frenchman plays rugger
  nextTo(house(irish,_,_,_,_),house(_,_,_,_,blue), Houses), %Irishman lives next to blue house
  exists(house(english,_,_,_,red),Houses),            %englishman lives in red house
  exists(house(spanish,dog,_,_,_),Houses),          %spaniard has dog
  exists(house(_,_,_,beer,green), Houses),          % beer in green house
  exists(house(scottish,_,_,whiskey,_), Houses),    %scottsman drinks whiskey
  rightOf(house(_,_,_,_,green),house(_,_,_,_,white),Houses),  %green house is to the right of the white house
  exists(house(_,snakes,tennis,_,_),Houses),        %tennis player owns snakes
  exists(house(_,_,soccer,_,yellow),Houses),        % soccer is played in the yellow house
  middleHouse(house(_,_,_,wine,_),Houses),          % Wine is consumed in middle house
  exists(house(_,hamster,_,_,_),Houses),
  exists(house(_,_,_,juice,_),Houses).






% Exercise # 9
%Josephus problem

josephus(N,M) :- form_list(1,N,L), find_winner(L,M,WIN), write('The winner is '), write(WIN).

form_list(A,B,[]) :- A > B.
form_list(A,B,L) :- A =< B, tmpA is A+1, form_list(tmpA,B,tmpL), myappend([A], tmpL,L).

find_winner([WIN],_,WIN).
find_winner(L,M,WIN) :- cycle(L,M,[_|tmpL]), find(tmpL,M,WIN).

cycle([],_,[]).
cycle([H|T],0,[H|T]).
cycle([H|T],N,L) :- N>0, myappend(T,[H],tmp), Nl is N-1, cycle(tmp,Nl,L).

