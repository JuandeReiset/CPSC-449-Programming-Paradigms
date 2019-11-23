% Exercise #1 predicates
myappend([],X,X).
myappend([X|Y],Z, [X|W]) :- myappend(Y,Z,W).

mymember(A,[A|Rest]).
mymember(A,[Y|Rest]) :- mymember(A,Rest).

myreverse(Xs, Ys) :-
  myreverse(Xs, [], Ys, Ys).
myreverse([], Ys, Ys, []).
myreverse([X|Xs], Rs, Ys, [_|Bound]) :-
  myreverse(Xs, [X|Rs], Ys, Bound).

myflatten(X,[X]) :- \+ isit_list(X).
myflatten([],[]).
myflatten([X|Xs],Zs) :- myflatten(X,Y), myflatten(Xs, Ys), myappend(Y,YS,ZS).

isit_list(X) :-
    var(X), !,
    fail.
isit_list([]).
isit_list([_|T]) :-
   isit_list(T).

%remove(X,Y,Z).
 
% Exercise #2 member2, makes sure that X is present exactly two times on Y

member2(X,[X|Rest]) :- member2b(X,Rest).
member2(X,[Y|Rest]) :- X \= Y, member2(X,Rest).

member2b(X,[X|Rest]) :- not(mymember(X,Rest)).
member2b(X,[Y|Rest]) :- X \= Y,  member2b(X,Rest).


% Exercise #3
substring([X|Something],[X|Rest]) :- subhelper(Something,Rest).
substring(X, [X|Rest]).
substring(X,[Y|Rest]) :- X \= Y ,  substring(X,Rest).

subhelper([X|Something],[X|Rest]) :- subhelper(Something,Rest).
subhelper(X,[X|Rest]).

% Exercise # 4
mysublist([], [] ).
mysublist([X|XS], [X|YS]) :- mysublist(XS, YS).
mysublist([_|XS], YS) :- mysublist(XS, YS).



% Exercise #5

mypermutation([],[]).
mypermutation([H|T], S) :-
  ','(mypermutation(T,P), ','(myappend(X,Y,P),
  myappend(X, [H|Y], S))).

takeout(X,[X|R],R).
takeout(X,[F|R],[F|S]) :- takeout(X,R,S).

% Exercise #6
daugther('Henri de Reiset','Maria Jimenez','Paula de Reiset').
daugther('Jose Jimenez','Maria Carbo','Maria Jimenez').
daugther('Jose Jimenez','Maria Carbo','Noni Jimenez').
daugther('Fernando Uraga','Noni Jimenez','Valentina Uraga').
son('Henri de Reiset','Maria Jimenez','Juan de Reiset').
son('Phillipe de Reiset', 'Olga Margary', 'Henri de Reiset').
husband('Henri de Reiset','Maria Jimenez').
husband('Fernando Uraga', 'Noni Jimenez').

wife(X,Y) :- husband(Y,X).

% X is parent of Y
parent(X,Y) :- father(X,Y); mother(X,Y).
father(X,Y) :- daugther(X,W,Y); son(X,W,Y).
mother(X,Y) :- daugther(W,X,Y) ; son(W,X,Y).

% X is grandparent of Y
grandfather(X,Y) :- father(X,Z), father(Z,Y);father(X,Z), mother(Z,Y).
grandmother(X,Y) :- mother(X,Z) , mother(Z,Y); mother(X,Z) , father(Z,Y).

% X is sibling of Y
sibling(X,Y) :- father(Z,X), father(Z,Y), X \= Y ; mother(Z,X), mother(Z,Y) , X \= Y.
brother(X,Y) :- sibling(X,Y), son(W,Z,X).
sister(X,Y) :- sibling(X,Y), daugther(W,Z,X).

% X is cousing of Y
cousin(X,Y) :- parent(Z,X), parent(W,Y), sibling(Z,W).

% X is uncle|aunt of Y
uncle(X,Y) :- parent(Z,Y) , brother(X,Z); parent(Z,Y) , sister(W,Z), husband(X,W).
aunt(X,Y) :- parent(Z,Y) , sister(X,Z); parent(Z,Y) , brother(W,Z), wife(X,W).


%Exercise #7
edge(a,b).
edge(a,c).
edge(a,d).
edge(e,a).
edge(c,a).
edge(z,x).
edge(x,y).
edge(z,y).

path(X,Y) :- edge(X,Y); edge(X,Z), edge(Z,Y).
shortestpath(X,Y,L) :- edge(X,Y) , L is L+1.

%Exercise # 8

%logical puzzle

h(Nationality,Pet, Cigarette,Drink,Color)



% Exercise # 9

%Josephus problem
