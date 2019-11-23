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
   
% Exercise #2 member2, makes sure that X is present exactly two times on Y








% Exercise #3

% Exercise #5

mypermutation([X|Y],Z) :- mypermutation(Y,W) , takeout(X,Z,W).

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

parent(X,Y) :- father(X,Y) ; mother(X,Y).
father(X,Y) :- daugther(X,W,Y); son(X,W,Y).
mother(X,Y) :- daugther(W,X,Y) ; son(W,X,Y).
grandfather(X,Y) :- father(X,Z), father(Z,Y).
grandmother(X,Y) :- mother(X,Z) , mother(Z,Y).
sibling(X,Y) :- father(Z,X), father(Z,Y) ; mother(Z,X), mother(Z,Y).
brother(X,Y) :- sibling(X,Y), son(W,Z,X).
sister(X,Y) :- sibling(X,Y), daugther(W,Z,X).
cousin(X,Y) :- parent(Z,X), parent(W,Y), sibling(Z,W).
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
