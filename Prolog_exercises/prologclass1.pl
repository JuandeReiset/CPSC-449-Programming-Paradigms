% Reverse and permutation predicates:

loves(romeo, juliet).

loves(juliet, romeo) :- loves(romeo,juliet).

male(albert).
male(bob).
male(bill).
male(carl).
male(charlie).

female(alice).
female(betsy).
female(diana).
female(becky).
female(wendy).

happy(albert).
happy(alice).
happy(bob).
happy(bill).
with_albert(alice).

runs(albert) :-
  happy(albert).
  
dances(alice) :-
  happy(alice), with_albert(alice).

does_alice_dance :- dances(alice),
  write('When Alice is happy and with Albert she dances').
  

swims(bob) :-
  happy(bob),
  near_water(bob).

swims(bill) :-
 happy(bill).

swims(bill) :-
 near_water(bill).

parent(albert,bob).
parent(albert,betsy).

parent(alice,bob).
parent(alice,betsy).

parent(bob,carl).
parent(bob,charlie).

get_grandparent:-
  parent(X,carl),
  parent(X,charlie),
  format('~w ~s grandparent ~n)', [X, "is the"]).

brother(bob,bill).


guess_num :- loop(1).

loop(15) :- write('YOU GUESSED IT').
loop(X) :-
 X \= 15,
 write('Guess Number '),
 read(Guess),
 write(Guess),
 write(' is not the number'), nl,
 loop(Guess).
