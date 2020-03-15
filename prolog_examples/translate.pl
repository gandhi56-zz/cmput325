translate(D,[],[]).
translate(D,[A|R],[B|S]) :- tword(D,A,B), translate(D,R,S).

tword([],X,X).
tword([(X,Y)|_],X,Y).
tword([(X,Y)|R],X1,Y1) :- var(Y1), X \== X1, tword(R,X1,Y1).
tword([(X,Y)|R],X1,Y1) :- var(X1), Y \== Y1, tword(R,X1,Y1).
