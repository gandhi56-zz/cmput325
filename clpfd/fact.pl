:- use_module(library(clpfd)).
f(0,1).
f(N, F) :-
  N #> 0,
  N1 #= N - 1,
  f(N1, F1),
  F #= N * F1.
