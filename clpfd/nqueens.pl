:- use_module(library(clpfd)).
nq(N, Qs) :-
  length(Qs, N),
  Qs ins 1..N,
  safe(Qs).

safe([]).
safe([Q | Qs]) :-
  safe(Qs, Q, 1),
  safe(Qs).

safe([], _, _).
safe([Q|Qs], Q0, D0) :-
  Q0 #\= Q,
  abs(Q0 - Q) #\= D0,
  D1 #= D0 + 1,
  safe(Qs, Q0, D1).


