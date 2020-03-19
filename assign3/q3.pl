
amember(X, [X|_]).
amember(X, [_|L]) :-
  amember(X, L).

not_member(_, []).
not_member(X, [Y|L]) :-
  X \== Y,
  not_member(X, L).

add2list([], L, L).
add2list([A|L1], L2, [A|L3]) :-
  add2list(L1, L2, L3).

rev([], []).
rev([H|T], R) :-
  rev(T, S),
  add2list(S, [H], R).

nocopy(X, [_|L]) :-
  nocopy(X, L).
nocopy(X, [X|L]) :-
  not_member(X, L).

a3member(X, L) :-
  nocopy(X, L).



