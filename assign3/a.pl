
/*
* miscelleanous predicates
*
*/

listLen([], 0).
listLen([_|T], N) :- listLen(T, NRest), N is NRest + 1.



/*
  ===========================================
  || 1 a3last                              ||
  ===========================================
*/

a3last([X], X).
a3last([_|Rest], Last) :- 
  a3last(Rest, Last).

/*
  ===========================================
  || 2 is_even and even                    ||
  ===========================================
*/
is_even(N) :- mod(N, 2) =:= 0.
xatom(A) :- atom(A).
xatom(A) :- number(A).
unnest([],[]).
unnest([A|L],[A|L1]) :- 
     xatom(A), unnest(L,L1).
unnest([A|L],R) :- 
     unnest(A,A1), unnest(L,L1), append(A1,L1,R).
even(L, E) :-
  unnest(L, L2),
  findall(X, (member(X, L2), is_even(X)), E).


/*
  ===========================================
  || 3 a3member                            ||
  ===========================================
*/


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
  rev(L, Rev),
  nocopy(X, Rev).


/*
  ===========================================
  || 4.1 one_step_synthesis                ||
  ===========================================
*/

one_step_synthesis(P, L) :-
  findall(X, required(X, P), Res),
  list_to_set(Res, L).

/*
  ===========================================
  || 4.2 full_synthesis                    ||
  ===========================================
*/

% P is available
full_synthesis(P, [P], []) :-
  available(P).

% P is unavailable, recurse on
% its ingredients I
full_synthesis(P, A, M) :-
  \+ available(P),
  one_step_synthesis(P, I),
  split(I, A, M).

% trivial case
split([], [], []).

% ingredient X is available
split([X|IRest], [X|ARest], M) :-
  available(X),
  split(IRest, ARest, M).

% ingredient X is unavailable and
% it is atomic
split([X|IRest], A, [X|MRest]) :-
  \+available(X),
  one_step_synthesis(X, I),
  listLen(I, 0),
  split(IRest, A, MRest).

% ingredient X is unavailable and
% it is not atomic
split([X|_], A, M) :-
  \+available(X),
  one_step_synthesis(X, I),
  listLen(I, N),
  N > 0,
  split(I, A, M).



