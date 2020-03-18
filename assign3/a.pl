a3last([X], X).
a3last([_|Rest], Last) :- 
  a3last(Rest, Last).

is_even(N) :- mod(N, 2) =:= 0.

/*
  unnest(A, B) is true iff B is A without any nesting of lists
*/
xatom(A) :- atom(A).
xatom(A) :- number(A).
unnest([],[]).
unnest([A|L],[A|L1]) :- 
     xatom(A), unnest(L,L1).
unnest([A|L],R) :- 
     unnest(A,A1), unnest(L,L1), append(A1,L1,R).

/*
  even(L, E) is true iff E is an unnested list of L containing
  only even elements
*/

even(L, E) :-
  unnest(L, L2),
  findall(X, (member(X, L2), is_even(X)), E).


/*
  a3member(-X, ++L) generates members of X without any repetition
*/

/*
list2set([], []).
list2set([E|Es], Set) :-
  member(E, Es),
  list2set(Es, Set).

list2set([E|Es], [E|Set]) :-
  maplist(dif(E), Es),
  list2set(Es, Set).

a3member(X, L) :-
  list2set(L, L2),
  member(X, L2).
*/

not_member(_, []).
not_member(X, [Y|L]) :-
  X \== Y,
  not_member(X, L).

nocopies([], []).
nocopies([L|Rest], S) :-
  member(L, Rest),
  nocopies(Rest, S).

nocopies([L|Rest], [L|S]) :-
  not_member(L, Rest),
  nocopies(Rest, S).

a3member(X, L) :-
  nocopies(L, S),
  member(X, S).

/*
  one_step_synthesis(++P, ?L) is true if P is a product and
  L is the list of all ingredients which are required for the
  final step that produces P, in sorted order as the relevant
  required facts in the database.
*/

one_step_synthesis(P, L) :-
  findall(X, required(X, P), L).

/*
  full_synthesis(++P, ?A, ?M)
  given
    P = product
    A = list of all available ingredients that is required at any step in the synthesis of P
    M = list of missing ingredients that are required but not available
  Elements in A and M could be directly or indirectly required
*/

full_synthesis(P, A, M) :-
  one_step_synthesis(P, I).







