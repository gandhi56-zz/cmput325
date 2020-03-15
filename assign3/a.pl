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






