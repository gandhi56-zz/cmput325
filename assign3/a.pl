a3last([X], X).
a3last([_|Rest], Last) :- 
  a3last(Rest, Last).

is_even(N) :- mod(N, 2) =:= 0.

%even(L, E)

