lastN(L, N, R) :- length(L, LLength), 
                  NRemove is LLength - N,
                  removeFirstN(L, NRemove, R).
removeFirstN(L, 0, L).
removeFirstN([_|L], N, R) :- N > 0,
                             N1 is N - 1,
                             removeFirstN(L, N1, R).

/* test cases:
?- lastN([7,3,4], 1, R).
should return R = [4].

?- lastN([7,3,4], 3, R).
should return R = [7,3,4].

?- lastN([7,3,4], 0, R).
should return R = [].

?- lastN([7,3,4], 2, [3,4]).
should return yes.
*/
