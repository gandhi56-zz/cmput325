lastN(L, N, L) :- length(L, LLength), 
                  LLength =< N.
lastN(L, N, R) :- length(L, LLength), 
                  LLength > N,
                  L = [_| Rest],
                  lastN(Rest, N, R).
