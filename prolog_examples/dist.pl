aroad(X,Y,L) :- road(X,Y,L).
aroad(Y,X,L) :- road(X,Y,L).

dist(X,X,0).
dist(X,Y,L) :- aroad(X,Y,L).
dist(X,Y,L) :- X \== Y, aroad(X,Z,L1), dist2(Z,Y,X,L2), L is L1+L2.

dist2(X,Y,Bad,L) :- aroad(X,Y,L).
dist2(X,Y,Bad,L) :- X \== Y, aroad(X,Z,L1), Z \== Bad, dist2(Z,Y,X,L2), L is L1+L2.

distCities(A, B, N) :- city(A),city(B),dist(A,B,N).
