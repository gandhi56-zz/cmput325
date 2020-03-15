mostmoney([dollar(X)],E,Y,dollar(X)).
mostmoney([euro(X)],E,Y,euro(X)).
mostmoney([yen(X)],E,Y,yen(X)).
mostmoney([X|R],E,Y,X) :- mostmoney(R,E,Y,X1), betterequal(X,X1,E,Y).
mostmoney([X|R],E,Y,X1) :- mostmoney(R,E,Y,X1), less(X,X1,E,Y).

betterequal(X,X1,E,Y) :- convert(X,E,Y,V),convert(X1,E,Y,V1), V >= V1.
less(X,X1,E,Y) :- convert(X,E,Y,V),convert(X1,E,Y,V1), V < V1.

convert(dollar(X),_,_,V) :- V is 100*X.
convert(euro(X),E,_,V) :- V is E*X.
convert(yen(X),_,Y,V) :- V is Y*X.
