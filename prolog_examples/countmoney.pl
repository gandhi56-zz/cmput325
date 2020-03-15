countmoney([], dollar(0), euro(0), yen(0)).

countmoney([dollar(X)|R], dollar(D), euro(E), yen(Y)) :- 
  countmoney(R, dollar(D1), euro(E), yen(Y)), D is D1+X.
  
countmoney([euro(X)|R], dollar(D), euro(E), yen(Y)) :- 
  countmoney(R, dollar(D), euro(E1), yen(Y)), E is E1+X.
  
countmoney([yen(X)|R], dollar(D), euro(E), yen(Y)) :- 
  countmoney(R, dollar(D), euro(E), yen(Y1)), Y is Y1+X.
