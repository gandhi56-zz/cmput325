a3last([X], X).
a3last([_|Rest], Last) :- 
  a3last(Rest, RestLast), 
  write(Rest),
  Last is RestLast.



