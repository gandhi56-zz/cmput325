sqsum([], 0).
sqsum([First | Rest], Sum) :- sqsum(Rest, RestSum),
                            Sum is RestSum + First * First.
