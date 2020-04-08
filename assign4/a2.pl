
game(calgary, edmonton).
game(calgary, regina).
game(calgary, saskatoon).
game(calgary, vancouver).
game(edmonton, regina).
game(edmonton, saskatoon).
game(edmonton, vancouver).
game(regina, saskatoon).
game(regina, vancouver).
game(saskatoon, vancouver).

count(_, [], 0).
count(X, [X|Rest], N) :- count(X, Rest, NRest), N is NRest + 1.
count(X, [Y|Rest], N) :- \+ X = Y, count(X, Rest, N).

my_all_different([]).
my_all_different([A|L]) :-  \+ member(A,L), my_all_different(L).

schedule(M) :- length(M, 10), ok(0, M, 1). 
schedule_saturday_night(M) :- length(0, M, 10), ok(M, 1).

% game 1
ok(K, [(X, Y)|Rest], 1) :- game(X, Y), X \= regina, Y \= regina,
  ok(K, Rest, 2, (X, Y)), \+ member((X, Y), Rest).

% game 2
ok(K, [(X, Y)|Rest], 2, (L, R)) :- game(X, Y), X \= regina, Y \= regina,
  my_all_different([X, Y, L, R]), ok(K, Rest, 3, (X, Y)), \+ member((X, Y), Rest).

% game 3
ok(K, [(X, Y)|Rest], 3, (L, R)) :- game(X, Y), my_all_different([X, Y, L, R]),
  ok(K, Rest, 4, (X, Y)), \+ member((X, Y), Rest).

% game 4
ok(K, [(X, Y)|Rest], 4, (L, R)) :- game(X, Y), my_all_different([X, Y, L, R]),
  ok(K, Rest, 5, (X, Y)), \+ member((X, Y), Rest).

% game 5
ok(0, [(X, Y)|Rest], 5, (L, R)) :- game(X, Y), my_allKdifferent([X, Y, L, R]),
  ok(0, Rest, 6, (X, Y)), \+ member((X, Y), Rest).
ok(1, [(X, Y)|Rest], 5, (L, R)) :- X = calgary, Y = edmonton, 
  my_all_different([X, Y, L, R]),
  ok(1, Rest, 6, (X, Y)), \+ member((X, Y), Rest).

% game 6
ok(K, [(X, Y)|Rest], 6, _) :- game(X, Y), my_all_different([X, Y]),
  ok(K, Rest, 7, (X, Y)), \+ member((X, Y), Rest).

% game 7
ok(K, [(X, Y)|Rest], 7, (L, R)) :- game(X, Y), my_all_different([X, Y, L, R]),
  ok(K, Rest, 8, (X, Y)), \+ member((X, Y), Rest).

% game 8
ok(K, [(X, Y)|Rest], 8, (L, R)) :- game(X, Y), my_all_different([X, Y, L, R]),
  ok(K, Rest, 9, (X, Y)), \+ member((X, Y), Rest).

% game 9
ok(K, [(X, Y)|Rest], 9, (L, R)) :- game(X, Y), X \= saskatoon, Y \= saskatoon,
  my_all_different([X, Y, L, R]),
  ok(K, Rest, 10, (X, Y)), \+ member((X, Y), Rest).

% game 10
ok(_, [(X, Y)], 10, (L, R)) :- game(X, Y), X \= saskatoon, Y \= saskatoon, 
  my_all_different([X, Y, L, R]).























