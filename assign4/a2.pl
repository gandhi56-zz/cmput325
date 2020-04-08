is_team(X) :- member(X, [calgary, edmonton, regina, saskatoon, vancouver]).
are_teams(X, Y) :- is_team(X), is_team(Y), alph(X, Y).

alph(calgary, edmonton).
alph(calgary, regina).
alph(calgary, saskatoon).
alph(calgary, vancouver).
alph(edmonton, regina).
alph(edmonton, saskatoon).
alph(edmonton, vancouver).
alph(regina, saskatoon).
alph(regina, vancouver).
alph(saskatoon, vancouver).

count(_, [], 0).
count(X, [X|Rest], N) :- count(X, Rest, NRest), N is NRest + 1.
count(X, [Y|Rest], N) :- \+ X = Y, count(X, Rest, N).

my_all_different([]).
my_all_different([A|L]) :-  \+ member(A,L), my_all_different(L).

schedule(M) :-  length(M, 10), ok(M, 1). 
%count((calgary, edmonton), M, 1),
% count((calgary, regina), M, 1),
% count((calgary, saskatoon), M, 1),
% count((calgary, vancouver), M, 1),
% count((edmonton, regina), M, 1),
%  count((edmonton, saskatoon), M, 1),
%  count((edmonton, vancouver), M, 1),
%  count((regina, saskatoon), M, 1),
%  count((regina, vancouver), M, 1),
%  count((saskatoon, vancouver), M, 1).

% game 1
ok([(X, Y)|Rest], 1) :- are_teams(X, Y) , my_all_different([X, Y, regina]),
  ok(Rest, 2, (X, Y)).

% game 2
ok([(X, Y)|Rest], 2, (L, R)) :- are_teams(X, Y) , my_all_different([X, Y, L, R, regina]),
  ok(Rest, 3, (X, Y)).

% game 3
ok([(X, Y)|Rest], 3, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R]),
  ok(Rest, 4, (X, Y)).

% game 4
ok([(X, Y)|Rest], 4, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R]),
  ok(Rest, 5, (X, Y)).

% game 5
ok([(X, Y)|Rest], 5, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R]),
  ok(Rest, 6, (X, Y)).

% game 6
ok([(X, Y)|Rest], 6, _) :- are_teams(X, Y), my_all_different([X, Y]),
  ok(Rest, 7, (X, Y)).

% game 7
ok([(X, Y)|Rest], 7, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R]),
  ok(Rest, 8, (X, Y)).

% game 8
ok([(X, Y)|Rest], 8, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R]),
  ok(Rest, 9, (X, Y)).

% game 9
ok([(X, Y)|Rest], 9, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R, saskatoon]),
  ok(Rest, 10, (X, Y)).

ok([(X, Y)], 10, (L, R)) :- are_teams(X, Y), my_all_different([X, Y, L, R, saskatoon]).























