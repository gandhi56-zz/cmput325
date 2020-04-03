
menu(M) :-
  length(M, 7),
  ok(M, 7, 0).

ok([H|Rest], 7, X) :- fish_dish(H), ok(Rest, 6, X), \+ member(H, Rest).
ok([H|Rest], 6, X) :- fish_dish(H), ok(Rest, 5, X), \+ member(H, Rest).
ok([H|Rest], 6, X) :- meat_dish(H), ok(Rest, 5, X), \+ member(H, Rest), X = 0.
ok([H|Rest], 5, X) :- fish_dish(H), ok(Rest, 4, X), \+ member(H, Rest).
ok([H|Rest], 5, X) :- vegetarian_dish(H), ok(Rest, 4, X), \+ member(H, Rest).
ok([H|Rest], 4, X) :- meat_dish(H), ok(Rest, 3, X), \+ member(H, Rest), X = 0.
ok([H|Rest], 4, X) :- vegetarian_dish(H), ok(Rest, 3, X), \+ member(H, Rest).
ok([H|Rest], 3, X) :- vegetarian_dish(H), ok(Rest, 2, X), \+ member(H, Rest).
ok([H|Rest], 2, X) :- vegetarian_dish(H), ok(Rest, 1, X), \+ member(H, Rest).
ok([H|Rest], 2, X) :- meat_dish(H), ok(Rest, 1, X), \+ member(H, Rest), X = 0.
ok([H], 1, 0) :- meat_dish(H).
ok([H], 1, 1) :- vegetarian_dish(H).
ok([H], 1, 1) :- fish_dish(H).

count_solutions(C) :-   aggregate_all(count, menu(_), C).

meat_free_menu(M) :-
  length(M, 7),
  ok(M, 7, 1).

count_meat_free_solutions(C) :-   aggregate_all(count, meat_free_menu(_), C).
