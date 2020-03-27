
menu(M) :-
  length(M, 7),
  ok(M, 7).

ok([H|Rest], 7) :- fish_dish(H), ok(Rest, 6), \+ member(H, Rest).
ok([H|Rest], 6) :- fish_dish(H), ok(Rest, 5), \+ member(H, Rest).
ok([H|Rest], 6) :- meat_dish(H), ok(Rest, 5), \+ member(H, Rest).
ok([H|Rest], 5) :- fish_dish(H), ok(Rest, 4), \+ member(H, Rest).
ok([H|Rest], 5) :- vegetarian_dish(H), ok(Rest, 4), \+ member(H, Rest).
ok([H|Rest], 4) :- meat_dish(H), ok(Rest, 3), \+ member(H, Rest).
ok([H|Rest], 4) :- vegetarian_dish(H), ok(Rest, 3), \+ member(H, Rest).
ok([H|Rest], 3) :- vegetarian_dish(H), ok(Rest, 2), \+ member(H, Rest).
ok([H|Rest], 2) :- vegetarian_dish(H), ok(Rest, 1), \+ member(H, Rest).
ok([H|Rest], 2) :- meat_dish(H), ok(Rest, 1), \+ member(H, Rest).
ok([H], 1) :-   meat_dish(H).

count_solutions(C) :-   aggregate_all(count, menu(_), C).

meat_free_menu(M) :-
  menu(M),
  no_meat(M).

no_meat([H]) :- meat_dish(H).
no_meat([H|Rest]) :- \+ meat_dish(H), no_meat(Rest).

count_meat_free_solutions(C) :-   aggregate_all(count, meat_free_menu(_), C).
