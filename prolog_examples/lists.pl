% List notation, lists with variables, and matching equal lists
test1(A,L) :- [A|L] = [a,b,c].
test2(X, Rest) :- [a,b,X,d|Rest] = [a,b,c,d,e].
test3a :- [a|[b,c]] = [a, b, c].
test3b(L) :- [[a,b],c|L] = [a,b,c].
test4(L) :- [a,b,c|L] = [a,b,c].
test5 :- [a|[]] = [a].

%*******************************************************************
% Append with regular lists
append([], L, L).
append([A| L1], L2, [A|L3])
    :- append(L1, L2, L3).

test6(A) :- append([a1, a2], [b1], A).
test7(X,Y) :- append(X, Y, [a1, a2]).

% Append with lists represented by nil and cons
append(nil, L, L).
append(cons(A, L1), L2, cons(A, L3))
    :- append(L1, L2, L3).

test8(Q) :- append(cons(a1, cons(a2, nil)), cons(b1, nil), Q).
test9(X,Y) :- append(X,Y,cons(a1, cons(a2, cons(b1, nil)))).

% Append with lists represented by e and c
append(e, L, L).
append(c(A, L1), L2, c(A, L3)) :- append(L1, L2, L3).

test10(Q) :- append(c(a1, c(a2, e)), c(b1, e), Q).

%*******************************************************************
% List membership
member(X, [X|_]).
member(X, [_|L]) :- member(X, L).

not_member(_, []).
not_member(X, [Y|L]) :- X \== Y,
                        not_member(X, L).
                        
test11 :- member(5, [1,3,5,7,9]).
test11b :- member(5, [1,3,[5],7,9]).
test12 :- member(4, [1,3,5,7,9]).
test13 :- not_member(5, [1,3,5,7,9]).
test14 :- not_member(4, [1,3,5,7,9]).
test15(X) :- member(X, [1,3,5,7,9]).
test16(X) :- not_member(X, [1,3,5,7,9]).

%*******************************************************************
% Define a relation reverse(X,Y) such that Y is the reverse of input list X

reverse([], []).
reverse([A|Rest], Rev) :- reverse(Rest, RevRest), 
                          append(RevRest, [A], Rev).
test17(X) :- reverse([1,3,5,7,9], X).
test18(X) :- reverse(X, [1,3,5,7,9]).
% test18 computes one solution correctly, then goes into infinite descent
% if you ask for more solutions with ";".
% Can you see why?
