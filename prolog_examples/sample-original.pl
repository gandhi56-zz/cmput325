/*
    Sample Prolog Programs - original codes. 
    These predicates typically work well for finding the first solution, 
    but some of them have issues with backtracking.
    You can load the file and run the test cases.
*/

%*******************************************************************
% Using constants and function symbols to build data structures:
% you can use any constant symbol to represent 0 and 
% any function symbol to represent the successor function

plus(0, X, X).
plus(s(X), Y, s(Z)) :- plus(X,Y,Z).

mult(0, X, 0).
mult(s(0), X, X).
mult(s(X), Y, N) :- mult(X, Y, N1), plus(Y, N1, N).

factorial(0, s(0)).
factorial(s(0), s(0)).
factorial(s(X), N) :- factorial(X, N1), mult(N1, s(X), N).

%*******************************************************************
% In practical programming, you use 
% builtin predicates for arithmetics.

fact1(0, 1).
fact1(1, 1).
fact1(X, N) :- Z is X - 1,
               fact1(Z, N1), 
               N is N1 * X.

% test cases

test1(W) :- plus(s(s(0)), s(s(s(0))), W).
test2(X,Y) :- plus(X, Y, s(s(s(0)))).
test3(W) :- mult(s(s(0)), s(s(s(0))), W).
test4(W) :- fact(s(s(s(s(0)))), W).
test5(W) :- fact1(4, W).

%*******************************************************************
% In Prolog, we can only define predicates (relations). If you
% want to define a function, you have to define a relation by 
% using an extra parameter to hold the "returned" result.
% The proper use of the parameters in invocations will give 
% you the desired function. 

% We earlier wrote the Lisp function "cartesian". 
% We now define a relation, i.e., cartesian(X,Y,Z) where 
% X and Y are lists and  Z is the list of all the pairs corresponding
% to the cartesian product of X and Y.
% Example:
% cartesian([a, b], [d, e], 
%           [[a, d], [a, e], [b, d], [b, e]]) 
% is true.


cartesian([], L, []).
cartesian([A|N], L, M) :- 
             pair(A,L,M1), 
             cartesian(N, L, M2),
             append(M1, M2, M).
pair(A, [], []).
pair(A, [B|L], [[A,B]|N] ) :- pair(A, L, N).

% tests
test6(W) :- cartesian([a, b], [d, e], W).
test7 :- cartesian([a, b], [d, e], [[a, d], [a, e], [b, d], [b, e]]). 

%*******************************************************************

% Given a (possibly nested) list of numbers L, 
% sum(L,N) will have N bound to the sum of the numbers in L.

sum([],0).
sum(N, N) :- number(N).
sum([A|L],S) :- sum(A,S1), sum(L,S2), S is S1 + S2.

% test case:
testSum(S) :- sum([2,3,[4,5,[6],7],9], S).

%*******************************************************************
% flatten(L,L1): flatten a list of atoms (atoms and numbers) L to 
% a flat list L1. 

flatten([],[]).
flatten([A|L],[A|L1]) :- 
     xatom(A), flatten(L,L1).
flatten([A|L],R) :- 
     flatten(A,A1), flatten(L,L1), append(A1,L1,R).

xatom(A) :- atom(A).
xatom(A) :- number(A).

% tests
testFlatten1(I) :- flatten([2,[a]], I).
testFlatten2(I) :- flatten([2,[a],[b,5,[c],d],9], I).

%*******************************************************************
% You can express patterns of nested lists, e.g.
%
%  1. []              an empty list
%  2. [[]|L]         a list whose car part is an empty list
%  3. [[A|W]|L]   a list whose car part is a non-empty list
%  4. [A|L]          if none of the above applies, A is an atom or number.


flat([],[]).
flat([[]|L],L1) :- flat(L,L1).
flat([[A|L]|W],R) :- flat([A|L],U), flat(W,W1), append(U,W1,R).
flat([A|L], [A|L1]) :- flat(L,L1).

% The last clause above works only for getting the first answer.
% Backtracking could invoke it when A is not an atom. Better to replace 
% it by 
%
% flat([A|L], [A|L1]) :- xatom(A), flat(L,L1).

% tests
f2(I) :- flat([2,[a]], I).
f3(I) :- flat([2,[a],[b,5,[c],d],9], I).


%*******************************************************************
%                 The Eight-Queens Problem
%
% A board of eight queens with one of them placed in&nbsp;each row
% such that no two of them are in the same column, or in the same
% diagonal (if they are, they will attack each other).
% We use a list to represent the board positions and how queens are
% placed on the board. The ith queen (in the ith row) in the jth
% column will be represented by placing integer i as the jth element
% in the list. For example, [1,4,2,5,3,8,6,7] represents the locations
% of the queens as:
%    the first queen is at the board position (1.1);
%    the second queen is at the board position (2.4);
%    the third queen is at the board position (3.2), etc. 

solution(Board) :-
    permutation([1,2,3,4,5,6,7,8], Board), safe(Board).

permutation([], []).
permutation([A|M], N) :- 
          permutation(M, N1),
          insert(A, N1, N).

insert(A, L, [A|L]).
insert(A, [B|L], [B|L1]) :- insert(A, L, L1).

safe([Q]).
safe([Q|List]) :- nodiag(Q, List, 1), safe(List).

nodiag(Q, [], Dist).
nodiag(Q1, [Q2|List], Dist) :- 
       noattack(Q1, Q2, Dist),
       NewDist is Dist + 1,
       nodiag(Q1, List, NewDist).

noattack(Q1, Q2, Dist) :- 
       Q2 - Q1 =\= Dist, Q1 - Q2 =\= Dist.

% could use the builtin function abs(X) -- absolute value of X:
%     abs(Q2-Q1) =\= Dist


%*******************************************************************
% To generalize the solution to the N-queens problem, we can define
% queens(+N,-S) where N is a positive integer and S should be bound to
% a solution of the N-queens problem.

queens(N,S) :- gen_list(N,L), permutation(L, S), safe(S).
gen_list(1, [1]).
gen_list(N, L) :- N > 1, N1 is N - 1, gen_list(N1, L1), append(L1,[N],L).

%*******************************************************************
% You can use the following to save typing a long string for loading 
% this file -- once it has been loaded for the first time, simply type l. 
% to load it again after changes were made. 

l :- consult('sample-prog.pl').

% To measure the CPU time of running your query: 
 
stats :- 
   statistics(runtime,[_,X]), 
   T is X/1000,
   nl,
   write('Run time: '),
   write(T), write(' sec.'), nl.

% Example:
qs :- queens(8,S), stats. 

% where the CPU time for generating the first solution in proving 
% queens(8,S) is printed.</pre>
