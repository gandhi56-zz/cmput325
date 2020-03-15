/*
Cmput 325 Prolog sample code
Length of a List.
To load and run this example in SWI Prolog:
- start SWI Prolog by typing
swipl
- At the prompt ?-, load the file with
[intro].
- dont forget the dot at the end.
*/

% A fact
len([], 0).

% A rule
len([First|Rest], N) :- len(Rest, NRest),
                        N is NRest + 1.

/*
Examples of queries you can try:
len([a,b,c], X).
len([a,b,c], 3).
len([a,b,c], 4).
len(X, 3).
len(X, Y).
len([], X).
len([[[a,b,c]]], X).
*/
