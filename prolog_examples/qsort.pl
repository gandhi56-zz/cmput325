/* from http://ktiml.mff.cuni.cz/~bartak/prolog/sorting.html with some bugfixes by me */

quick_sort([],[]).
quick_sort([H|T],Sorted):-
	partition(H,T,L1,L2),quick_sort(L1,Sorted1),quick_sort(L2,Sorted2),
	append(Sorted1,[H|Sorted2], Sorted).
   
partition(_,[],[],[]).
partition(H,[X|T],[X|L],G):-X=<H,partition(H,T,L,G).
partition(H,[X|T],L,[X|G]):-X>H,partition(H,T,L,G).

/* --------------------------------------------------------------------------- */
quick_sort2(List,Sorted):-q_sort(List,[],Sorted). 
q_sort([],Acc,Acc).
q_sort([H|T],Acc,Sorted):-
	partition2(H,T,L1,L2),
	q_sort(L1,Acc,Sorted1),
	q_sort(L2,[H|Sorted1],Sorted).

partition2(_,[],[],[]).
partition2(H,[X|T],[X|L],G):-X>H,partition2(H,T,L,G).
partition2(H,[X|T],L,[X|G]):-X=<H,partition2(H,T,L,G).


randomList(0,[]).
randomList(N,[F|R]) :- N>0, F is random(100000), N1 is N-1, randomList(N1,R).

/* --------------------------------------------------------------------------- */
test(N) :- randomList(N,L),
            write('Sorting '),write(L),
            quick_sort(L,Sorted),
            write(' gives '),write(Sorted).

test2(N) :- randomList(N,L),
            write('Sorting '),write(L),
            quick_sort2(L,Sorted),
            write(' gives '),write(Sorted).
