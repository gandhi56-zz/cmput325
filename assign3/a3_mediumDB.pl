/** a3_mediumDB.pl*/

/****** Database configuration 1 ******/

required(hydrogen_peroxide, water).
required(hydrogen, hydrogen_peroxide).
required(oxygen,hydrogen_peroxide).

required(hydrogen_bicarbonate, water).
required(sodium_bicarbonate, hydrogen_bicarbonate).
required(hydrochloric_acid, hydrogen_bicarbonate).
required(sodium, sodium_bicarbonate).
required(bicarbonate, sodium_bicarbonate).
required(hydrogen, hydrochloric_acid).
required(chlorine, hydrochloric_acid).


available(carbon).
available(oxygen).
available(hydrogen).
available(sodium).
available(chlorine).
available(hydrochloric_acid).

/*

?- full_synthesis(sodium_bicarbonate,A,M).
A = [sodium],
M = [bicarbonate] ;

?- full_synthesis(water,A,M).
A = [hydrogen, oxygen, sodium, hydrochloric_acid],
M = [bicarbonate] ;
false.


?- full_synthesis(hydrochloric_acid,A,M).
A = [hydrochloric_acid],
M = [] ;
false.

*/


/****** Database configuration 2 ******
Idential to Database Configuration 1 except that hydrochloric_acid is no more available,
all the paths that leads to hydrochloric_acid should be synthesized further.
*/

required(hydrogen_peroxide, water).
required(hydrogen, hydrogen_peroxide).
required(oxygen,hydrogen_peroxide).

required(hydrogen_bicarbonate, water).
required(sodium_bicarbonate, hydrogen_bicarbonate).
required(hydrochloric_acid, hydrogen_bicarbonate).
required(sodium, sodium_bicarbonate).
required(bicarbonate, sodium_bicarbonate).
required(hydrogen, hydrochloric_acid).
required(chlorine, hydrochloric_acid).


available(carbon).
available(oxygen).
available(hydrogen).
available(sodium).
available(chlorine).

/**

?- full_synthesis(water,A,M).
A = [hydrogen, oxygen, sodium, chlorine],
M = [bicarbonate] ;
false.

full_synthesis(hydrochloric_acid,A,M).
A = [hydrogen, chlorine],
M = [] ;
false.

***/ 



