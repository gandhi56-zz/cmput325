

/* a3_medium_cycleDB.pl */

required(oxygen, water).
required(water, hydrogen_peroxide).
required(hydrogen_peroxide, oxygen).
required(water, oil).
required(oil, oxygen).
required(oil, butter).
required(butter, oil).
required(water, lemonade).

/*
?- in_cycle(oxygen, Cycle).
Cycle = [oxygen, water, hydrogen_peroxide, oxygen];
Cycle = [oxygen, water, oil, oxygen].

?- in_cycle(hydrogen_peroxide, Cycle).
Cycle = [hydrogen_peroxide, oxygen, water, hydrogen_peroxide].

?- in_cycle(oil, Cycle).
Cycle = [oil, oxygen, water, oil];
Cycle = [oil, butter, oil].

?- in_cycle(water, Cycle).
Cycle = [water, hydrogen_peroxide, oxygen, water];
Cycle = [water, oil, oxygen, water].

?- in_cycle(lemonade, Cycle).
false.
*/
