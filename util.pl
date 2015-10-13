use_module(library(random)).

setSeed:-
	now(Now),
	Seed is Now mod 30269,
	getrand(random(X, Y, Z, _)),
	setrand(random(Seed, X, Y, Z)), 
	!.

coinToss(X):-
	setSeed,
	random(0, 1, X).
