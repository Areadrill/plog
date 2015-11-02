getInt(X, Value):- repeat,
			write('Value for '), write(Value), write(': '),
			read(X1),
			integer(X1),
			X is X1,
			!.

readCoordinates(X1, Y1):-
 getInt(X1, 'x'),nl,
 getInt(Y1, 'y').

readPlayer(X):-
	repeat,
	write('Golden player chooses a player to start the game (silverPlayer. or goldenPlayer.)'), nl,
	read(X),
	(X = silverPlayer; X = goldenPlayer).
