getInt(X, Value):- repeat,
			write('Value for '), write(Value), write(': '),
			read(X1),
			integer(X1),			
			X is X1,
			!.
			
readCoordinates(X1, Y1):- 
 getInt(X1, 'x'),nl,
 getInt(Y1, 'y').

