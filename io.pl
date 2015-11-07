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
	player(X),!.

clearScreen:-write('\e[2J').

mainMenu:-
write('Welcome to Breakthru!'), nl, nl,
write('What would you like to do: '), nl,
write('1. Play game'), nl,
write('2. Tutorial'), nl,
write('3. Exit'), nl,
repeat,
getInt(Option, 'option'),
(Option = 1, clearScreen, playMenu;
Option = 2, clearScreen, tutorial;
Option = 3).

playMenu:-
write('How would you like to play: '), nl,
write('1. Player vs Player'), nl,
write('2. Player vs Bot'), nl,
write('3. Bot vs Bot'), nl,
getInt(Option, 'option'),
(Option = 1, clearScreen, startGame;
Option = 2, write('Under construction');
Option = 3, write('Under construction')).
