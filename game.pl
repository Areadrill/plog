:- use_module(library(random)).
:- include('board.pl').
:- include('util.pl').
:- include('io.pl').

:- dynamic(flagships/1).
:- dynamic(silverPieces/1).
:- dynamic(goldenPieces/1).
:- dynamic(gameState/2).
:- dynamic(position/3).

piece(goldenPiece).
piece(silverPiece).
piece(flagship).

player(goldenPlayer).
player(silverPlayer).



flagships(1).

owner(goldenPlayer, goldenPiece).
owner(silverPlayer, silverPiece).
owner(goldenPlayer, flagship).


state(begin).
state(game).
state(over).

teste(_):- createBoard(3,3,Board),
		replaceInBoard(2,2, boda, Board, NewBoard),
		getCell(NewBoard, 2, 2, boda).

startGame:- assert(goldenPieces(0)), assert(silverPieces(0)),
			createBoard(11,11,Board),
			replaceInBoard(5, 5, flagship, Board, NewBoard),
			setupBoard(NewBoard, NewerBoard),
			playGame(NewerBoard).

playGame(Board):-
	readPlayer(Player),
	assert(gameState(Board, Player)),
	repeat,
	retract(gameState(CurrentBoard, CurrentPlayer)),
	takeTurn(Player, CurrentBoard, NewBoard, NewPlayer),
	printBoard(NewBoard, 11),
	assert(gameState(NewBoard, Player)),
	fail. %mudar para winning condition



setupBoard(Board, NewBoard):- assert(gameState(Board, goldenPlayer)),
					fillBoard(11,11),
					repeat,
					retract(gameState(CurrentBoard, CurrentPlayer)),
					write(CurrentPlayer),
					placePiece(CurrentBoard, CurrentPlayer, NewBoard, NewPlayer),
					printBoard(NewBoard, 11),
					assert(gameState(NewBoard, NewPlayer)),
					boardFull.


boardFull:- goldenPieces(N), flagships(F), silverPieces(J),
			J == 4, %devia ser 20
			N == 4, %devia ser 11
			F == 1. %taboum

placePiece(CurrentBoard, goldenPlayer, NewBoard, goldenPlayer):-
goldenPieces(J), J < 3,
 retract(goldenPieces(G)),
 write('New Piece Coordinates: '),
 repeat,
 readCoordinates(X, Y),
 validateCoordinates(CurrentBoard, X, Y, goldenPlayer),
 replaceInBoard(X, Y, goldenPiece, CurrentBoard, NewBoard),
 asserta( (position(X,Y, goldenPiece)) ),
 G1 is G+1,
 assert(goldenPieces(G1)),
 !.

 placePiece(CurrentBoard, goldenPlayer, NewBoard, silverPlayer):-
 retract(goldenPieces(G)),
 write('New Piece Coordinates: '),
 repeat,
 readCoordinates(X, Y),
 validateCoordinates(CurrentBoard, X, Y, goldenPlayer),
 replaceInBoard(X, Y, goldenPiece, CurrentBoard, NewBoard),
 asserta( (position(X,Y,goldenPiece)) ),
 G1 is G+1,
 assert(goldenPieces(G1)),
 !.


 placePiece(CurrentBoard, silverPlayer, NewBoard, silverPlayer):-
 silverPieces(J), J =< 3, %aqui e 20
  retract(silverPieces(G)),
 write('New Piece Coordinates: '),
 repeat,
 readCoordinates(X, Y),
 validateCoordinates(CurrentBoard, X, Y,  silverPlayer),
 replaceInBoard(X, Y, silverPiece, CurrentBoard, NewBoard),
 asserta( (position(X,Y, silverPiece)) ),
 G1 is G+1,
 assert(silverPieces(G1)),
 !.



validateCoordinates(Board, X, Y, goldenPlayer):-
	X > 2, X < 8,
	Y > 2, Y < 8,
	getCell(Board, X, Y, emptyCell), !.

validateCoordinates(Board, X, Y, silverPlayer):-
	\+(validateCoordinates(Board, X, Y, goldenPlayer)),
	getCell(Board, X, Y, emptyCell), !.


takeTurn(Player, Board, NewBoard, NewPlayer):-
	write('Turn for '), write(Player), nl,
	write('Choose the piece you want to move: '), nl,
	readCoordinates(XI, YI),
	write('Where do you want to move it to: '), nl,
	readCoordinates(XF, YF),
	evaluateMove(XI, YI, XF, YF, Board, NewBoard, Player).



evaluateMove(Xi, Yi, Xf, Yf, Board, NewererBoard, Player):-
	getCell(Board, Xi, Yi, Piece),
	owner(Player, Piece),
	calculateDistances(Xi, Yi, Xf, Yf, DX, DY),
	(DX \= 0, DY is 0;
	DX is 0, DY \= 0;
	DX is 1, DY is 1;
	DX is 1, DY is -1;
	DX is -1, DY is 1;
	DX is -1, DY is -1),
	replaceInBoard(Xi, Yi, emptyCell, Board, NewBoard),
	replaceInBoard(Xf, Yf, Piece, NewBoard, NewererBoard).

calculateDistances(Xi, Yi, Xf, Yf, DX, DY):-
	DX is Xf - Xi,
	DY is Yf - Yi.

	emptySpace(X,Y,X,Y):- fail.
	emptySpace(X,Y,Xf,Y):- X>Xf, X1 is X-1, emptyCellsBetween(X1,Y,Xf,Y).
	emptySpace(X,Y,Xf,Y):- X < Xf, X1 is X+1, emptyCellsBetween(X1, Y, Xf, Y).
	emptySpace(X,Y,X,Yf):- Y> Yf, Y1 is Y-1, emptyCellsBetween(X,Y1,X, Yf).
	emptySpace(X,Y,X,Yf):- Y < Yf, Y1 is Y+1, emptyCellsBetween(X,Y1,X,Yf).

	emptyCellsBetween(X,Y,X,Y):- findall(Z, position(X,Y,Z), [emptyCell]), !.
	emptyCellsBetween(X,Y,X,Yf):-Y>Yf,Y1 is Y-1, findall(Z, position(X,Y,Z), [emptyCell]), emptyCellsBetween(X,Y1,X,Yf).
	emptyCellsBetween(X,Y,X,Yf):-Yf1 is Yf-1, findall(Z, position(X,Yf,Z), [emptyCell]), emptyCellsBetween(X,Y,X,Yf1).
	emptyCellsBetween(X,Y,Xf,Y):-X>Xf,X1 is X-1, findall(Z, position(X,Y,Z), [emptyCell]), emptyCellsBetween(X1,Y,Xf,Y).
	emptyCellsBetween(X,Y,Xf,Y):-Xf1 is Xf-1, findall(Z, position(Xf,Y,Z), [emptyCell]), emptyCellsBetween(X,Y,Xf1,Y).

	validMove(X,Y,Xf,Yf, Player):-
		owner(Player, Piece),
		position(X,Y,Piece),
		position(Xf,Yf, emptyCell),
		findall(Z, position(Xf,Yf,Z), [emptyCell]),
		emptySpace(X,Y,Xf,Yf).
