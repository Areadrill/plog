:- use_module(library(random)).
:- include('board.pl').
:- include('util.pl').
:- include('io.pl').

:- dynamic(flagships/1).
:- dynamic(silverPieces/1).
:- dynamic(goldenPieces/1).
:- dynamic(gameState/2).

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

teste(X):- createBoard(3,3,Board),
		replaceInBoard(2,2, boda, Board, NewBoard),
		getCell(NewBoard, 2, 2, boda).

startGame:- assert(goldenPieces(0)), assert(silverPieces(0)),
			createBoard(11,11,Board),
			replaceInBoard(5, 5, flagship, Board, NewBoard),
			setupBoard(NewBoard),
			currentState(boda).
			
			
setupBoard(Board):- assert(gameState(Board, goldenPlayer)),
					repeat,
					retract(gameState(CurrentBoard, CurrentPlayer)),
					write(CurrentPlayer),
					placePiece(CurrentBoard, CurrentPlayer, NewBoard, NewPlayer),
					printBoard(NewBoard, 11),
					assert(gameState(NewBoard, NewPlayer)),
					boardFull.
	
			
boardFull:- goldenPieces(N), flagships(F), silverPieces(J),
			J == 20, N == 12, F == 1.

placePiece(CurrentBoard, goldenPlayer, NewBoard, goldenPlayer):-
goldenPieces(J), J < 3,
 retract(goldenPieces(G)),
 write('New Piece Coordinates: '),
 repeat,
 readCoordinates(X, Y),
 validateCoordinates(CurrentBoard, X, Y, goldenPlayer),
 replaceInBoard(X, Y, goldenPiece, CurrentBoard, NewBoard),
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
 G1 is G+1,
 assert(goldenPieces(G1)),
 !.
 
 
 placePiece(CurrentBoard, silverPlayer, NewBoard, silverPlayer):-
 silverPieces(J), J =< 20,
  retract(silverPieces(G)),
 write('New Piece Coordinates: '),
 repeat,
 readCoordinates(X, Y),
 validateCoordinates(CurrentBoard, X, Y,  silverPlayer),
 replaceInBoard(X, Y, silverPiece, CurrentBoard, NewBoard),
 G1 is G+1,
 assert(silverPieces(G1)),
 !.
 
 
 
validateCoordinates(Board, X, Y, goldenPlayer):-
	X > 2, X < 8,
	Y > 2, Y < 8, 
	getCell(Board, X, Y, emptyCell), !.
 
validateCoordinates(Board, X, Y, silverPlayer):-
	\+(validateCoordinates(X, Y, goldenPlayer)).