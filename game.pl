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


goldenPieces(0).
silverPieces(0).
flagships(1).

owner(goldenPlayer, goldenPiece).
owner(silverPlayer, silverPiece).
owner(goldenPlayer, flagship).


state(begin).
state(game).
state(over).

startGame:- createBoard(11,11,Board),
			setupBoard(Board),
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
 retract(goldenPieces(G)), G =< 11,
 write('New Piece Coordinates: '),
 repeat,
 readCoordinates(X, Y),
 validateCoordinates(CureentBoard, X, Y, goldenPlayer),
 replaceInBoard(Y, X, goldenPiece, CurrentBoard, NewBoard),
 G1 is G+1,
 assert(goldenPieces(G1)),
 !.
 
 placePiece(CurrentBoard, goldenPlayer, NewBoard, silverPlayer).
 
 placePiece(CurrentBoard, silverPlayer, NewBoard, silverPlayer).
 
 
 
validateCoordinates(Board, X, Y, goldenPlayer):-
	X > 2, X < 8,
	Y > 2, Y < 8, 
	getCell(Board, X, Y, emptyCell), !.
 
validateCoordinates(X, Y, silverPlayer):-
	\+(validateCoordinates(X, Y, goldenPlayer)).