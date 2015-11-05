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

currentPlayer(goldenPlayer).

flagships(1).

owner(goldenPlayer, goldenPiece).
owner(silverPlayer, silverPiece).
owner(goldenPlayer, flagship).


state(begin).
state(game).
state(over).


startGame:- retractall(position(_,_,_)), assert(goldenPieces(0)), assert(silverPieces(0)),
	setupBoard,
	playGame.

playGame:-
	readPlayer(Player),
	assert(player(Player)),
	repeat,
	retract(player(NPlayer)),
	takeTurn(NPlayer, CurrentBoard, NewBoard, NewPlayer),
	printBoard(0, 0),
	assert(player(NewPlayer)),
	fail.



setupBoard:-
	fillBoard,
	asserta(position(5,5,flagship)),
	placePiece(goldenPlayer, 0).

placePiece(goldenPlayer, 4):- placePiece(silverPlayer, 0).
placePiece(goldenPlayer, N):- N1 is N+1, N < 4,
	write('New piece for golden player:'), nl,
	repeat,
	readCoordinates(X,Y),
	validateCoordinates(X,Y, goldenPlayer),!,
	asserta(position(X,Y,goldenPiece)),
		printBoard,
	placePiece(goldenPlayer, N1).

placePiece(silverPlayer, 4).
placePiece(silverPlayer, N):- N1 is N+1, N< 4,
	write('New piece for silver player:'), nl,
	repeat,
	readCoordinates(X,Y),
	validateCoordinates(X,Y, silverPlayer),!,
	asserta(position(X,Y,silverPiece)),
	placePiece(silverPiece, N1).


boardFull:- goldenPieces(N), flagships(F), silverPieces(J),
	J == 4, %devia ser 20
	N == 4, %devia ser 11
	F == 1. %taboum


validateCoordinates( X, Y, goldenPlayer):-
	X > 2, X < 8,
	Y > 2, Y < 8,
	findall(Z, position(X,Y,Z), [emptyCell]).

validateCoordinates(X, Y, silverPlayer):-
	\+(validateCoordinates( X, Y, goldenPlayer)),
	findall(Z, position(X,Y,Z), [emptyCell]).




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
