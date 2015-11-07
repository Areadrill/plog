:- use_module(library(random)).
:- include('board.pl').
:- include('util.pl').
:- include('io.pl').

:- dynamic(flagships/1).
:- dynamic(silverPieces/1).
:- dynamic(goldenPieces/1).
:- dynamic(gameState/2).
:- dynamic(position/3).
:- dynamic(player/1).
:- dynamic(currentPlayer/1).
piece(goldenPiece).
piece(silverPiece).
piece(flagship).

player(goldenPlayer).
player(silverPlayer).

flagships(1).

owner(goldenPlayer, goldenPiece).
owner(silverPlayer, silverPiece).
owner(goldenPlayer, flagship).
opponent(silverPlayer, goldenPlayer).
opponent(goldenPlayer, silverPlayer).

state(begin).
state(game).
state(over).

breakthru:-
mainMenu.


startGame:- retractall(position(_,_,_)), assert(goldenPieces(0)), assert(silverPieces(0)),
	setupBoard,
	playGame.

teste:-fillBoard(0,0), asserta(position(1,1,goldenPiece)), asserta(position(0,0,silverPiece)), asserta(position(0,1,silverPiece)), asserta(position(0,2,silverPiece)), asserta(position(0,3,silverPiece)), asserta(position(4,4,goldenPiece)),
asserta(position(4,5,goldenPiece)), asserta(position(4,6,goldenPiece)), asserta(position(4,3,goldenPiece)).

playGame:-
	readPlayer(Player),
	assert(currentPlayer(Player)),
	repeat,
	retract(currentPlayer(CurrentPlayer)),
	takeTurn(CurrentPlayer, NewPlayer),
	assert(currentPlayer(NewPlayer)),
	printBoard,
	fail.

takeTurn(goldenPlayer, silverPlayer):-
(playerGolden(human), doPlayerMovement(goldenPlayer), printBoard, doPlayerMovement(goldenPlayer);
playerGolden(bot), validPlay(X, Y, Xf, Yf, goldenPlayer), doPlay(X, Y, Xf, Yf, goldenPlayer), printBoard,
validPlay(X1, Y1, X1f, Y1f, goldenPlayer), doPlay(X1, Y1, X1f, Y1f, goldenPlayer), printBoard), !.

takeTurn(silverPlayer, goldenPlayer):-
(playerSilver(human), doPlayerMovement(silverPlayer), printBoard, doPlayerMovement(silverPlayer);
playerSilver(bot), validPlay(X, Y, Xf, Yf, silverPlayer), doPlay(X, Y, Xf, Yf, silverPlayer), printBoard,
validPlay(X1, Y1, X1f, Y1f, silverPlayer), doPlay(X1, Y1, X1f, Y1f, silverPlayer), printBoard), !.

doPlayerMovement(Player):-write(Player), write(' chooses a piece to move:'), nl,
repeat,
readCoordinates(Xi,Yi),owner(Player,Piece),position(Xi,Yi,Piece), !,
nl,write('destination:'),nl,
repeat,
readCoordinates(Xf,Yf),
validPlay(Xi,Yi,Xf,Yf,Player),!,
doPlay(Xi,Yi,Xf,Yf,Player).

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
		printBoard,!,
	placePiece(goldenPlayer, N1).

placePiece(silverPlayer, 4).
placePiece(silverPlayer, N):- N1 is N+1, N< 4,
	write('New piece for silver player:'), nl,
	repeat,
	readCoordinates(X,Y),
	validateCoordinates(X,Y, silverPlayer),!,
	asserta(position(X,Y,silverPiece)),
	printBoard,!,
	placePiece(silverPlayer, N1).

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

doPlay(X,Y,Xf,Yf,Player):- validMove(X,Y,Xf,Yf,Player),
	owner(Player,Piece),retract(position(X,Y,Piece)),asserta(position(Xf,Yf,Piece)).

doPlay(X,Y,Xf,Yf,Player):- validCapture(X,Y,Xf,Yf,Player),
	owner(Player,Piece), retract(position(X,Y,Piece)),opponent(Player, Opponent), owner(Opponent,OpponentPiece), retract(position(Xf,Yf,OpponentPiece)), asserta(position(Xf,Yf,Piece)).

validPlay(X,Y,Xf,Yf,Player):- (validMove(X,Y,Xf,Yf,Player);validCapture(X,Y,Xf,Yf,Player)).

	validCapture(X,Y,Xf,Yf,Player):-
	 opponent(Player, Opponent),
	 owner(Opponent,OpponentPiece),
	 position(Xf,Yf,OpponentPiece),
	 (X =:= Xf-1; X =:= Xf+1),
	 (Y =:= Yf-1; Y =:= Yf+1).

	validMove(X,Y,Xf,Yf, Player):- %movimento normal
		owner(Player, Piece),
		position(X,Y,Piece),
		position(Xf,Yf, emptyCell),
		findall(Z, position(Xf,Yf,Z), [emptyCell]),
		emptySpace(X,Y,Xf,Yf).
