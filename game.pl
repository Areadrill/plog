:- use_module(library(random)).
:- use_module(library(system)).
:- use_module(library(lists)).
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
:- dynamic(moved/1).
:- dynamic(moved/2).
:- dynamic(captured/0).

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
	setRandSeed,
	setupBoard,
	playGame.

teste:-fillBoard(0,0), asserta(position(1,1,goldenPiece)), asserta(position(0,0,silverPiece)), asserta(position(0,1,silverPiece)), asserta(position(0,2,silverPiece)), asserta(position(0,3,silverPiece)), asserta(position(4,4,goldenPiece)),
asserta(position(4,5,goldenPiece)), asserta(position(4,6,goldenPiece)), asserta(position(4,3,goldenPiece)), asserta(position(5,5,flagship)).

playGame:-
	readPlayer(Player),
	assert(currentPlayer(Player)),
	repeat,
	retract(currentPlayer(CurrentPlayer)),
	retractall(moved(_)), retractall(moved(_,_)),retractall(captured),
	takeTurn(CurrentPlayer, NewPlayer),
	assert(currentPlayer(NewPlayer)),
	printBoard,
	wonGame(Victor),!, clearScreen,
	write(Victor),write('won the game!').

wonGame(goldenPlayer):- position(_,0,flagship);position(0,_,flagship);position(11,_,flagship);position(_,11,flagship);\+position(_,_,silverPiece).
wonGame(silverPlayer):- \+position(_,_,flagship).

takeTurn(goldenPlayer, silverPlayer):-
(playerGolden(human), doPlayerMovement(goldenPlayer), printBoard,( (\+moved(flagship), \+captured, printBoard, doPlayerMovement(goldenPlayer)); (moved(flagship);captured) );
playerGolden(bot), validPlay(X, Y, Xf, Yf, goldenPlayer), doPlay(X, Y, Xf, Yf, goldenPlayer), printBoard,
randomPlay(goldenPlayer, Pred1), Pred =.. Pred1, Pred, (\+moved(flagship),\+captured,randomPlay(goldenPlayer, Pred1), Pred =.. Pred1, Pred);(moved(flagship);captured), printBoard), !.

takeTurn(silverPlayer, goldenPlayer):-
(playerSilver(human), doPlayerMovement(silverPlayer), printBoard, ((\+captured,doPlayerMovement(silverPlayer));captured);
playerSilver(bot), validPlay(X, Y, Xf, Yf, silverPlayer), doPlay(X, Y, Xf, Yf, silverPlayer), printBoard,
randomPlay(goldenPlayer, Pred1), Pred =.. Pred1, Pred, (captured;(\+captured,randomPlay(silverPlayer, Pred1), Pred =.. Pred1, Pred)), printBoard), !.


doPlayerMovement(Player):-
repeat,write(Player), write(' chooses a piece to move:'), nl,
readCoordinates(Xi,Yi),owner(Player,Piece),position(Xi,Yi,Piece),\+moved(Xi,Yi), !,
nl,write('destination:'),nl,
repeat,
readCoordinates(Xf,Yf),
validPlay(Xi,Yi,Xf,Yf,Player),!,
position(Xi,Yi,Piece),
asserta(moved(Piece)),
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
	owner(Player,Piece),retract(position(X,Y,Piece)),asserta(position(Xf,Yf,Piece)), asserta(moved(Xf,Yf)).

doPlay(X,Y,Xf,Yf,Player):- validCapture(X,Y,Xf,Yf,Player),
	owner(Player,Piece), retract(position(X,Y,Piece)),opponent(Player, Opponent), owner(Opponent,OpponentPiece), retract(position(Xf,Yf,OpponentPiece)), asserta(position(Xf,Yf,Piece)),asserta(captured), asserta(moved(Xf,Yf)).

validPlay(X,Y,Xf,Yf,Player):- (validMove(X,Y,Xf,Yf,Player);validCapture(X,Y,Xf,Yf,Player)),\+moved(X,Y).

validCapture(X,Y,Xf,Yf,Player):-
 owner(Player, Piece),
 position(X,Y,Piece),
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

randomPlay(CurrPlayer, Pred):-
findall([Xi,Yi,Xf,Yf], validMove(Xi,Yi,Xf,Yf,CurrPlayer), Possiveis),
length(Possiveis, N),
random(0, N, NList),
nth0(NList, Possiveis, ChosenPlay),
append(ChosenPlay, [CurrPlayer], AlmostPred),
append([doPlay], AlmostPred, Pred).
