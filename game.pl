:- use_module(library(random)).
:- include('board.pl').
:- include('util.pl').

piece(goldenPiece).
piece(silverPiece).
piece(flagship).

owner(goldenPlayer, goldenPiece).
owner(silverPlayer, silverPiece).
owner(goldenPlayer, flagship).

startGame:- createBoard(11,11,BoardEmpty),
			replaceInBoard(5,5,flagship, BoardEmpty, BoardFlag),
			printBoard(BoardFlag,11).
	
	
	