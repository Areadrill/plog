:- use_module(library(random)).
:- include('board.pl').

piece(goldenPiece).
piece(silverPiece).
piece(flagship).

owner(goldenPlayer, goldenPiece).
owner(silverPlayer, silverPiece).
owner(goldenPlayer, flagship).

