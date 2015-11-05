
exampleInitial([ [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                 [' ',' ',' ','p','p','p','p','p',' ',' ',' '],
                 [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                 [' ','p',' ',' ','d','d','d',' ',' ','p',' '],
                 [' ','p',' ','d',' ',' ',' ','d',' ','p',' '],
                 [' ','p',' ','d',' ','F',' ','d',' ','p',' '],
                 [' ','p',' ','d',' ',' ',' ','d',' ','p',' '],
                 [' ','p',' ',' ','d','d','d',' ',' ','p',' '],
                 [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                 [' ',' ','p','p','p','p','p','p',' ',' ',' '],
                 [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '] ]).

exampleMiddle([ [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ','p','p',' ','d','p',' ',' ',' '],
                [' ',' ',' ','d',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ','p','d',' ','d','p',' ',' ',' '],
                [' ','p',' ',' ',' ','p',' ','d',' ','p',' '],
                [' ','p',' ','d',' ','F',' ','d',' ','p',' '],
                [' ','p',' ','d',' ',' ',' ','d',' ','p',' '],
                [' ','p',' ',' ','d','d','d',' ',' ','p',' '],
                [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ','p','p','p','p','p','p',' ',' ',' '],
                [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '] ]).

exampleEnd([ [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ','p','p',' ','d','p',' ',' ',' '],
                [' ',' ',' ','d',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ',' ','p','d',' ','d','p',' ',' ',' '],
                [' ','p',' ',' ',' ','p',' ','d',' ','p',' '],
                [' ','p',' ','d',' ',' ',' ',' ','d','p',' '],
                [' ','p',' ','d',' ',' ',' ','d',' ','p',' '],
                [' ','p',' ','p','d',' ','d',' ',' ','p',' '],
                [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' '],
                [' ',' ','p','','d',' ','p','p',' ',' ',' '],
                [' ',' ',' ',' ',' ','F',' ',' ',' ',' ',' '] ]).


symbol(emptyCell, ' ').
symbol(flagship, 'F').
symbol(silverPiece, 'p').
symbol(goldenPiece, 'd').

getBoardElement([H|_], Piece, X, Y):- getElement(H, Piece).
getBoardElement([_|T], Piece):- getBoardElement(T, Piece).
getBoardElement([], _):- fail.

getElement([H|_], H).
getElement([_|T], Piece):- getElement(T, Piece).
getElement([], _):- fail.



createBoard(_, 0, []).
createBoard(N, M, [H|T]):-
  M > 0,
  createLine(N, H),
  M1 is M-1,
  createBoard(N, M1, T).

createLine(0, []).
createLine(N, [emptyCell|T]):-
  N > 0,
  N1 is N-1,
  createLine(N1, T).

printBoard([], 0):-write(' ----------------------------------------------'),nl,
	write('   0   1   2   3   4   5   6   7   8   9  10'), nl.

printBoard([H|T], N):-
  N1 is N-1,
  Print is 10-N1,
  write(' ---------------------------------------------'),nl,
  write(Print), printLine(H, '|'), nl,
  printBoard(T, N1).

printLine([], Sep):-
  write(Sep).
printLine([H|T], Sep):-
  symbol(H, Symb),
  write(Sep),write(' '), write(Symb), write(' '),
  printLine(T, Sep).


replaceInLine(0, Char, [_|T], [Char|T]).
replaceInLine(Y, Char, [H|T], [H|R]):-
  Y > 0,
  Y1 is Y-1,
  replaceInLine(Y1, Char, T, R).

replaceInBoard(Y, 0, Char, [H|T], [L|T]):-
  replaceInLine(Y, Char, H, L).

replaceInBoard(Y, X, Char, [H|T], [H|R]):-
  X > 0,
  X1 is X-1,
  replaceInBoard(Y, X1, Char, T, R).


getCell([H|_], X, 0, E):-
	getCellL(H, X, E).
getCell([_|T], X, Y, E):-
	Y1 is Y-1,
	getCell(T, X, Y1, E).

getCellL([H|_], 0, H).
getCellL([_|T], X, E):-
	X1 is X-1,
	getCellL(T, X1, E).

fillBoardLine(11,Y).
fillBoardLine(X,Y):-assertz(position(X,Y, emptyCell)), X1 is X+1, fillBoardLine(X1,Y).

fillBoard(X,11).
fillBoard(X,Y):-fillBoardLine(X,Y), Y1 is Y+1, fillBoard(X,Y1).
