
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
	write('   0   1   2   3   4   5   6   7   8   9  10').
	
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


replaceInLine(0, Char, [H|T], [Char|T]).
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

  
test:-
	createBoard(8, 8, L),
	printBoard(L), nl, nl,
	replaceInBoard(4, 4, 'x', L, Nl),
	printBoard(Nl).