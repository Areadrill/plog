
%create predicates based on ferrolhos implementation

createBoard(_, 0, L).
createBoard(N, M, [H|T]):-
  M > 0,
  createLine(N, H),
  M1 is M-1,
  createBoard(N, M1, T).

createLine(0, L).
createLine(N, [N|T]):-
  N > 0,
  N1 is N-1,
  createLine(N1, T).

printBoard([]).
printBoard([H|T]):-
  printLine(H, '|'), nl,
  printBoard(T).


printLine([], Sep):-
  write(Sep).
printLine([H|T], Sep):-
  write(Sep), write(H),
  printLine(T, Sep).


replaceInLine(0, Char, [Char|T]).
replaceInLine(Y, Char, [H|T]):-
  Y > 0,
  Y1 is Y-1,
  replaceInLine(Y1, Char, T).

replaceInBoard(Y, 0, Char, [H|T]):-
  replaceInLine(Y, Char, H).

replaceInBoard(Y, X, Char, [H|T]):-
  X > 0,
  X1 is X-1,
  replaceInBoard(Y, X1, Char, T).
