% -*- Prolog -*-

% gplc chess-main.pl chess.pl
:- initialization(comando).

% usar o predicado argument_list(LISTA) para ver os argumentos dados
% e direcionar para o formato pretendido as acções
comando :- argument_list([F,A]), print(F), comando(A).

comando([]).
% comando([A|As]) :- argumento(A), algebrica(A), comando(As).
comando(A) :- argumento(A).

argumento(F) :- file_exists(F), see(F), !, format("\n[file ~w]\n\n", [F]).
argumento(U) :- format("[nao existe: ~w]\n", [U]), !, halt.

% algebrica([]).
% algebrica([A|As]) :- gets(A), algebrica(As).
algebrica(A) :- read_file(A).

gets(S) :- get0(C), gets([], C, S).
gets(S, 10, S).		% 10 é o newline
gets(S, -1, S).		% -1 é o end-of-file
gets(I, C, [C|O]) :- get0(CC), gets(I, CC, O).

read_file(X) :- gets(X), format("~s", [X]).

unload_file(FILENAME) :- seeing(FILENAME), seen.

% phrase(expr(jogada(A, B)), "e4 e5", []).
