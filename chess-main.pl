% -*- Prolog -*-

% gplc chess-main.pl chess.pl
:- initialization(comando).

:- dynamic(posicao/4).


% usar o predicado argument_list(LISTA) para ver os argumentos dados
% e direcionar para o formato pretendido as acções
comando :- argument_list([F,A]), print(F), comando(A), init.

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

init_posicoes :- 
    retractall(posicao(_, _, _, _)),
    assertz(posicao('K', w, e, 1)),
    assertz(posicao('Q', w, d, 1)),
    assertz(posicao('R', w, a, 1)),
    assertz(posicao('R', w, h, 1)),
    assertz(posicao('N', w, b, 1)),
    assertz(posicao('N', w, g, 1)),
    assertz(posicao('B', w, c, 1)),
    assertz(posicao('B', w, f, 1)),
    assertz(posicao('P', w, a, 2)),
    assertz(posicao('P', w, b, 2)),
    assertz(posicao('P', w, c, 2)),
    assertz(posicao('P', w, d, 2)),
    assertz(posicao('P', w, e, 2)),
    assertz(posicao('P', w, f, 2)),
    assertz(posicao('P', w, g, 2)),
    assertz(posicao('P', w, h, 2)),
    assertz(posicao('P', b, a, 7)),
    assertz(posicao('P', b, b, 7)),
    assertz(posicao('P', b, c, 7)),
    assertz(posicao('P', b, d, 7)),
    assertz(posicao('P', b, e, 7)),
    assertz(posicao('P', b, f, 7)),
    assertz(posicao('P', b, g, 7)),
    assertz(posicao('P', b, h, 7)),
    assertz(posicao('K', b, e, 8)),
    assertz(posicao('Q', b, d, 8)),
    assertz(posicao('R', b, a, 8)),
    assertz(posicao('R', b, h, 8)),
    assertz(posicao('N', b, b, 8)),
    assertz(posicao('N', b, g, 8)),
    assertz(posicao('B', b, c, 8)),
    assertz(posicao('B', b, f, 8)).

mostrar_tabuleiro :-
    forall(between(1, 8, L), (write(L), mostrar_linha(L))), write('  a  b  c  d  e  f  g  h'), nl.

mostrar_linha(L) :-
    write(' '),
    forall(between(1, 8, C), (mostrar_posicao(C, L), write(' '))), nl.

mostrar_posicao(C, L) :-
    posicao(TIPO, COR, COLUNA, LINHA),
    coluna(COLUNA, C),
    LINHA = L,
    write(COR), write(TIPO).

mostrar_posicao(_, _) :- write('  ').

coluna(a, 1).
coluna(b, 2).
coluna(c, 3).
coluna(d, 4).
coluna(e, 5).
coluna(f, 6).
coluna(g, 7).
coluna(h, 8).

init :-
    init_posicoes,
    mostrar_tabuleiro,
    jogar. 


/* init :-
    init_posicoes,
    mostrar_tabuleiro. */

jogar :-
    ler_linha(jogada(B, P)),

    print(B), nl,
    atualizar_tabuleiro(w, B),
    mostrar_tabuleiro,

    print(P), nl,
    atualizar_tabuleiro(b, P),
    mostrar_tabuleiro,

    jogar.

ler_linha(X) :-
    gets(L),
    phrase(expr(X), L, []).

atualizar_tabuleiro(COR, move(TIPO, ColunaFinal, LinhaFinal)) :-
    TIPO = 'P',
    validar_peao(COR, ColunaFinal, LinhaFinal),
    posicao(TIPO, COR, ColunaFinal, _),
    retract(posicao(TIPO, COR, ColunaFinal, _)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'N',
    validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));
    
    true.


validar_peao(COR, Coluna, LinhaFinal) :-
    COR = w, posicao('P', COR, Coluna, LinhaAtual),
    LinhaAtual #= 2, LinhaFinal - LinhaAtual =< 2;

    COR = w, posicao('P', COR, Coluna, LinhaAtual),
    LinhaAtual #\= 2, LinhaFinal - LinhaAtual #=1;

    COR = b, posicao('P', COR, Coluna, LinhaAtual),
    LinhaAtual #= 7, LinhaAtual - LinhaFinal =< 2;

    COR = b, posicao('P', COR, Coluna, LinhaAtual),
    LinhaAtual #\= 7, LinhaAtual - LinhaFinal #= 1.

validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt + 1, LinhaInicial is LinhaFinal - 2,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt - 1, LinhaInicial is LinhaFinal - 2,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt - 2, LinhaInicial is LinhaFinal - 1,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt - 2, LinhaInicial is LinhaFinal + 1,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt + 2, LinhaInicial is LinhaFinal - 1,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt + 2, LinhaInicial is LinhaFinal + 1,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt + 1, LinhaInicial is LinhaFinal + 2,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial);

    coluna(ColunaFinal, ColunaFinalInt),
    ColunaInicialInt is ColunaFinalInt - 1, LinhaInicial is LinhaFinal + 2,
    coluna(ColunaInicial, ColunaInicialInt), posicao('N', COR, ColunaInicial, LinhaInicial).