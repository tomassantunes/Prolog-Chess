% -*- Prolog -*-

% gplc chess-main.pl chess.pl
:- initialization(comando).

:- dynamic(posicao/4).

% usar o predicado argument_list(LISTA) para ver os argumentos dados
% e direcionar para o formato pretendido as acções
comando :- argument_list([A]), comando(A), init.

comando([]).
comando(A) :- argumento(A).

argumento(F) :- file_exists(F), see(F), !, format("\n[file ~w]\n\n", [F]).
argumento(U) :- format("[nao existe: ~w]\n", [U]), !, halt.

gets(S) :- get0(C), gets([], C, S).
gets(S, 10, S).		% 10 é o newline
gets(S, -1, S).		% -1 é o end-of-file
gets(I, C, [C|O]) :- get0(CC), gets(I, CC, O).

unload_file(FILENAME) :- seeing(FILENAME), seen.

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
    forall(between(1, 8, L), (linha(L, LL), write(LL), mostrar_linha(LL))), write('  a  b  c  d  e  f  g  h'), nl.

mostrar_linha(L) :-
    write(' '),
    forall(between(1, 8, C), (mostrar_posicao(C, L), write(' '))), nl.

mostrar_posicao(C, L) :-
    posicao(TIPO, COR, COLUNA, LINHA),
    coluna(COLUNA, C),
    LINHA = L,
    desenho(TIPO, COR, P),
    
    write(P), write(' ').

mostrar_posicao(_, _) :- write('  ').

desenho('P', w, '♙').
desenho('B', w, '♗').
desenho('N', w, '♘').
desenho('R', w, '♖').
desenho('Q', w, '♕').
desenho('K', w, '♔').
desenho('P', b, '♟').
desenho('B', b, '♝').
desenho('N', b, '♞').
desenho('R', b, '♜').
desenho('Q', b, '♛').
desenho('K', b, '♚').

linha(1, 8).
linha(2, 7).
linha(3, 6).
linha(4, 5).
linha(5, 4).
linha(6, 3).
linha(7, 2).
linha(8, 1).

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

    print(B), print(' -> brancas'), nl,
    atualizar_tabuleiro(w, B),
    mostrar_tabuleiro,

    print(P), print(' -> pretas'), nl,
    atualizar_tabuleiro(b, P),
    mostrar_tabuleiro,

    jogar;

    ler_linha(jogada(B, P, R)),

    print(B), print(' -> brancas'), nl,
    atualizar_tabuleiro(w, B),
    mostrar_tabuleiro,

    print(P), print(' -> pretas'), nl,
    atualizar_tabuleiro(b, P),
    mostrar_tabuleiro,

    print(R), nl,
    atualizar_tabuleiro(b, R),
    mostrar_tabuleiro,

    jogar. 

ler_linha(X) :-
    gets(L),
    phrase(lj(X), L, []).

% movimentos básicos
atualizar_tabuleiro(COR, movimento(TIPO, ColunaFinal, LinhaFinal)) :-
    TIPO = 'P',
    validar_peao(COR, ColunaFinal, LinhaFinal),
    posicao(TIPO, COR, ColunaFinal, _),
    retract(posicao(TIPO, COR, ColunaFinal, _)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'N',
    validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));
    
    TIPO = 'B',
    validar_bispo(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'R',
    validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'Q',
    validar_rainha(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'K',
    validar_rei(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    true.

% movimentos de peões com take
atualizar_tabuleiro(COR, movimento(TIPO, ColunaInicial, captura, ColunaFinal, LinhaFinal)) :-
    validar_peao_captura(COR, ColunaInicial, ColunaFinal, LinhaFinal, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    true.

% movimentos de peões com take e xeque ou xeque-mate
atualizar_tabuleiro(COR, movimento(TIPO, ColunaInicial, captura, ColunaFinal, LinhaFinal, A)) :-
    validar_peao_captura(COR, ColunaInicial, ColunaFinal, LinhaFinal, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)), 
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A); true.

% movimentos do resto das peças com take
atualizar_tabuleiro(COR, movimento(TIPO, captura, ColunaFinal, LinhaFinal)) :-
    TIPO = 'N', validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'B', validar_bispo_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'R', validar_torre_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'Q', validar_rainha_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'K', validar_rei_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));
    
    true.

% movimentos do resto das peças com xeque ou xeque-mate
atualizar_tabuleiro(COR, movimento(TIPO, ColunaFinal, LinhaFinal, xeque)) :-
    TIPO = 'N', validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque);

    TIPO = 'B', validar_bispo(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque);

    TIPO = 'R',validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque);

    TIPO = 'Q', validar_rainha(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque);

    true.

% movimentos do resto das peças com xeque ou xeque-mate
atualizar_tabuleiro(COR, movimento(TIPO, ColunaFinal, LinhaFinal, xeque-mate)) :-
    TIPO = 'N', validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque-mate);

    TIPO = 'B', validar_bispo(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque-mate);

    TIPO = 'R',validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque-mate);

    TIPO = 'Q', validar_rainha(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao(_, _, ColunaFinal, LinhaFinal), retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, xeque-mate);

    true.

% movimentos do resto das peças com take e xeque ou xeque-mate
atualizar_tabuleiro(COR, movimento(TIPO, captura, ColunaFinal, LinhaFinal, A)) :-
    TIPO = 'N', validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'B', validar_bispo_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'R', validar_torre_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'Q', validar_rainha_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    true.

% movimentos de peças em que haja a eventualidade de escolher a peça que está na coluna x
atualizar_tabuleiro(COR, movimento(TIPO, ColunaInicial, ColunaFinal, LinhaFinal)) :-
    TIPO = 'N', validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'R', validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    true.

% movimentos de peças em que haja a eventualidade de escolher a peça que está na linha x
atualizar_tabuleiro(COR, movimento(TIPO, LinhaInicial, ColunaFinal, LinhaFinal)) :-
    TIPO = 'N', validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'R', validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    true.

% movimentos de peças com take em que haja a eventualidade de escolher a peça que está na coluna x
atualizar_tabuleiro(COR, movimento(TIPO, ColunaInicial, captura, ColunaFinal, LinhaFinal)) :-
    TIPO = 'N', validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'R', validar_torre_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    true.

% movimentos de peças com take em que haja a eventualidade de escolher a peça que está na linha x
atualizar_tabuleiro(COR, movimento(TIPO, LinhaInicial, captura, ColunaFinal, LinhaFinal)) :-
    TIPO = 'N', validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    TIPO = 'R', validar_torre_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal));

    true.

% movimentos de peças com take e xeque ou xeque-mate em que haja a eventualidade de escolher a peça que está na coluna x
atualizar_tabuleiro(COR, movimento(TIPO, ColunaInicial, ColunaFinal, LinhaFinal, A)) :-
    TIPO = 'N', validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'R', validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    true.

% movimentos de peças com xeque ou xeque-mate em que haja a eventualidade de escolher a peça que está na linha x
atualizar_tabuleiro(COR, movimento(TIPO, LinhaInicial, ColunaFinal, LinhaFinal, A)) :-
    TIPO = 'N', validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'R',validar_torre(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);
    true.

% movimentos de peças com take e xeque ou xeque-mate em que haja a eventualidade de escolher a peça que está na coluna x
atualizar_tabuleiro(COR, movimento(TIPO, ColunaInicial, captura, ColunaFinal, LinhaFinal, A)) :-
    TIPO = 'N', validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'R', validar_torre_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    true.

% movimentos de peças com take e xeque ou xeque-mate em que haja a eventualidade de escolher a peça que está na linha x
atualizar_tabuleiro(COR, movimento(TIPO, LinhaInicial, captura, ColunaFinal, LinhaFinal, A)) :-
    TIPO = 'N', validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    TIPO = 'R', validar_torre_captura(COR, TIPO, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    retract(posicao(TIPO, COR, ColunaInicial, LinhaInicial)), retract(posicao(_, _, ColunaFinal, LinhaFinal)),
    assertz(posicao(TIPO, COR, ColunaFinal, LinhaFinal)), em_xeque(COR, A);

    true.

atualizar_tabuleiro(COR, CASTLE) :-
    COR = 'w',
    CASTLE = 'roque_curto',
    validar_roque_curto(COR),
    retract(posicao('K', COR, e, 1)), retract(posicao('R', COR, h, 1)),
    assertz(posicao('K', COR, g, 1)), assertz(posicao('R', COR, f, 1));

    COR = 'b',
    CASTLE = 'roque_curto',
    validar_roque_curto(COR),
    retract(posicao('K', COR, e, 8)), retract(posicao('R', COR, h, 8)),
    assertz(posicao('K', COR, g, 8)), assertz(posicao('R', COR, f, 8));

    COR = 'w',
    CASTLE = 'roque_longo',
    validar_roque_longo(COR),
    retract(posicao('K', COR, e, 1)), retract(posicao('R', COR, a, 1)),
    assertz(posicao('K', COR, c, 1)), assertz(posicao('R', COR, d, 1));

    COR = 'b',
    CASTLE = 'roque_longo',
    validar_roque_longo(COR),
    retract(posicao('K', COR, e, 8)), retract(posicao('R', COR, a, 8)),
    assertz(posicao('K', COR, c, 8)), assertz(posicao('R', COR, d, 8));

    true.

em_xeque(COR, XEQUE) :-
    COR = w, XEQUE = xeque, print('PRETAS em XEQUE'), nl;
    COR = b, XEQUE = xeque, print('BRANCAS em XEQUE'), nl;

    COR = w, XEQUE = xeque-mate, print('PRETAS em XEQUE-MATE'), nl;
    COR = b, XEQUE = xeque-mate, print('BRANCAS em XEQUE-MATE'), nl.

validar_peao(COR, Coluna, LinhaFinal) :-
    COR = w, posicao('P', COR, Coluna, LinhaAtual), \+ posicao(_, _, Coluna, LinhaFinal),
    LinhaAtual #= 2, LinhaFinal - LinhaAtual =< 2;

    COR = w, posicao('P', COR, Coluna, LinhaAtual), \+ posicao(_, _, Coluna, LinhaFinal),
    LinhaAtual #\= 2, LinhaFinal - LinhaAtual #=1;

    COR = b, posicao('P', COR, Coluna, LinhaAtual), \+ posicao(_, _, Coluna, LinhaFinal),
    LinhaAtual #= 7, LinhaAtual - LinhaFinal =< 2;

    COR = b, posicao('P', COR, Coluna, LinhaAtual), \+ posicao(_, _, Coluna, LinhaFinal),
    LinhaAtual #\= 7, LinhaAtual - LinhaFinal #= 1.

validar_peao_captura(COR, ColunaInicial, ColunaFinal, LinhaFinal, LinhaInicial) :-
    COR = w, posicao('P', COR, ColunaInicial, LinhaInicial), \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal),
    coluna(ColunaFinal, ColunaFinalInt), ColunaInicialInt is ColunaFinalInt - 1, coluna(ColunaInicial, ColunaInicialInt),
    LinhaInicial #= LinhaFinal - 1;

    COR = w, posicao('P', COR, ColunaInicial, LinhaInicial), \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal),
    coluna(ColunaFinal, ColunaFinalInt), ColunaInicialInt is ColunaFinalInt + 1, coluna(ColunaInicial, ColunaInicialInt),
    LinhaInicial #= LinhaFinal - 1;

    COR = b, posicao('P', COR, ColunaInicial, LinhaInicial), \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal),
    coluna(ColunaFinal, ColunaFinalInt), ColunaInicialInt is ColunaFinalInt - 1, coluna(ColunaInicial, ColunaInicialInt),
    LinhaInicial #= LinhaFinal + 1;

    COR = b, posicao('P', COR, ColunaInicial, LinhaInicial), \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal),
    coluna(ColunaFinal, ColunaFinalInt), ColunaInicialInt is ColunaFinalInt + 1, coluna(ColunaInicial, ColunaInicialInt),
    LinhaInicial #= LinhaFinal + 1.

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

validar_cavalo_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    COR = 'w',
    validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal);

    COR = 'b',
    validar_cavalo(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal).

diagonal_direita_cima(COR, P, CF, LF, CI, LI) :-
    coluna(CF, CFInt), CFIntI is CFInt - 1, coluna(CI, CFIntI), LI is LF - 1,
    posicao(P, COR, CI, LI);

    coluna(CF, CFInt), CFIntI is CFInt - 1, coluna(CFF, CFIntI), LFF is LF - 1,
    \+ posicao(_, _, CFF, LFF), diagonal_direita_cima(COR, P, CFF, LFF, CI, LI).

diagonal_direita_baixo(COR, P, CF, LF, CI, LI) :-
    coluna(CF, CFInt), CFIntI is CFInt - 1, coluna(CI, CFIntI), LI is LF + 1,
    posicao(P, COR, CI, LI);

    coluna(CF, CFInt), CFIntI is CFInt - 1, coluna(CFF, CFIntI), LFF is LF + 1,
    \+ posicao(_, _, CFF, LFF), diagonal_direita_baixo(COR, P, CFF, LFF, CI, LI).

diagonal_esquerda_cima(COR, P, CF, LF, CI, LI) :-
    coluna(CF, CFInt), CFIntI is CFInt + 1, coluna(CI, CFIntI), LI is LF - 1,
    posicao(P, COR, CI, LI);

    coluna(CF, CFInt), CFIntI is CFInt + 1, coluna(CFF, CFIntI), LFF is LF - 1,
    \+ posicao(_, _, CFF, LFF), diagonal_esquerda_cima(COR, P, CFF, LFF, CI, LI).

diagonal_esquerda_baixo(COR, P, CF, LF, CI, LI) :-
    coluna(CF, CFInt), CFIntI is CFInt + 1, coluna(CI, CFIntI), LI is LF + 1,
    posicao(P, COR, CI, LI);

    coluna(CF, CFInt), CFIntI is CFInt + 1, coluna(CFF, CFIntI), LFF is LF + 1,
    \+ posicao(_, _, CFF, LFF), diagonal_esquerda_baixo(COR, P, CFF, LFF, CI, LI).

validar_bispo(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    diagonal_direita_cima(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial);
    diagonal_direita_baixo(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial);
    diagonal_esquerda_cima(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial);
    diagonal_esquerda_baixo(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial).

validar_bispo_captura(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    COR = w, validar_bispo(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal);

    COR = b, validar_bispo(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal).

validar_torre(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    posicao(P, COR, ColunaInicial, LinhaFinal), posicao(P, COR, ColunaInicial, LinhaInicial), coluna(ColunaInicial, ColunaInicialInt),
    coluna(ColunaFinal, ColunaFinalInt), ColunaInicialInt #< ColunaFinalInt, CIInt is ColunaInicialInt + 1, CFInt is ColunaFinalInt - 1,
    forall(between(CIInt, CFInt, C), (coluna(CC, C), \+ posicao(_, _, CC, LinhaInicial))), LinhaInicial = LinhaFinal;

    posicao(P, COR, ColunaInicial, LinhaFinal), posicao(P, COR, ColunaInicial, LinhaInicial), coluna(ColunaInicial, ColunaInicialInt),
    coluna(ColunaFinal, ColunaFinalInt), ColunaInicialInt #> ColunaFinalInt, CIInt is ColunaInicialInt - 1, CFInt is ColunaFinalInt + 1,
    forall(between(CFInt, CIInt, C), (coluna(CC, C), \+ posicao(_, _, CC, LinhaInicial))), LinhaInicial = LinhaFinal;

    posicao(P, COR, ColunaFinal, LinhaInicial), posicao(P, COR, ColunaInicial, LinhaInicial), LinhaInicial #< LinhaFinal,
    LI is LinhaInicial + 1, LF is LinhaFinal - 1, forall(between(LI, LF, L), \+ posicao(_, _, ColunaInicial, L)), ColunaInicial = ColunaFinal;
    posicao(P, COR, ColunaFinal, LinhaInicial), posicao(P, COR, ColunaInicial, LinhaInicial), LinhaInicial #> LinhaFinal,
    LI is LinhaInicial - 1, LF is LinhaFinal + 1, forall(between(LF, LI, L), \+ posicao(_, _, ColunaInicial, L)), ColunaInicial = ColunaFinal.

validar_torre_captura(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    COR = w, validar_torre(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal);

    COR = b, validar_torre(COR, P, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal).

validar_rainha(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    validar_bispo(COR, 'Q', ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial);
    validar_torre(COR, 'Q', ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial).

validar_rainha_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    COR = w, validar_rainha(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal);

    COR = b, validar_rainha(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal).

validar_rei(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    LinhaInicial is LinhaFinal - 1, ColunaInicial = ColunaFinal, posicao('K', COR, ColunaInicial, LinhaInicial);
    LinhaInicial is LinhaFinal + 1, ColunaInicial = ColunaFinal, posicao('K', COR, ColunaInicial, LinhaInicial);

    LinhaInicial = LinhaFinal, coluna(ColunaFinal, CI), CII is CI - 1, coluna(ColunaInicial, CII), posicao('K', COR, ColunaInicial, LinhaInicial);
    LinhaInicial = LinhaFinal, coluna(ColunaFinal, CI), CII is CI + 1, coluna(ColunaInicial, CII), posicao('K', COR, ColunaInicial, LinhaInicial);

    LinhaInicial is LinhaFinal - 1, coluna(ColunaFinal, CI), CII is CI - 1, coluna(ColunaInicial, CII), posicao('K', COR, ColunaInicial, LinhaInicial);
    LinhaInicial is LinhaFinal + 1, coluna(ColunaFinal, CI), CII is CI - 1, coluna(ColunaInicial, CII), posicao('K', COR, ColunaInicial, LinhaInicial);

    LinhaInicial is LinhaFinal - 1, coluna(ColunaFinal, CI), CII is CI + 1, coluna(ColunaInicial, CII), posicao('K', COR, ColunaInicial, LinhaInicial);
    LinhaInicial is LinhaFinal + 1, coluna(ColunaFinal, CI), CII is CI + 1, coluna(ColunaInicial, CII), posicao('K', COR, ColunaInicial, LinhaInicial).

validar_rei_captura(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial) :-
    COR = w, validar_rei(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, b, ColunaFinal, LinhaFinal);

    COR = b, validar_rei(COR, ColunaFinal, LinhaFinal, ColunaInicial, LinhaInicial),
    \+ posicao('K', _, ColunaFinal, LinhaFinal), posicao(_, w, ColunaFinal, LinhaFinal).

validar_roque_curto(COR) :-
    COR = 'w',
    % rei em e1 e torre em h1, f1 e g1 estão vazios,
    posicao('K', COR, e, 1), posicao('R', COR, h, 1), \+ posicao(_, _, f, 1), \+ posicao(_, _, g, 1);
    
    COR = 'b',
    % rei em e8 e torre em h8, f8 e g8 estão vazios 
    posicao('K', COR, e, 8), posicao('R', COR, h, 8), \+ posicao(_, _, f, 8), \+ posicao(_, _, g, 8).

validar_roque_longo(COR) :-
    COR = 'w',
    % rei em e1 e torre em a1, b1, c1 e d1 estão vazios,
    posicao('K', COR, e, 1), posicao('R', COR, a, 1), \+ posicao(_, _, b, 1), \+ posicao(_, _, c, 1), \+ posicao(_, _, d, 1);
    
    COR = 'b',
    % rei em e8 e torre em a8, b8, c8 e d8 estão vazios 
    posicao('K', COR, e, 8), posicao('R', COR, a, 8), \+ posicao(_, _, b, 8), \+ posicao(_, _, c, 8), \+ posicao(_, _, d, 8).