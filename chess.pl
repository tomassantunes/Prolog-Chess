% -*- Prolog -*-

% syntax -> lj(X, "...", []).
lj(jogada(A, B))    --> branca(A), " ", preta(B).
lj(jogada(A, B))    --> branca(A), " ", resultado(B).
lj(jogada(A, B, C)) --> branca(A), " ", preta(B), " ", resultado(C).

% branca(V)                        --> coluna(V), linha(V).
branca(A)                        --> castle(A). % Ex: O-O
branca(movimento('P', A, B))          --> coluna(A), linha(B). % Ex: e4
branca(movimento(A, B, C))            --> peca(A), coluna(B), linha(C). % Ex: Nf3
branca(movimento(A, B, C, D))         --> peca(A), coluna(B), coluna(C), linha(D). % Ex: Nbd2
branca(movimento(A, B, C, D))         --> peca(A), linha(B), coluna(C), linha(D). % Ex: N3d4
branca(movimento('P', A, B, C, D))    --> coluna(A), acao(B), coluna(C), linha(D). % Ex: exf5
branca(movimento(A, B, C, D))         --> peca(A), acao(B), coluna(C), linha(D). % Ex: Qxf6
branca(movimento(A, B, C, D, E))      --> peca(A), coluna(B), acao(C), coluna(D), linha(E). % Ex: Nbxf6
branca(movimento(A, B, C, D, E))      --> peca(A), linha(B), acao(C), coluna(D), linha(E). % Ex: N3xd4
branca(movimento('P', A, B, C))       --> coluna(A), linha(B), acao(C). % e5+
branca(movimento('P', A, B, C, D, E)) --> coluna(A), acao(B), coluna(C), linha(D), acao(E). % exf5+
branca(movimento(A, B, C, D))         --> peca(A), coluna(B), linha(C), acao(D). % Ra8+
branca(movimento(A, B, C, D, E))      --> peca(A), acao(B), coluna(C), linha(D), acao(E).  % Ex: Rxa8+
branca(movimento(A, B, C, D, E))      --> peca(A), coluna(B), coluna(C), linha(D), acao(E).  % Ex: Nba8+
branca(movimento(A, B, C, D, E))      --> peca(A), linha(B), coluna(C), linha(D), acao(E).  % Ex: R3a8+
branca(movimento(A, B, C, D, E, F))   --> peca(A), coluna(B), acao(C), coluna(D), linha(E), acao(F).  % Ex: Nbxa8+
branca(movimento(A, B, C, D, E, F))   --> peca(A), linha(B), acao(C), coluna(D), linha(E), acao(F).  % Ex: R3xa8+

% preta(V)                         --> coluna(V), linha(V).
preta(A)                         --> castle(A). % Ex: O-O
preta(movimento('P', A, B))           --> coluna(A), linha(B). % Ex: e4
preta(movimento(A, B, C))             --> peca(A), coluna(B), linha(C). % Ex: Nf3
preta(movimento(A, B, C, D))          --> peca(A), coluna(B), coluna(C), linha(D). % Ex: Nbd2
preta(movimento(A, B, C, D))          --> peca(A), linha(B), coluna(C), linha(D). % Ex: N3d4
preta(movimento(A, B, C, D, E))       --> peca(A), coluna(B), acao(C), coluna(D), linha(E). % Ex: Nbxf6
preta(movimento(A, B, C, D, E))       --> peca(A), linha(B), acao(C), coluna(D), linha(E). % Ex: N3xd4
preta(movimento('P', A, B, C, D))     --> coluna(A), acao(B), coluna(C), linha(D). % Ex: exf5
preta(movimento(A, B, C, D))          --> peca(A), acao(B), coluna(C), linha(D). % Ex: Qxf6
preta(movimento('P', A, B, C))        --> coluna(A), linha(B), acao(C). % e5+
preta(movimento('P', A, B, C, D, E))  --> coluna(A), acao(B), coluna(C), linha(D), acao(E). % exf5+
preta(movimento(A, B, C, D))          --> peca(A), coluna(B), linha(C), acao(D). % Ra8+
preta(movimento(A, B, C, D, E))       --> peca(A), acao(B), coluna(C), linha(D), acao(E).  % Ex: Rxa8+
preta(movimento(A, B, C, D, E))       --> peca(A), coluna(B), coluna(C), linha(D), acao(E).  % Ex: Nba8+
preta(movimento(A, B, C, D, E))       --> peca(A), linha(B), coluna(C), linha(D), acao(E).  % Ex: R3a8+
preta(movimento(A, B, C, D, E, F))    --> peca(A), coluna(B), acao(C), coluna(D), linha(E), acao(F).  % Ex: Nbxa8+
preta(movimento(A, B, C, D, E, F))    --> peca(A), linha(B), acao(C), coluna(D), linha(E), acao(F).  % Ex: R3xa8+

peca('K') --> "K".
peca('Q') --> "Q".
peca('R') --> "R".
peca('B') --> "B".
peca('N') --> "N".

acao(captura)     --> "x".
acao(xeque)     --> "+".
acao(xeque-mate) --> "#".

castle(roque_curto)  --> "O-O".
castle(roque_longo) --> "O-O-O".

resultado('BRANCAS GANHAM') --> "1-0".
resultado('PRETAS GANHAM') --> "0-1".
resultado('EMPATE')       --> "1/2-1/2".

coluna(a) --> "a".
coluna(b) --> "b".
coluna(c) --> "c".
coluna(d) --> "d".
coluna(e) --> "e".
coluna(f) --> "f".
coluna(g) --> "g".
coluna(h) --> "h".

linha(1) --> "1".
linha(2) --> "2".
linha(3) --> "3".
linha(4) --> "4".
linha(5) --> "5".
linha(6) --> "6".
linha(7) --> "7".
linha(8) --> "8".