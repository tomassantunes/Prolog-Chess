% -*- Prolog -*-

% syntax -> expr(X, "...", []).
expr(jogada(A, B))    --> branca(A), " ", preta(B).
expr(jogada(A, B))    --> branca(A), " ", resultado(B).
expr(jogada(A, B, C)) --> branca(A), " ", preta(B), " ", resultado(C).

branca(V)                        --> coluna(V), linha(V).
branca(A)                        --> castle(A). % Ex: O-O
branca(move('P', A, B))          --> coluna(A), linha(B). % Ex: e4
branca(move(A, B, C))            --> piece(A), coluna(B), linha(C). % Ex: Nf3
branca(move(A, B, C, D))         --> piece(A), coluna(B), coluna(C), linha(D). % Ex: Nbd2
branca(move(A, B, C, D))         --> piece(A), linha(B), coluna(C), linha(D). % Ex: N3d4
branca(move('P', A, B, C, D))    --> coluna(A), action(B), coluna(C), linha(D). % Ex: exf5
branca(move(A, B, C, D))         --> piece(A), action(B), coluna(C), linha(D). % Ex: Qxf6
branca(move(A, B, C, D, E))      --> piece(A), coluna(B), action(C), coluna(D), linha(E). % Ex: Nbxf6
branca(move(A, B, C, D, E))      --> piece(A), linha(B), action(C), coluna(D), linha(E). % Ex: N3xd4
branca(move('P', A, B, C))       --> coluna(A), linha(B), action(C). % e5+
branca(move('P', A, B, C, D, E)) --> coluna(A), action(B), coluna(C), linha(D), action(E). % exf5+
branca(move(A, B, C, D))         --> piece(A), coluna(B), linha(C), action(D). % Ra8+
branca(move(A, B, C, D, E))      --> piece(A), action(B), coluna(C), linha(D), action(E).  % Ex: Rxa8+
branca(move(A, B, C, D, E))      --> piece(A), coluna(B), coluna(C), linha(D), action(E).  % Ex: Nba8+
branca(move(A, B, C, D, E))      --> piece(A), linha(B), coluna(C), linha(D), action(E).  % Ex: R3a8+
branca(move(A, B, C, D, E, F))   --> piece(A), coluna(B), action(C), coluna(D), linha(E), action(F).  % Ex: Nbxa8+
branca(move(A, B, C, D, E, F))   --> piece(A), linha(B), action(C), coluna(D), linha(E), action(F).  % Ex: R3xa8+

preta(V)                         --> coluna(V), linha(V).
preta(A)                         --> castle(A). % Ex: O-O
preta(move('P', A, B))           --> coluna(A), linha(B). % Ex: e4
preta(move(A, B, C))             --> piece(A), coluna(B), linha(C). % Ex: Nf3
preta(move(A, B, C, D))          --> piece(A), coluna(B), coluna(C), linha(D). % Ex: Nbd2
preta(move(A, B, C, D))          --> piece(A), linha(B), coluna(C), linha(D). % Ex: N3d4
preta(move(A, B, C, D, E))       --> piece(A), coluna(B), action(C), coluna(D), linha(E). % Ex: Nbxf6
preta(move(A, B, C, D, E))       --> piece(A), linha(B), action(C), coluna(D), linha(E). % Ex: N3xd4
preta(move('P', A, B, C, D))     --> coluna(A), action(B), coluna(C), linha(D). % Ex: exf5
preta(move(A, B, C, D))          --> piece(A), action(B), coluna(C), linha(D). % Ex: Qxf6
preta(move('P', A, B, C))        --> coluna(A), linha(B), action(C). % e5+
preta(move('P', A, B, C, D, E))  --> coluna(A), action(B), coluna(C), linha(D), action(E). % exf5+
preta(move(A, B, C, D))          --> piece(A), coluna(B), linha(C), action(D). % Ra8+
preta(move(A, B, C, D, E))       --> piece(A), action(B), coluna(C), linha(D), action(E).  % Ex: Rxa8+
preta(move(A, B, C, D, E))       --> piece(A), coluna(B), coluna(C), linha(D), action(E).  % Ex: Nba8+
preta(move(A, B, C, D, E))       --> piece(A), linha(B), coluna(C), linha(D), action(E).  % Ex: R3a8+
preta(move(A, B, C, D, E, F))    --> piece(A), coluna(B), action(C), coluna(D), linha(E), action(F).  % Ex: Nbxa8+
preta(move(A, B, C, D, E, F))    --> piece(A), linha(B), action(C), coluna(D), linha(E), action(F).  % Ex: R3xa8+



piece('K') --> "K".
piece('Q') --> "Q".
piece('R') --> "R".
piece('B') --> "B".
piece('N') --> "N".

action(takes)     --> "x".
action(check)     --> "+".
action(checkmate) --> "#".

castle(kingside_castle)  --> "O-O".
castle(queenside_castle) --> "O-O-O".

resultado(white_wins) --> "1-0".
resultado(black_wins) --> "0-1".
resultado(draw)       --> "1/2-1/2".

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