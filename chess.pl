% -*- Prolog -*-

% syntax -> expr(X, "...", []).
expr(jogada(A, B))    --> branca(A), " ", preta(B).
expr(jogada(A, B))    --> branca(A), " ", resultado(B).
expr(jogada(A, B, C)) --> branca(A), " ", preta(B), " ", resultado(C).

branca(V)                   --> coluna(V), linha(V).
branca(A)                   --> castle(A). % Ex: O-O
branca(move(A, B))          --> coluna(A), linha(B). % Ex: e4
branca(move(A, B, C))       --> piece(A), coluna(B), linha(C). % Ex: Nf3
branca(move(A, B, C, D))    --> piece(A), coluna(B), coluna(C), linha(D). % Ex: Nbd2
branca(move(A, B, C, D))    --> coluna(A), action(B), coluna(C), linha(D). % Ex: exf5
branca(move(A, B, C, D))    --> piece(A), action(B), coluna(C), linha(D). % Ex: Qxf6
branca(move(A, B, C))       --> coluna(A), linha(B), action(C). % e5+
branca(move(A, B, C, D, F)) --> coluna(A), action(B), coluna(C), linha(D), action(F). % exf5+
branca(move(A, B, C, D))    --> piece(A), coluna(B), linha(C), action(D). % Ra8+
branca(move(A, B, C, D, F)) --> piece(A), action(B), coluna(C), linha(D), action(F).  % Ex: Rxa8+

preta(V)                    --> coluna(V), linha(V).
preta(A)                    --> castle(A). % Ex: O-O
preta(move(A, B))           --> coluna(A), linha(B). % Ex: e4
preta(move(A, B, C))        --> piece(A), coluna(B), linha(C). % Ex: Nf3
preta(move(A, B, C, D))     --> piece(A), coluna(B), coluna(C), linha(D). % Ex: Nbd2
preta(move(A, B, C, D))     --> coluna(A), action(B), coluna(C), linha(D). % Ex: exf5
preta(move(A, B, C, D))     --> piece(A), action(B), coluna(C), linha(D). % Ex: Qxf6
preta(move(A, B, C))        --> coluna(A), linha(B), action(C). % e5+
preta(move(A, B, C, D, F))  --> coluna(A), action(B), coluna(C), linha(D), action(F). % exf5+
preta(move(A, B, C, D))     --> piece(A), coluna(B), linha(C), action(D). % Ra8+
preta(move(A, B, C, D, F))  --> piece(A), action(B), coluna(C), linha(D), action(F).  % Ex: Rxa8+


piece(king)   --> "K".
piece(queen)  --> "Q".
piece(rook)   --> "R".
piece(bishop) --> "B".
piece(knight) --> "N".

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