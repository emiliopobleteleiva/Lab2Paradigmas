% Archivo: conecta4.pl

% Entrada de módulos
:- use_module(player).
:- use_module(piece).
:- use_module(board).
:- use_module(game).

start_game:-
player(1, "Emilio", "Rojo", 0, 0, 0, 10, P1),
player(2, "Mac", "Negro", 0, 0, 0, 10, P2),
piece("Rojo", RedPiece),
piece("Negro", BlackPiece),

board(EmptyBoard),
game(P1, P2, EmptyBoard, 1, G0),

write(P1), nl, write(P2), nl,

player_play(G0, P1, 0, G1),
player_play(G1, P2, 1, G2),
player_play(G2, P1, 1, G3),
player_play(G3, P2, 3, G4),
player_play(G4, P1, 3, G5),
player_play(G5, P2, 2, G6),
player_play(G6, P1, 4, G7),
player_play(G7, P2, 2, G8),
player_play(G8, P1, 5, G9),
player_play(G9, P2, 6, G10),
player_play(G10, P1, 2, G11),
player_play(G11, P2, 5, G12),
player_play(G12, P1, 5, G13),
player_play(G13, P2, 4, G14),
player_play(G14, P1, 2, G15),
player_play(G15, P2, 4, G16),
player_play(G16, P1, 4, G17),
player_play(G17, P2, 3, G18),
player_play(G18, P1, 2, G19),
player_play(G19, P2, 3, G20),
game_get_board(G20, B20),
who_is_winner(B20, Winner),
write('el ganador es el jugador'),write(Winner), write('!'), nl,
print_board(B20).
