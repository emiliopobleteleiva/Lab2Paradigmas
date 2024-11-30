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

player_play(G0, P1, 0, G1),
player_play(G1, P2, 2, G2),
player_play(G2, P1, 5, G3),
player_play(G3, P2, 0, G4),
player_play(G4, P1, 4, G5),
player_play(G5, P2, 6, G6),
player_play(G6, P1, 1, G7),
player_play(G7, P2, 1, G8),
player_play(G8, P1, 1, G9),
player_play(G9, P2, 1, G10),
player_play(G10, P1, 1, G11),
player_play(G11, P2, 1, G12),

%posicionar una pieza fuera del rango
write('Reaccion esperada, devolver "Columna llena"'), nl,
player_play(G12, P1, 1, G13),

game_get_board(G13, NewBoard),
print_board(NewBoard), nl,

write('Datos juego: (G12 porque G13 no se pudo completar)'), nl,
write(G12),

write('Continuar juego hasta que se acaben las fichas'), nl,
player_play(G13, P1, 4, G14),
player_play(G14, P2, 3, G15),
player_play(G15, P1, 6, G16),
player_play(G16, P2, 6, G17),
player_play(G17, P1, 6, G18),
player_play(G18, P2, 6, G19),
player_play(G19, P1, 3, G20),

write('Ambos jugadores se quedaron sin fichas'), nl,
write(G20), nl,
game_get_board(G20, B20),
print_board(B20), nl,

who_is_winner(B20, Winner),
write('Hay ganadores? (0 = No, 1 = Player1, 2 = Player 2'), nl,
write(Winner), nl,

write('Es empate?'), nl,
is_draw(G20),
end_game(G20, EndedGame),

write('Juego terminado: '), nl,
write(EndedGame).
