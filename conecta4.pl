% Archivo: conecta4.pl

% Entrada de módulos
:- use_module(player).
:- use_module(piece).
:- use_module(board).
:- use_module(game).

% Predicado principal para ejecutar el juego
start_game :-
% 1. Crear jugadores (10 fichas cada uno para un juego corto)
player(1, "Juan", "red", 0, 0, 0, 10, P1),
player(2, "Mauricio", "yellow", 0, 0, 0, 10, P2),
% 2. Crear fichas
piece("red", RedPiece),
piece("yellow", YellowPiece),
% 3. Crear tablero inicial vacío
board(EmptyBoard),
print_board(EmptyBoard),

% 4. Crear nuevo juego
write('Paso 1'), nl,

game(P1, P2, EmptyBoard, 1, G0),

write('Paso 2'), nl,
% 5. Realizando movimientos para crear una victoria diagonal
player_play(G0, P1, 0, G1),    % Juan juega en columna 0

write('coso locura1'), nl,

game_get_board(G1, CurrentBoard),
print_board(CurrentBoard),

player_play(G1, P2, 1, G2),    % Mauricio juega en columna 1

write('coso locura2'), nl,
player_play(G2, P1, 1, G3),    % Juan juega en columna 1
player_play(G3, P2, 2, G4),    % Mauricio juega en columna 2
player_play(G4, P1, 2, G5),    % Juan juega en columna 2
player_play(G5, P2, 3, G6),    % Mauricio juega en columna 3
player_play(G6, P1, 2, G7),    % Juan juega en columna 2
player_play(G7, P2, 3, G8),    % Mauricio juega en columna 3
player_play(G8, P1, 3, G9),    % Juan juega en columna 3
player_play(G9, P2, 0, G10),   % Mauricio juega en columna 0
player_play(G10, P1, 3, G11),  % Juan juega en columna 3 (victoria diagonal)
% 6. Verificaciones del estado del juego
write('¿Se puede jugar en el tablero vacío? ').

