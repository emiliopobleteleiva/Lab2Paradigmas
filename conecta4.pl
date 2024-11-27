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
% 4. Crear nuevo juego
game(P1, P2, EmptyBoard, 1, G0),
write('Juego 0: '), write(G0), nl,
% 5. Realizando movimientos para crear una victoria diagonal
player_play(G0, P1, 0, G1),    % Juan juega en columna 0
write('jugada 1'), nl, write(G1), nl,
player_play(G1, P2, 1, G2),    % Mauricio juega en columna 1
write('jugada 2'), nl, write(G2), nl,
player_play(G2, P1, 1, G3),    % Juan juega en columna 1
write('jugada 3'), nl,
player_play(G3, P2, 2, G4),    % Mauricio juega en columna 2
write('jugada 4'), nl,
player_play(G4, P1, 2, G5),    % Juan juega en columna 2
write('jugada 5'), nl,
player_play(G5, P2, 3, G6),    % Mauricio juega en columna 3
write('jugada 6'), nl,
player_play(G6, P1, 2, G7),    % Juan juega en columna 2
write('jugada 7'), nl,
player_play(G7, P2, 3, G8),    % Mauricio juega en columna 3
player_play(G8, P1, 3, G9),    % Juan juega en columna 3
player_play(G9, P2, 0, G10),   % Mauricio juega en columna 0
player_play(G10, P1, 3, G11).  % Juan juega en columna 3 (victoria diagonal)


