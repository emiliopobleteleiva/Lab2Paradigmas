% Archivo: conecta4.pl

% RF01 - Explicación de los requerimientos funcionales
% Se deben comprender y documentar claramente antes de implementar.



%RF07
% check_vertical(Board, PlayerPiece, State)
% Board: Tablero (en formato de filas)
% PlayerPiece: Pieza del jugador que estamos verificando
% State: Resultado (win si se encuentran 4 consecutivas, fail en caso contrario)

check_vertical(Board, PlayerPiece, win) :-
    transpose(Board, TransposedBoard),    % Convertimos columnas en filas
    member(Column, TransposedBoard),     % Iteramos sobre cada columna
    consecutive_four(Column, PlayerPiece). % Verificamos si hay 4 consecutivas.

check_vertical(_, _, fail).               % Si no encontramos, devolvemos "fail".

% consecutive_four(List, PlayerPiece)
% Verifica si hay 4 elementos consecutivos iguales a PlayerPiece
consecutive_four(List, PlayerPiece) :-
    append(_, [PlayerPiece, PlayerPiece, PlayerPiece, PlayerPiece | _], List).

%RF08
% check_horizontal(Board, PlayerPiece, State)
% Board: Tablero (en formato de filas)
% PlayerPiece: Pieza del jugador que estamos verificando
% State: Resultado (win si se encuentran 4 consecutivas, fail en caso contrario)

check_horizontal(Board, PlayerPiece, win) :-
    member(Row, Board),              % Iteramos sobre cada fila
    consecutive_four(Row, PlayerPiece). % Verificamos si hay 4 consecutivas.

check_horizontal(_, _, fail).         % Si no encontramos, devolvemos "fail".

% RF09 - check_diagonal(Board, PlayerPiece, State)
% Verifica si hay una victoria en las diagonales (ascendentes o descendentes)
% Entrada:
%   Board: Tablero (en formato de filas)
%   PlayerPiece: Pieza del jugador
%   State: Resultado (win si se encuentran 4 consecutivas, fail en caso contrario)

% Caso diagonal ascendente
check_diagonal(Board, PlayerPiece, win) :-
    findall(Diagonal, diagonal_ascending(Board, Diagonal), Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, PlayerPiece).

% Caso diagonal descendente
check_diagonal(Board, PlayerPiece, win) :-
    findall(Diagonal, diagonal_descending(Board, Diagonal), Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, PlayerPiece).

% Si no se encuentra victoria
check_diagonal(_, _, fail).

% Generar diagonales ascendentes
diagonal_ascending(Board, Diagonal) :-
    append(_, [Row1, Row2, Row3, Row4 | _], Board),
    nth1(Col, Row1, P1),
    ColNext1 is Col + 1, nth1(ColNext1, Row2, P2),
    ColNext2 is Col + 2, nth1(ColNext2, Row3, P3),
    ColNext3 is Col + 3, nth1(ColNext3, Row4, P4),
    Diagonal = [P1, P2, P3, P4].

% Generar diagonales descendentes
diagonal_descending(Board, Diagonal) :-
    append(_, [Row1, Row2, Row3, Row4 | _], Board),
    nth1(Col, Row1, P1),
    ColNext1 is Col - 1, nth1(ColNext1, Row2, P2),
    ColNext2 is Col - 2, nth1(ColNext2, Row3, P3),
    ColNext3 is Col - 3, nth1(ColNext3, Row4, P4),
    Diagonal = [P1, P2, P3, P4].




% --- Tests ---

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
% 5. Realizando movimientos para crear una victoria diagonal
player_play(G0, P1, 0, G1),    % Juan juega en columna 0
player_play(G1, P2, 1, G2),    % Mauricio juega en columna 1
player_play(G2, P1, 1, G3),    % Juan juega en columna 1
player_play(G3, P2, 2, G4),    % Mauricio juega en columna 2
player_play(G4, P1, 2, G5),    % Juan juega en columna 2
player_play(G5, P2, 3, G6),    % Mauricio juega en columna 3
player_play(G6, P1, 2, G7),    % Juan juega en columna 2
player_play(G7, P2, 3, G8),    % Mauricio juega en columna 3
player_play(G8, P1, 3, G9),    % Juan juega en columna 3
player_play(G9, P2, 0, G10),   % Mauricio juega en columna 0
player_play(G10, P1, 3, G11),  % Juan juega en columna 3
