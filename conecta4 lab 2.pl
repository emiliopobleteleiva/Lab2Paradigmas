% Archivo: conecta4.pl

% RF01 - Explicación de los requerimientos funcionales
% Se deben comprender y documentar claramente antes de implementar.

% RF02 - TDA Player
% Constructor para crear un jugador con sus atributos
% player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, Player)
player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, [ID, Name, Color, Wins, Losses, Draws, RemainingPieces]).

% RF03 - TDA Piece
% Constructor para crear una ficha con un color específico
% piece(Color, Piece)
piece(Color, [Color]).

% RF04 - TDA Board
% Constructor para crear un tablero vacío de 7 columnas x 6 filas

% board(Board)
board(Board) :-
    length(Row, 7),    % Cada fila tiene 7 columnas
    maplist(=(empty), Row), % Inicializa todas las posiciones como vacías
    length(Board, 6),  % El tablero tiene 6 filas
    maplist(=(Row), Board). % Repite la misma fila vacía 6 veces

% Imprime cada fila del tablero separada por un salto de línea

print_board([]).  % Caso base: no quedan filas
print_board([Row | Rest]) :-
    writeln(Row), % Imprime la fila actual
    print_board(Rest). % Llama recursivamente para las filas restantes

% RF05 - Predicado "can_play"
% Verifica si es posible realizar una jugada en el tablero (si hay espacios vacíos)
can_play(Board) :-
    member(Row, Board),
    member(empty, Row).

% RF06 - Predicado "play_piece"
% Coloca una ficha en la posición más baja disponible de una columna en el tablero
% Caso base: Si la posición actual está vacía y es la última fila (más baja disponible), coloca la ficha.

insert_piece_in_column([empty | Rest], Piece, [Piece | Rest]) :-
    \+ member(empty, Rest).  % Asegúrate de que no haya más posiciones vacías debajo.

% Caso recursivo: Si la posición actual está ocupada o no es la última vacía, continúa con las filas superiores.
insert_piece_in_column([Occupied | Rest], Piece, [Occupied | NewRest]) :-
    insert_piece_in_column(Rest, Piece, NewRest).

play_piece_in_column([Col | OtherCols], 0, Piece, [NewCol | OtherCols]) :-
    insert_piece_in_column(Col, Piece, NewCol).
play_piece_in_column([Col | OtherCols], ColIndex, Piece, [Col | NewCols]) :-
    ColIndex > 0,
    NextColIndex is ColIndex - 1,
    play_piece_in_column(OtherCols, NextColIndex, Piece, NewCols).

play_piece(Board, ColIndex, Piece, NewBoard) :-
    transpose(Board, TransposedBoard),                 % Trabajar por columnas
    play_piece_in_column(TransposedBoard, ColIndex, Piece, NewTransposedBoard),
    transpose(NewTransposedBoard, NewBoard).           % Restaurar filas originales

% Transponer una matriz
transpose([], []).
transpose([F | Fs], Ts) :-
    transpose_aux(F, [F | Fs], Ts).

transpose_aux([], _, []).
transpose_aux([_ | Rs], Rows, [Col | Cs]) :-
    maplist(nth1(1), Rows, Col),
    maplist(tail, Rows, RestRows),
    transpose_aux(Rs, RestRows, Cs).

tail([_|T], T).

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

% RF10 - who_is_winner/2
% Verifica si hay un ganador en el tablero actual.
% Entrada: Board (tablero actual)
% Salida: Winner (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador)

who_is_winner(Board, Winner) :-
    % Verificar si el jugador 1 tiene victoria
    (   piece("red", Player1Piece),
        (   check_vertical(Board, Player1Piece, win)
        ;   check_horizontal(Board, Player1Piece, win)
        ;   check_diagonal(Board, Player1Piece, win)
        ) -> Winner = 1
    ;   % Verificar si el jugador 2 tiene victoria
        piece("yellow", Player2Piece),
        (   check_vertical(Board, Player2Piece, win)
        ;   check_horizontal(Board, Player2Piece, win)
        ;   check_diagonal(Board, Player2Piece, win)
        ) -> Winner = 2
    ;   % Si no hay victorias
        Winner = 0
    ).

% RF11 - Crear una nueva partida
% game(Player1, Player2, Board, CurrentTurn, Game)
% Player1: Información del jugador 1
% Player2: Información del jugador 2
% Board: Tablero vacío inicial
% CurrentTurn: Turno inicial (1 para jugador 1, 2 para jugador 2)
% Game: Estructura resultante que contiene el estado inicial del juego

game(Player1, Player2, Board, CurrentTurn, [Player1, Player2, Board, CurrentTurn]).

% RF12 - Generar historial de movimientos de una partida
% game_history(Game, History)
% Entrada:
%   Game: Estado del juego que contiene el historial
% Salida:
%   History: Lista de movimientos realizados

game_history([_, _, _, _, History], History).


% --- Tests ---

