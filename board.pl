%archivo 'board.pl'

%modulo de las funciones:
:- module(board, [board/1,
          can_play/1,
          play_piece/4,
          print_board/1,
          check_vertical/3,       % Verifica victorias verticales
          check_horizontal/3,     % Verifica victorias horizontales
          check_diagonal/3
          ]).

%funciones

% RF04 - TDA Board
% Constructor para crear un tablero vacío de 7 columnas x 6 filas

% board(Board)
board(Board) :-
    length(Row, 7),    % Cada fila tiene 7 columnas
    maplist(=(empty), Row), % Inicializa todas las posiciones como vacías
    length(Board, 6),  % El tablero tiene 6 filas
    maplist(=(Row), Board). % Repite la misma fila vacía 6 veces


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

% Imprime cada fila del tablero separada por un salto de línea

print_board([]).  % Caso base: no quedan filas
print_board([Row | Rest]) :-
    writeln(Row), % Imprime la fila actual
    print_board(Rest). % Llama recursivamente para las filas restantes

% RF07 - Verificar victoria vertical
check_vertical(Board, PlayerPiece, win) :-
    transpose(Board, TransposedBoard),    % Convertimos columnas en filas
    member(Column, TransposedBoard),     % Iteramos sobre cada columna
    consecutive_four(Column, PlayerPiece). % Verificamos si hay 4 consecutivas.

check_vertical(_, _, fail).               % Si no encontramos, devolvemos "fail".

% RF08 - Verificar victoria horizontal
check_horizontal(Board, PlayerPiece, win) :-
    member(Row, Board),              % Iteramos sobre cada fila
    consecutive_four(Row, PlayerPiece). % Verificamos si hay 4 consecutivas.

check_horizontal(_, _, fail).         % Si no encontramos, devolvemos "fail".

% RF09 - Verificar victoria diagonal
check_diagonal(Board, PlayerPiece, win) :-
    % Caso diagonal ascendente
    findall(Diagonal, diagonal_ascending(Board, Diagonal), Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, PlayerPiece).

check_diagonal(Board, PlayerPiece, win) :-
    % Caso diagonal descendente
    findall(Diagonal, diagonal_descending(Board, Diagonal), Diagonals),
    member(Diagonal, Diagonals),
    consecutive_four(Diagonal, PlayerPiece).

check_diagonal(_, _, fail).

% Auxiliar: Verifica si hay 4 consecutivos
consecutive_four(List, PlayerPiece) :-
    append(_, [PlayerPiece, PlayerPiece, PlayerPiece, PlayerPiece | _], List).

% Auxiliar: Generar diagonales ascendentes
diagonal_ascending(Board, Diagonal) :-
    append(_, [Row1, Row2, Row3, Row4 | _], Board),
    nth1(Col, Row1, P1),
    ColNext1 is Col + 1, nth1(ColNext1, Row2, P2),
    ColNext2 is Col + 2, nth1(ColNext2, Row3, P3),
    ColNext3 is Col + 3, nth1(ColNext3, Row4, P4),
    Diagonal = [P1, P2, P3, P4].

% Auxiliar: Generar diagonales descendentes
diagonal_descending(Board, Diagonal) :-
    append(_, [Row1, Row2, Row3, Row4 | _], Board),
    nth1(Col, Row1, P1),
    ColNext1 is Col - 1, nth1(ColNext1, Row2, P2),
    ColNext2 is Col - 2, nth1(ColNext2, Row3, P3),
    ColNext3 is Col - 3, nth1(ColNext3, Row4, P4),
    Diagonal = [P1, P2, P3, P4].
