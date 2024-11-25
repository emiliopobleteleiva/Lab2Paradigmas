%archivo 'board.pl'

%modulo de las funciones:
:- module(board, [board/1, can_play/1, play_piece/4, print_board/1]).

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
