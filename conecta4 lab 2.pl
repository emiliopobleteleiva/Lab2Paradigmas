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

% --- Tests ---

test_check_horizontal :-
    board(Board),                      % Crear un tablero vacío
    piece("red", RedPiece),            % Crear una ficha roja
    play_piece(Board, 0, RedPiece, NewBoard1), % Primer movimiento en columna 0
    play_piece(NewBoard1, 1, RedPiece, NewBoard2), % Segundo movimiento en columna 1
    play_piece(NewBoard2, 2, RedPiece, NewBoard3), % Tercer movimiento en columna 2
    play_piece(NewBoard3, 3, RedPiece, NewBoard4), % Cuarto movimiento en columna 3
    print_board(NewBoard4),            % Imprimir el tablero actualizado
    ( check_horizontal(NewBoard4, RedPiece, win) ->
        writeln('¡Victoria horizontal detectada!')
    ;   writeln('No hay victoria horizontal.')
    ).

