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

% --- Tests ---
% Crear jugadores
test_players :-
    player(1, "Juan", "red", 0, 0, 0, 21, Player1),
    player(2, "Mauricio", "yellow", 0, 0, 0, 21, Player2),
    writeln(Player1),
    writeln(Player2).

% Crear fichas
test_pieces :-
    piece("red", RedPiece),
    piece("yellow", YellowPiece),
    writeln(RedPiece),
    writeln(YellowPiece).

% Crear tablero vacío
test_board :-
    board(EmptyBoard),
    print_board(EmptyBoard). % Usamos el nuevo predicado

% Verificar si se puede jugar
test_can_play :-
    board(Board),
    can_play(Board),
    writeln('Se puede jugar en el tablero vacío.').

% Colocar ficha
test_play_piece :-
    board(Board),
    piece("red", RedPiece),
    play_piece(Board, 3, RedPiece, NewBoard),
    play_piece(Board, 2, RedPiece, NewBoard),
    play_piece(Board, 2, RedPiece, NewBoard),
    print_board(NewBoard). % Usamos el nuevo predicado

% Ejecutar todos los tests
run_tests :-
    test_players,
    test_pieces,
    test_board,
    test_can_play,
    test_play_piece.
