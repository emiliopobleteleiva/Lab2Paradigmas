%archivo 'board.pl'

%modulo de las funciones:
:- module(board, [board/1,
          can_play/1,
          bottom_column/2,
          replace_with/4,
          play_piece/4,
          print_board/1,
          check_vertical_win/2,       % Verifica victorias verticales
          check_horizontal_win/2,     % Verifica victorias horizontales
          check_diagonal_win/2,
          consecutive_four/2
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

bottom_column([H|T], Yout):-
    bottom_column(T, H, 1, Yout).

bottom_column([_|_],[H],_,_):-
    write('Columna llena'), nl.

bottom_column([empty|T], empty, Y, Yout):-
    NewY is Y + 1,
    bottom_column(T, empty, NewY, Yout).

bottom_column([H|_], empty, Y, Y).
bottom_column([], empty, Y , Y).



replace_with([_|T], 1, Elem, [Elem|T]).

replace_with([H|T], X, Elem, [H|ListOut]):-
    NewX is X - 1,
    replace_with(T, NewX, Elem, ListOut).
    

play_piece(Board, Column, Piece, NewBoard):-
    vertical_list(Board, Column, ListaColumna),
    bottom_column(ListaColumna, Y),
    elemento_x(Board, Y, Fila),
    replace_with(Fila, Column, Piece, NewFila),
    replace_with(Board, Y, NewFila, NewBoard).
    

% Imprime cada fila del tablero separada por un salto de línea

print_board([]).  % Caso base: no quedan filas
print_board([Row | Rest]) :-
    writeln(Row), % Imprime la fila actual
    print_board(Rest). % Llama recursivamente para las filas restantes

%funcion extraer elemento x
%caso base
elemento_x([H|_], 1, H).

elemento_x([_|T], Iteracion, Elemento):-
    Iteracion > 1,
    NuevaIt is Iteracion - 1,
    elemento_x(T, NuevaIt, Elemento).


% RF07 - Verificar victoria vertical

vertical_list([], _, []):- !.

vertical_list([Fila|T2], Columna, [Elemento|NewList]):-
    elemento_x(Fila, Columna, Elemento),
    vertical_list(T2, Columna, NewList).

check_vertical_win(Board, Winner) :-
    check_vertical(Board, 1, Winner).

check_vertical(_, 8, 0).
    
check_vertical(Board, Aux, Winner):-
    vertical_list(Board, Aux, ListaColumna),
    (consecutive_four(ListaColumna, Piece) -> Winner = Piece;
        NewAux is Aux + 1, check_vertical(Board, NewAux, Winner)).
    

% RF08 - Verificar victoria horizontal
check_horizontal_win([], 0).

check_horizontal_win([Fila|T], Winner):-
    (consecutive_four(Fila, Piece) -> Winner = Piece;
        check_horizontal_win(T, Winner)).
    

% RF09 - Verificar victoria diagonal

diagonal_list_desc(_, _, 7, []):- !.

diagonal_list_asc(_, 8, _, []):- !.

diagonal_list_desc(_, 8, _, []):- !.

diagonal_list_asc(_, _, 0, []):- !.

diagonal_list_asc(Board, X, Y, [Elemento|NwList]):-
    elemento_x(Board, Y, Fila),
    elemento_x(Fila, X, Elemento),
    NewX is X + 1, NewY is Y - 1,
    diagonal_list_asc(Board, NewX, NewY, NwList).
    
diagonal_list_desc(Board, X, Y, [Elemento|NwList]):-
    elemento_x(Board, Y, Fila),
    elemento_x(Fila, X, Elemento),
    NewX is X + 1, NewY is Y + 1,
    diagonal_list_desc(Board, NewX, NewY, NwList).

check_diagonal_asc(Board, Winner):-
    check_diagonal_asc(Board, 1, 4, Winner).

check_diagonal_asc(_, 5, 6, 0).

check_diagonal_asc(Board, X, Y, Winner):-
    diagonal_list_asc(Board, X, Y, NewList),
    (consecutive_four(NewList, Piece) -> Winner = Piece;
     (Y == 6 -> NewX is X + 1, check_diagonal_asc(Board, NewX, Y, Winner);
     X == 1 -> NewY is Y + 1, check_diagonal_asc(Board, X, NewY, Winner))
     ).

check_diagonal_desc(Board, Winner):-
    check_diagonal_desc(Board, 1, 3, Winner).

check_diagonal_desc(_, 5, 1, 0).

check_diagonal_desc(Board, X, Y, Winner):-
    diagonal_list_desc(Board, X, Y, NewList),
    (consecutive_four(NewList, Piece) -> Winner = Piece;
     (Y == 1 -> NewX is X + 1, check_diagonal_desc(Board, NewX, Y, Winner);
     X == 1 -> NewY is Y - 1, check_diagonal_desc(Board, X, NewY, Winner))
     ).

check_diagonal_win(Board, Winner) :-
    (check_diagonal_asc(Board, AscWinner), AscWinner \= 0 ->
        Winner = AscWinner;
    check_diagonal_desc(Board, DescWinner), DescWinner \= 0 ->
        Winner = DescWinner;
    Winner = 0).

% Auxiliar: Verifica si hay 4 consecutivos

find_player_color(Color, 1) :- player(1, _, Color, _, _, _, _, _).
find_player_color(Color, 2) :- player(2, _, Color, _, _, _, _, _).
    
%consecutive four
consecutive_four([Piece, Piece, Piece, Piece|_], Winner):-
    Piece \= empty,
    find_player_color(Piece, Winner).

consecutive_four([_ | T], Winner):-
    consecutive_four(T, Winner).
