% Archivo: game.pl

:- module(game, [
    who_is_winner/2,
    game/5,
    game_history/2,
    is_draw/2,
    update_stats/3,
    get_current_player/2,
    game_get_board/2,
    end_game/2,
    player_play/4
]).

:- use_module(board).

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

game(Player1, Player2, Board, CurrentTurn, [Player1, Player2, Board, CurrentTurn, []]).
game(Player1, Player2, Board, CurrentTurn, [Player1, Player2, Board, CurrentTurn]).

% RF12 - Generar historial de movimientos de una partida
% game_history(Game, History)
% Entrada:
%   Game: Estado del juego que contiene el historial
% Salida:
%   History: Lista de movimientos realizados

game_history([_, _, _, _, History], History).

%RF13 - Comprobar si el juego termina en empate
% isdraw(Game)
is_draw([_, _, Board, _, _], true) :-
    \+ can_play(Board). % Tablero lleno
is_draw([Player1, Player2, _, _, _], true) :-
    player_remaining_pieces(Player1, 0),
    player_remaining_pieces(Player2, 0). % Ambos jugadores sin fichas
is_draw(_, false).

% RF14 - Actualizar las estadísticas del jugador
% Entrada:
%   Game: Estado del juego, contiene la información del ganador
%   Player: Jugador a actualizar
%   UpdatedPlayer: Estructura del jugador con estadísticas actualizadas

update_stats([Player1, Player2, _, _, _], Player, UpdatedPlayer) :-
    who_is_winner(_, Winner),
    (   % Si el jugador es el ganador
        Winner =:= 1, Player = Player1 -> increment_stat(Player, wins, UpdatedPlayer)
    ;   Winner =:= 2, Player = Player2 -> increment_stat(Player, wins, UpdatedPlayer)
    ;   % Si el jugador perdió
        Winner =:= 1, Player = Player2 -> increment_stat(Player, losses, UpdatedPlayer)
    ;   Winner =:= 2, Player = Player1 -> increment_stat(Player, losses, UpdatedPlayer)
    ;   % Si es empate
        Winner =:= 0 -> increment_stat(Player, draws, UpdatedPlayer)
    ).

% Incrementar estadísticas
% Entrada:
%   Player: Estructura del jugador
%   Stat: Estatística que se va a incrementar
%   UpdatedPlayer: Jugador con estadísticas actualizadas
increment_stat([ID, Name, Color, Wins, Losses, Draws, RemainingPieces], wins,
               [ID, Name, Color, NewWins, Losses, Draws, RemainingPieces]) :-
    NewWins is Wins + 1.
increment_stat([ID, Name, Color, Wins, Losses, Draws, RemainingPieces], losses,
               [ID, Name, Color, Wins, NewLosses, Draws, RemainingPieces]) :-
    NewLosses is Losses + 1.
increment_stat([ID, Name, Color, Wins, Losses, Draws, RemainingPieces], draws,
               [ID, Name, Color, Wins, Losses, NewDraws, RemainingPieces]) :-
    NewDraws is Draws + 1.

% RF15 - Obtener el jugador cuyo turno está en curso
% Entrada:
%   Game: Estado del juego
% Salida:
%   CurrentPlayer: Jugador cuyo turno está en curso
get_current_player([Player1, _, _, 1], Player1).  % Si el turno actual es 1, devuelve Player1
get_current_player([_, Player2, _, 2], Player2). % Si el turno actual es 2, devuelve Player2

% RF16 - Obtener el estado actual del tablero en el juego
% Entrada:
%   Game: Estructura del juego
% Salida:
%   Board: Tablero actual
game_get_board([_, _, Board, _], Board).

% RF17 - Finalizar el juego y actualizar estadísticas
% Entrada:
%   Game: Estado del juego actual
% Salida:
%   FinalGame: Estado del juego actualizado con estadísticas finales

end_game(Game, FinalGame) :-
    Game = [Player1, Player2, Board, _, _],
    who_is_winner(Board, Winner),
    (   Winner =:= 1 -> update_stats(Game, Player1, UpdatedPlayer1), FinalGame = [UpdatedPlayer1, Player2, Board, 0, []]
    ;   Winner =:= 2 -> update_stats(Game, Player2, UpdatedPlayer2), FinalGame = [Player1, UpdatedPlayer2, Board, 0, []]
    ;   is_draw(Game, Draw), Draw = true ->
        update_stats(Game, Player1, UpdatedPlayer1),
        update_stats(Game, Player2, UpdatedPlayer2),
        FinalGame = [UpdatedPlayer1, UpdatedPlayer2, Board, 0, []]
    ).

% RF18 - Realizar movimiento
player_play([Player1, Player2, Board, CurrentTurn, History], Player, Column, [UpdatedPlayer1, UpdatedPlayer2, NewBoard, NewTurn, NewHistory]) :-
    % Verificar que sea el turno del jugador correcto
    ((CurrentTurn =:= 1, Player = Player1);
     (CurrentTurn =:= 2, Player = Player2)),

    % Obtener la pieza del jugador
    Player = [_, _, Color, _, _, _, RemainingPieces],
    RemainingPieces > 0, % Verificar que el jugador tenga fichas
    piece(Color, Piece),

    % Colocar la pieza en el tablero
    play_piece(Board, Column, Piece, NewBoard),

    % Reducir fichas del jugador
    UpdatedRemainingPieces is RemainingPieces - 1,

    % Actualizar el historial
    append(History, [[Player, Column]], NewHistory),

    % Alternar el turno
    NewTurn is 3 - CurrentTurn,

    % Actualizar los jugadores
    (CurrentTurn =:= 1 ->
        Player1 = [_, _, Color1, _, _, _, _],
        UpdatedPlayer1 = [_, _, Color1, _, _, _, UpdatedRemainingPieces],
        UpdatedPlayer2 = Player2
    ;
        Player2 = [_, _, Color2, _, _, _, _],
        UpdatedPlayer2 = [_, _, Color2, _, _, _, UpdatedRemainingPieces],
        UpdatedPlayer1 = Player1
    ).
