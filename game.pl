% Archivo: game.pl

:- module(game, [
    who_is_winner/2,
    game/5,
    game_history/2,
    is_draw/1,
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
    (check_vertical_win(Board, WinnerVer), WinnerVer \= 0 -> Winner = WinnerVer;
        (check_horizontal_win(Board, WinnerHor), WinnerHor \= 0 -> Winner = WinnerHor;
            (check_diagonal_win(Board, WinnerDiag), WinnerDiag \= 0 -> Winner = WinnerDiag;
     Winner = 0))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF11 - Crear una nueva partida
% game(Player1, Player2, Board, CurrentTurn, Game)
% Player1: Información del jugador 1
% Player2: Información del jugador 2
% Board: Tablero vacío inicial
% CurrentTurn: Turno inicial (1 para jugador 1, 2 para jugador 2)
% Game: Estructura resultante que contiene el estado inicial del juego

game(Player1, Player2, Board, CurrentTurn, [Player1, Player2, Board, CurrentTurn, []]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF12 - Generar historial de movimientos de una partida
% game_history(Game, History)
% Entrada:
%   Game: Estado del juego que contiene el historial
% Salida:
%   History: Lista de movimientos realizados

game_history([_, _, _, _, History], History).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%RF13 - Comprobar si el juego termina en empate
% isdraw(Game)

is_draw([Player1, Player2, Board, _, _]) :-
        \+ can_play(Board);
        Player1 = [_, _, _, _, _, _, P1RemainingPieces],
        Player2 = [_, _, _, _, _, _, P2RemainingPieces],
        P1RemainingPieces =:= 0,
        P2RemainingPieces =:= 0.
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF14 - Actualizar las estadísticas del jugador
% Entrada:
%   Game: Estado del juego, contiene la información del ganador
%   Player: Jugador a actualizar
%   UpdatedPlayer: Estructura del jugador con estadísticas actualizadas

update_stats([Player1, Player2, Board, _, _], Player, [ID, Name, Color, NewWins, NewLosses, NewDraws, NewRemainingPieces]):-
    Player = [ID,Name,Color, _, _, _, _],
    Player1 = [P1ID, _, _, P1Wins, P1Losses, P1Draws, P1RemainingPieces],
    Player2 = [P2ID, _, _, P2Wins, P2Losses, P2Draws, P2RemainingPieces],
    
    who_is_winner(Board, Winner),

    (Winner =:= 1, ID = P1ID -> NewWins is P1Wins + 1, NewLosses is P1Losses, NewDraws is P1Draws, NewRemainingPieces is P1RemainingPieces;
     Winner =:= 2, ID = P2ID -> NewWins is P2Wins + 1, NewLosses is P2Losses, NewDraws is P2Draws, NewRemainingPieces is P2RemainingPieces;
     Winner =:= 1, ID = P2ID -> NewLosses is P1Losses + 1, NewWins is P1Wins, NewDraws is P1Draws, NewRemainingPieces is P1RemainingPieces;
     Winner =:= 2, ID = P1ID -> NewLosses is P2Losses + 1, NewWins is P2Wins, NewDraws is P2Draws, NewRemainingPieces is P2RemainingPieces;
     Winner =:= 0, ID = P1ID -> NewDraws is P1Draws + 1, NewWins is P1Wins, NewLosses is P1Losses, NewRemainingPieces is P1RemainingPieces;
     Winner =:= 0, ID = P2ID -> NewDraws is P2Draws + 1, NewWins is P2Wins, NewLosses is P2Losses, NewRemainingPieces is P2RemainingPieces
     ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF15 - Obtener el jugador cuyo turno está en curso
% Entrada:
%   Game: Estado del juego
% Salida:
%   CurrentPlayer: Jugador cuyo turno está en curso
    

get_current_player([Player1, _, _, 1, _], Player1). % Si el turno actual es 1, devuelve Player1
get_current_player([_, Player2, _, 2, _], Player2). % Si el turno actual es 2, devuelve Player2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF16 - Obtener el estado actual del tablero en el juego
% Entrada:
%   Game: Estructura del juego
% Salida:

%   Board: Tablero actual
game_get_board([_, _, Board, _, _], Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF17 - Finalizar el juego y actualizar estadísticas
% Entrada:
%   Game: Estado del juego actual
% Salida:
%   FinalGame: Estado del juego actualizado con estadísticas finales

end_game(Game, [P1Updated, P2Updated, Board, CurrentTurn, GameOut]) :-
    Game = [Player1, Player2, Board, CurrentTurn, GameOut],
    update_stats(Game, Player1, P1Updated),
    update_stats(Game, Player2, P2Updated).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% RF18 - Realizar movimiento
player_play([Player1, Player2, Board, CurrentTurn, History], Player, X, [UpdatedPlayer1, UpdatedPlayer2, NewBoard, NewTurn, NewHistory]) :-
    Column is X + 1,
    % Obtener la pieza del jugador
    Player = [ID, Name, Color, _, _, _, _],
    (CurrentTurn =:= 1 -> Player1 = [_,_,_, Wins, Losses, Draws, RemainingPieces];
     Player2 = [_,_,_, Wins, Losses, Draws, RemainingPieces]),

    RemainingPieces > 0, % Verificar que el jugador tenga fichas
    piece(Color, Piece),


    % Colocar la pieza en el tablero
    play_piece(Board, Column, Piece, NewBoard),


    % Reducir fichas del jugador
    UpdatedRemainingPieces is RemainingPieces - 1,

    % Actualizar los jugadores
    (CurrentTurn =:= 1 ->
        NewTurn is 2,
        UpdatedPlayer1 = [ID, Name, Color, Wins, Losses, Draws, UpdatedRemainingPieces],
        UpdatedPlayer2 = Player2
    ;
        NewTurn is 1,
        UpdatedPlayer2 = [ID, Name, Color, Wins, Losses, Draws, UpdatedRemainingPieces],
        UpdatedPlayer1 = Player1
    ),
    % Actualizar el historial
    append(History, [[ID, Column]], NewHistory).
