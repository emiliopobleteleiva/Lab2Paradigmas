% Define el m�dulo `player` y exporta solo `player/8` y `update_stats/3`
:- module(player, [player/8, update_stats/3]).

% Implementaci�n del predicado `player/8`
player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, [ID, Name, Color, Wins, Losses, Draws, RemainingPieces]).

% Implementaci�n de `update_stats/3` que actualiza el puntaje del jugador
update_stats([ID, Name, Color, Wins, Losses, Draws, RemainingPieces], NewWins, [ID, Name, Color, NewWins, Losses, Draws, RemainingPieces]).

% Este predicado auxiliar no est� exportado y no ser� accesible desde otros archivos
auxiliar_privado(X) :- X > 0.

