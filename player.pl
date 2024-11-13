% Define el módulo `player` y exporta solo `player/8` y `update_stats/3`
:- module(player, [player/8, update_stats/3]).

% Implementación del predicado `player/8`
player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, [ID, Name, Color, Wins, Losses, Draws, RemainingPieces]).

% Implementación de `update_stats/3` que actualiza el puntaje del jugador
update_stats([ID, Name, Color, Wins, Losses, Draws, RemainingPieces], NewWins, [ID, Name, Color, NewWins, Losses, Draws, RemainingPieces]).

% Este predicado auxiliar no está exportado y no será accesible desde otros archivos
auxiliar_privado(X) :- X > 0.

