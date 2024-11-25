% RF02 - TDA Player - archivo 'player.pl'
% Constructor para crear un jugador con sus atributos
% player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, Player)

%modulo
:- module(player, [player/8]).

%constructor
player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, [ID, Name, Color, Wins, Losses, Draws, RemainingPieces]).
