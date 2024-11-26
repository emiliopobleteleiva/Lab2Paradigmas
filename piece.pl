% archivo 'piece.pl'

% RF03 - TDA Piece
% Constructor para crear una ficha con un color específico
% piece(Color, Piece)

%modulo
:- module(piece, [piece/2]).

%constructor
piece(Color, [Color]).
