%para RF01
% entender y explicar cada RF previo a programar, para asi establecer
% una l�gica

%TDAs planteados

% RF02
% TDA player
% desc: constructor, permite crear al jugador
% imp: usar estructuras basadas en listas
% dom:
% - id: int del numero identificador del jugador
% - name: string del nombre del jugador
% - color: string del color de la ficha del jugador
% - wins: int del numero de veces que ha ganado
% - losses: int del numero de veces que ha perdido
% - draws: int del numero de veces que ha empatado
% - remainingPieces: int de la cantidad de piezas que le quedan
% ej: player(1, "Juan", "red", 0, 0, 0, 21, Player).

% RF03
% TDA piece
% desc: constructor, crea una ficha y le asigna un color
% imp: usar estructuras basadas en listas
% dom:
% - color: string con el color escrito
% ej: piece("red", Piece).

% RF04
% TDA board
% desc: constructor, crea el arreglo del tablero
% imp: basado en listas, un tablero de 7x6 y debe empezar vac�o
% dom: no recibe
% ej: board(Board).

% RF05
% TDA board - predicado "can_play"
% desc: permite verificar si en el tablero a�n se puede realizar una jugada
% imp: verifica si hay al menos una posici�n libre en el tablero
% dom:
% - board: estado actual del tablero (TDA board)
% ej: can_play(EmptyBoard), can_play(CurrentBoard).

% RF06
% TDA board - predicado "play_piece"
% desc: jugar una ficha en el tablero en la columna indicada, ubic�ndola en la posici�n m�s baja disponible
% imp: usa el TDA board para colocar la ficha (piece) en la posici�n m�s baja de la columna seleccionada
% dom:
% - board: estado actual del tablero
% - column: �ndice de la columna donde se coloca la ficha
% - piece: la ficha a colocar (TDA piece)
% - newBoard: nuevo estado del tablero despu�s de colocar la ficha
% ej: play_piece(EmptyBoard, 3, RedPiece, NewBoard).

% RF07
% TDA board - predicado "check_vertical_win"
% desc: verificar si existe una victoria vertical (4 fichas consecutivas del mismo color en una columna)
% imp: resuelve de forma recursiva, explorando cada columna en el tablero
% dom:
% - board: estado actual del tablero
% - winner: 1 si gana el jugador 1, 2 si gana el jugador 2, 0 si no hay ganador vertical
% ej: check_vertical_win(CurrentBoard, Winner).

% RF08
% TDA board - predicado "check_horizontal_win"
% desc: verificar si existe una victoria horizontal (4 fichas consecutivas del mismo color en una fila)
% imp: resuelve de forma recursiva, explorando cada fila en el tablero
% dom:
% - board: estado actual del tablero
% - winner: 1 si gana el jugador 1, 2 si gana el jugador 2, 0 si no hay ganador horizontal
% ej: check_horizontal_win(CurrentBoard, Winner).

% RF09
% TDA board - predicado "check_diagonal_win"
% desc: verificar si existe una victoria diagonal (4 fichas consecutivas del mismo color en cualquier diagonal)
% imp: usa recursi�n para explorar las diagonales ascendentes y descendentes
% dom:
% - board: estado actual del tablero
% - winner: 1 si gana el jugador 1, 2 si gana el jugador 2, 0 si no hay ganador diagonal
% ej: check_diagonal_win(CurrentBoard, Winner).

% RF10
% TDA board - predicado "who_is_winner"
% desc: determinar si existe un ganador en el tablero, evaluando victoria vertical, horizontal o diagonal
% imp: usa los predicados de verificaci�n vertical, horizontal y diagonal para identificar un ganador
% dom:
% - board: estado actual del tablero
% - winner: 1 si gana el jugador 1, 2 si gana el jugador 2, 0 si no hay ganador
% ej: who_is_winner(Board, Winner).

% RF11
% TDA game - predicado "game"
% desc: constructor que crea una nueva partida de Conecta4
% imp: usa estructuras basadas en listas para almacenar los datos de los jugadores, el tablero, y el turno actual
% dom:
% - player1: representaci�n del primer jugador (TDA player)
% - player2: representaci�n del segundo jugador (TDA player)
% - board: estado inicial del tablero (vac�o)
% - currentTurn: turno actual (1 para el jugador 1, 2 para el jugador 2)
% ej: game(Player1, Player2, EmptyBoard, 1, Game).

% RF12
% TDA game - predicado "game_history"
% desc: genera un historial de los movimientos realizados en la partida
% imp: almacena los movimientos a medida que avanzan en el juego
% dom:
% - game: estado actual del juego (TDA game)
% - currentHistory: historial de movimientos actuales
% ej: game_history(Game, CurrentHistory).

% RF13
% TDA game - predicado "is_draw"
% desc: verifica si el juego actual ha terminado en empate
% imp: comprueba si el tablero est� lleno o si ambos jugadores no tienen m�s fichas
% dom:
% - game: estado actual del juego (TDA game)
% - resultado: #t si es empate, #f si no
% ej: is_draw(Game).

% RF14
% TDA player - predicado "update_stats"
% desc: actualiza las estad�sticas del jugador (victorias, derrotas o empates) despu�s de una partida
% imp: modifica el TDA player con nuevas estad�sticas
% dom:
% - game: estado actual del juego (TDA game)
% - oldStats: estad�sticas previas del jugador
% - newStats: nuevas estad�sticas del jugador
% ej: update_stats(Game, OldStats, NewStats).

% RF15
% TDA game - predicado "get_current_player"
% desc: obtiene el jugador cuyo turno est� en curso
% imp: accede al atributo de turno en el TDA game
% dom:
% - game: estado actual del juego (TDA game)
% - currentPlayer: jugador actual (TDA player)
% ej: get_current_player(Game, CurrentPlayer).

% RF16
% TDA game - predicado "game_get_board"
% desc: obtiene el estado actual del tablero en el juego
% imp: accede al tablero dentro del TDA game
% dom:
% - game: estado actual del juego (TDA game)
% - currentBoard: estado actual del tablero
% ej: game_get_board(Game, CurrentBoard).

% RF17
% TDA game - predicado "end_game"
% desc: finaliza el juego y actualiza las estad�sticas de los jugadores seg�n el resultado
% imp: usa el TDA game para actualizar las estad�sticas de los jugadores
% dom:
% - game: estado actual del juego (TDA game)
% - endGame: estado final del juego con estad�sticas actualizadas
% ej: end_game(Game, EndGame).

% RF18
% TDA game - predicado "player_play"
% desc: realiza un movimiento en el juego
% imp: verifica que sea el turno correcto, coloca la ficha en el tablero, cambia el turno, actualiza el historial, y revisa el estado del juego
% dom:
% - game: estado actual del juego (TDA game)
% - player: jugador que realiza el movimiento
% - column: columna donde se coloca la ficha
% - newGame: nuevo estado del juego despu�s del movimiento
% ej: player_play(Game, Player, Column, NewGame).

%%%%%%%

