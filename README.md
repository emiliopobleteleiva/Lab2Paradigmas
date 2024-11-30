# Lab2Paradigmas

Conecta4 Paradigma Lógico, desarrollado por Emilio Poblete Leiva

Descripción:

Implementación de juego Conecta4 utilizando el paradigma lógico en Prolog. Este incluye diseño modular de TDAs para representar jugadores, piezas, tableros y el juego mismo. A través de un enfoque declarativo, se gestionan reglas, jugadas y conciciones de victoria del juego.

player.pl: Define el TDA Player, que almacena información sobre los jugadores, como identificador, nombre, color y estadísticas.

piece.pl: Implementa el TDA Piece, que representa las fichas utilizadas en el juego.
    
board.pl: Contiene las funciones relacionadas con el tablero, incluyendo la creación, jugadas, y verificaciones de victorias.
        
game.pl: Gestiona el estado general del juego, incluyendo el manejo de turnos y actualización del historial.


scripts de prueba: 

script_base_204446830_Emilio_PobleteLeiva.pl: Script base con pruebas y casos predeterminados.

script1_204446830_Emilio_PobleteLeiva.pl: Script adicional con pruebas funcionales específicas.

script2_204446830_Emilio_PobleteLeiva.pl: Script adicional con escenarios avanzados.


Funciones:

    board(Board)
    Descripción: Constructor para crear un tablero vacío de 6 filas y 7 columnas con todas las posiciones inicializadas como empty.
    Uso: board(Board) genera un tablero vacío para iniciar el juego.
    
    can_play(Board)
    Descripción: Verifica si hay espacios disponibles en el tablero para realizar una jugada.
    Uso: can_play(Board) retorna verdadero si se puede jugar.
    
    play_piece(Board, Column, Piece, NewBoard)
    Descripción: Inserta una pieza en la posición más baja disponible de la columna especificada.
    Uso: play_piece(Board, Column, Piece, NewBoard) actualiza el tablero con la nueva jugada.
    
    bottom_column(ColumnList, Index)
    Descripción: Determina la posición más baja disponible (de abajo hacia arriba) en una columna.
    Uso: bottom_column(ColumnList, Index) retorna el índice disponible en la columna.
    
    replace_with(List, Index, Element, NewList)
    Descripción: Reemplaza un elemento en una lista en el índice dado con un nuevo valor.
    Uso: replace_with(List, Index, Element, NewList) actualiza la lista.
    
    check_vertical_win(Board, Winner)
    Descripción: Verifica si hay 4 fichas consecutivas del mismo color en cualquier columna del tablero.
    Uso: check_vertical_win(Board, Winner) retorna el ganador o 0 si no hay ganador.
    
    check_horizontal_win(Board, Winner)
    Descripción: Verifica si hay 4 fichas consecutivas del mismo color en cualquier fila del tablero.
    Uso: check_horizontal_win(Board, Winner) retorna el ganador o 0 si no hay ganador.
    
    check_diagonal_win(Board, Winner)
    Descripción: Comprueba si hay 4 fichas consecutivas en diagonales (ascendentes o descendentes).
    Uso: check_diagonal_win(Board, Winner) retorna el ganador o 0.
    
    piece(Color, Piece)
    •    Descripción: Constructor para crear una ficha con un color específico.
    •    Uso: piece("red", RedPiece) genera una ficha roja
    
    player(ID, Name, Color, Wins, Losses, Draws, RemainingPieces, Player)
    •    Descripción: Constructor para crear un jugador con estadísticas iniciales.
    •    Uso: player(1, "Juan", "red", 0, 0, 0, 21, Player) crea un jugador con 21 fichas rojas.
    
    update_stats(Player, Wins, Losses, Draws, UpdatedPlayer)
    •    Descripción: Actualiza las estadísticas del jugador al final de un juego.
    •    Uso: update_stats(Player, Wins, Losses, Draws, UpdatedPlayer) devuelve al jugador con estadísticas actualizadas.
    
    game(Player1, Player2, Board, Turn, Game)
    •    Descripción: Constructor para crear un nuevo estado del juego con dos jugadores, un tablero, y el turno actual.
    •    Uso: game(P1, P2, Board, 1, Game) inicializa un juego con el jugador 1 en turno.
    
    player_play(Game, Player, Column, NewGame)
    •    Descripción: Realiza una jugada actualizando el estado del juego.
    •    Uso: player_play(Game, Player, Column, NewGame) coloca la ficha y actualiza el turno.
    
    game_get_board(Game, Board)
    •    Descripción: Obtiene el tablero actual del estado del juego.
    •    Uso: game_get_board(Game, Board) retorna el tablero del juego.
    
    get_current_player(Game, CurrentPlayer)
    •    Descripción: Obtiene el jugador que tiene el turno actual.
    •    Uso: get_current_player(Game, CurrentPlayer) retorna el jugador en turno.
    
    is_draw(Game)
    •    Descripción: Comprueba si el juego ha terminado en empate.
    •    Uso: is_draw(Game) retorna verdadero si no se pueden realizar más jugadas.
    
    game_history(Game, History)
    •    Descripción: Recupera el historial de jugadas realizadas en el juego.
    •    Uso: game_history(Game, History) retorna el historial.
    
    end_game(Game, EndedGame)
    •    Descripción: Finaliza el juego actualizando las estadísticas de los jugadores.
    •    Uso: end_game(Game, EndedGame) retorna el juego finalizado.
