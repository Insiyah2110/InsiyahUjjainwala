import java.util.Random;
import java.util.*;

class AIPlayer extends Player{
    
    Random random = new Random();
    char opponent_symbol = board.getOppSymbol();
    int move;

    public AIPlayer(char symbol, Board board, String name) {
        super(symbol, board, name);
    }

    public void makeMove(Board board){
        if (!board.IsWinningMove(symbol)){ 
            if (!board.IsOppWinningMove(opponent_symbol)){
                int move = random.nextInt(7); //int between 0 and 6
                board.AIMove(symbol, move);
            }
        }


    }

}