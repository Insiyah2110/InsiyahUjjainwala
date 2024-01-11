import java.util.Scanner;

class HumanPlayer extends Player {
    public HumanPlayer(char symbol, Board board, String name) {
        super(symbol, board, name);
    }

    public void makeMove(Board board){
        System.out.println(name + " please input your move: ");
        Scanner input = new Scanner(System.in);
        int move = input.nextInt();
                
        if (!board.isColFull(move)){
            board.humanMove(this.symbol, move);
        }
        else{
            while (board.isColFull(move)){
                System.out.println("Column " + move + " is full. Please enter a valid input.");
                System.out.println(name + " please input your move: ");
                move = input.nextInt();  
            }
            board.humanMove(this.symbol,move);
        }

    }



    
}