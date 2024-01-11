import java.util.Scanner;

public class Board {

	private final int NUM_OF_COLUMNS = 7;
	private final int NUM_OF_ROW = 6;
	private char[][] array = new char[NUM_OF_ROW][NUM_OF_COLUMNS];
	private char OppSymbol;


	/* 
	 * The board object must contain the board state in some manner.
	 * You must decide how you will do this.
	 * 
	 * You may add addition private/public methods to this class is you wish.
	 * However, you should use best OO practices. That is, you should not expose
	 * how the board is being implemented to other classes. Specifically, the
	 * Player classes.
	 * 
	 * You may add private and public methods if you wish. In fact, to achieve
	 * what the assignment is asking, you'll have to
	 * 
	 */
	
	public Board() {
		
		for (int row = 0; row < NUM_OF_ROW; row++){
			for (int column = 0; column < NUM_OF_COLUMNS; column++){
				array[row][column] = ' ';
			}
		}
	}
	
	public void printBoard() {
		for (int row = 0; row < NUM_OF_ROW ; row++){
			System.out.print('|');
			for (int column = 0; column < NUM_OF_COLUMNS; column++){
				System.out.print(array[row][column]);
				System.out.print('|');
				
				}
			System.out.println();
			}
		System.out.println("---------------");
	}
	
	
	public boolean containsWin() {

		//checking 4 down
		for (int row = 0; row < NUM_OF_ROW - 3; row++){
			for (int column = 0; column < NUM_OF_COLUMNS; column++){
				if (array[row][column] != ' ') {
					if (array[row][column] == array[row][column] &&
					array[row + 1][column] == array[row][column] &&
					array[row + 2][column] == array[row][column] &&
					array[row + 3][column] == array[row][column]) {
						return true;
					}
				}
			}
		}
		//checking 4 across
		for (int row = 0; row < NUM_OF_ROW; row++){
			for (int column = 0; column < NUM_OF_COLUMNS - 3; column++){
				if (array[row][column] != ' ') {
					if (array[row][column] == array[row][column] &&
					array[row][column + 1] == array[row][column] &&
					array[row][column + 2] == array[row][column] &&
					array[row][column + 3] == array[row][column]){
						return true;
					}
				}
			}
		}

		//checking upwards diagonal 
		for (int row = 0; row < NUM_OF_ROW - 3; row++){
			for (int column = 0; column < NUM_OF_COLUMNS - 3; column++){
				if (array[row][column] != ' ') {
					if (array[row][column] == array[row][column] &&
					array[row + 1][column + 1] == array[row][column] &&
					array[row + 2][column + 2] == array[row][column] &&
					array[row + 3][column + 3] == array[row][column]){
						return true;
					}
				}
			}
		}

		//checking downwards diagonal
		for (int row = NUM_OF_ROW; row < NUM_OF_ROW - 3; row--){
			for (int column = 0; column < NUM_OF_COLUMNS - 3; column++){
				if (array[row][column] != ' ') {
					if (array[row][column] == array[row][column] &&
					array[row - 1][column + 1] == array[row][column] &&
					array[row - 2][column + 2] == array[row][column] &&
					array[row - 3][column + 3] == array[row][column]){
						return true;
					}
				} 
			}
		}

		return false;
	}
	
	public boolean isTie() {
		for (int row = 0; row < NUM_OF_ROW ; row++){
			for (int column = 0; column < NUM_OF_COLUMNS ; column++){
				if (array[row][column] == ' ') {
					return false;
				}
			}
		}
		return true;

	}
	
	public void reset() {
		for (int row = 0; row < NUM_OF_ROW; row++){
			for (int column = 0; column < NUM_OF_COLUMNS; column++){
				if (array[row][column] != ' '){
					array[row][column] = ' ';
				}
			}
		}
	}

	public int getRows(){
		return NUM_OF_ROW;
	}

	public int getColumn(){
		return NUM_OF_COLUMNS;
	}
	
	public char[][] getBoard(){
		return array;
	}

	public void humanMove(char symbol, int move){
		
		for (int row = NUM_OF_ROW ; row > 0; row--){
			if (array[row - 1][move-1] == ' '){
				array[row - 1][move-1] = symbol;
				break;
			}
		}
	}

	public boolean isColFull(int move){
		if (array[0][move-1] != ' ') {
			return true;
		}
		return false;
	}

	public void AIMove(char symbol, int move){
		for (int row = NUM_OF_ROW ; row > 0; row--){
			if (array[row - 1][move] == ' '){
				array[row - 1][move] = symbol;
				break;
			}
		}
	}


	public boolean IsWinningMove(char symbol){
		int x;
		for (int column = 0; column < NUM_OF_COLUMNS; column++){
			for (int row = NUM_OF_ROW - 1 ; row >= 0; row--){
				if (array[row][column] == ' ') {
					if (row == NUM_OF_ROW - 1){
						array[row ][column] = symbol;
					}

					else if (row > 0) {
						x = row + 1;
						if (array[x][column] != ' '){
							array[row - 1][column] = symbol;
						}
					}


				if (containsWin()){
					AIMove(symbol, column);
					return true;
				}
		
				else{
					array[row][column] = ' ';
				}
			}
					
		}
	}
		return false;
	}

	public char getOppSymbol (char symbol){
		for (int row = 0; row < NUM_OF_ROW; row++){
			for (int column = 0; column < NUM_OF_COLUMNS; column++){
				if (array[row][column] != symbol && array[row][column] != ' '){
					OppSymbol = array[row][column];
				}
			}
		}
		return OppSymbol;
	}

	public char getOppSymbol(){
		return OppSymbol;
	}
	
	public boolean IsOppWinningMove(char symbol){
		int x;
		for (int column = 0; column < NUM_OF_COLUMNS; column++){
			for (int row = NUM_OF_ROW - 1 ; row >= 0; row--){
				if (array[row][column] == ' ') {
					if (row == NUM_OF_ROW - 1){
						array[row][column] = OppSymbol;
					}

					else if (row > 0) {
						x = row + 1;
						if (array[x][column] != ' '){
							array[row - 1][column] = OppSymbol;
						}
					}


				if (containsWin()){
					AIMove(symbol, column);
					return true;
				}
		
				else{
					array[row][column] = ' ';
				}
			}
					
		}
	}
		return false;
	}

}
