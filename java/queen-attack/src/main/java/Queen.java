class Queen {
    private final int row;
    private final int col;

    Queen(int row, int col) {
        if (row < 0) {
            throw new IllegalArgumentException("Queen position must have positive row.");
        }
        if (row > 7) {
            throw new IllegalArgumentException("Queen position must have row <= 7.");
        }
        if (col < 0) {
            throw new IllegalArgumentException("Queen position must have positive column.");
        }
        if (col > 7) {
            throw new IllegalArgumentException("Queen position must have column <= 7.");
        }

        this.row = row;
        this.col = col;
    }

    int getRow() {
        return row;
    }

    int getCol() {
        return col;
    }
}
