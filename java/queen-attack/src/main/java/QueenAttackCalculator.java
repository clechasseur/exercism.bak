class QueenAttackCalculator {
    private final Queen queen1;
    private final Queen queen2;

    QueenAttackCalculator(Queen queen1, Queen queen2) {
        if (queen1 == null || queen2 == null) {
            throw new IllegalArgumentException("You must supply valid positions for both Queens.");
        }
        if (queen1.getRow() == queen2.getRow() && queen1.getCol() == queen2.getCol()) {
            throw new IllegalArgumentException("Queens cannot occupy the same position.");
        }

        this.queen1 = queen1;
        this.queen2 = queen2;
    }

    boolean canQueensAttackOneAnother() {
        return queen1.getRow() == queen2.getRow() ||
                queen1.getCol() == queen2.getCol() ||
                Math.abs(queen1.getRow() - queen2.getRow()) == Math.abs(queen1.getCol() - queen2.getCol());
    }
}
