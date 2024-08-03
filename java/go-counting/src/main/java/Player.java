enum Player {
    NONE(' '),
    BLACK('B'),
    WHITE('W');

    private static final Player[] VALUES = values();

    private final char symbol;

    Player(final char symbol) {
        this.symbol = symbol;
    }

    char getSymbol() {
        return symbol;
    }

    static Player forSymbol(final char symbol) {
        for (Player player : VALUES) {
            if (player.getSymbol() == symbol) {
                return player;
            }
        }
        throw new IllegalArgumentException("Invalid player symbol: " + symbol);
    }
}
