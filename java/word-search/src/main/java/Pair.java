class Pair {
    private final int x;
    private final int y;

    Pair(final int x, final int y) {
        this.y = y;
        this.x = x;
    }

    int getX() {
        return x;
    }

    int getY() {
        return y;
    }

    Pair plus(Pair o) {
        return new Pair(getX() + o.getX(), getY() + o.getY());
    }

    Pair times(int i) {
        return new Pair(getX() * i, getY() * i);
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Pair pair = (Pair) o;

        return x == pair.x && y == pair.y;
    }

    @Override
    public int hashCode() {
        int result = x;
        result = 31 * result + y;
        return result;
    }
}
