class GridPosition {
    final int x;
    final int y;

    GridPosition(int x, int y) {
        this.x = x;
        this.y = y;
    }

    GridPosition move(Orientation orientation) {
        if (orientation == Orientation.NORTH) {
            return new GridPosition(x, y + 1);
        } else if (orientation == Orientation.EAST) {
            return new GridPosition(x + 1, y);
        } else if (orientation == Orientation.SOUTH) {
            return new GridPosition(x, y - 1);
        } else {
            return new GridPosition(x - 1, y);
        }
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + x;
        result = prime * result + y;
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null || getClass() != obj.getClass()) {
            return false;
        } else if (x != ((GridPosition) obj).x || y != ((GridPosition)obj).y) {
            return false;
        } else {
            return true;
        }
    }
}
