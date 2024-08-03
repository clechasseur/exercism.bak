class SpiralMatrixBuilder {
    int[][] buildMatrixOfSize(int size) {
        int[][] matrix = new int[size][];
        for (int i = 0; i < size; ++i) {
            matrix[i] = new int[size];
        }

        if (size > 0) {
            int i = 1;
            Cursor cursor = new Cursor(0, 0, Direction.RIGHT);
            while (!cursor.outOfBounds(size) && matrix[cursor.y][cursor.x] == 0) {
                matrix[cursor.y][cursor.x] = i++;
                Cursor newCursor = cursor.move();
                if (newCursor.outOfBounds(size) || matrix[newCursor.y][newCursor.x] != 0) {
                    cursor = cursor.turnRight().move();
                } else {
                    cursor = newCursor;
                }
            }
        }

        return matrix;
    }

    private enum Direction {
        UP, DOWN, LEFT, RIGHT,
    }

    private static class Cursor {
        final int x;
        final int y;
        final Direction direction;

        Cursor(int x, int y, Direction direction) {
            this.x = x;
            this.y = y;
            this.direction = direction;
        }

        Cursor turnRight() {
            if (direction == Direction.UP) {
                return new Cursor(x, y, Direction.RIGHT);
            } else if (direction == Direction.RIGHT) {
                return new Cursor(x, y, Direction.DOWN);
            } else if (direction == Direction.DOWN) {
                return new Cursor(x, y, Direction.LEFT);
            } else {
                return new Cursor(x, y, Direction.UP);
            }
        }

        Cursor move() {
            if (direction == Direction.UP) {
                return new Cursor(x, y - 1, direction);
            } else if (direction == Direction.RIGHT) {
                return new Cursor(x + 1, y, direction);
            } else if (direction == Direction.DOWN) {
                return new Cursor(x, y + 1, direction);
            } else {
                return new Cursor(x - 1, y, direction);
            }
        }

        boolean outOfBounds(int size) {
            return x < 0 || x >= size || y < 0 || y >= size;
        }
    }
}
