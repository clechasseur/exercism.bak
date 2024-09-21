import java.util.Arrays;

class RectangleCounter {
    int countRectangles(String[] grid) {
        int colCount = grid.length;
        int rowCount = colCount > 0 ? grid[0].length() : 0;
        int count = 0;
        for (int x = 0; x < rowCount; x++) {
            for (int y = 0; y < colCount; y++) {
                for (int xi = x + 1; xi < rowCount; xi++) {
                    for (int yi = y + 1; yi < colCount; yi++) {
                        if (isRectangle(grid, x, y, xi, yi)) {
                            count++;
                        }
                    }
                }
            }
        }
        return count;
    }

    private boolean isRectangle(String[] grid, int x, int y, int xi, int yi) {
        return grid[y].charAt(x) == '+' && grid[yi].charAt(xi) == '+'
                && grid[y].charAt(xi) == '+' && grid[yi].charAt(x) == '+'
                && isHorizontalLine(grid, y, x + 1, xi - 1)
                && isHorizontalLine(grid, yi, x + 1, xi - 1)
                && isVerticalLine(grid, x, y + 1, yi - 1)
                && isVerticalLine(grid, xi, y + 1, yi - 1);
    }

    private boolean isHorizontalLine(String[] grid, int y, int x, int xi) {
        return grid[y].substring(x, xi + 1).chars().allMatch(c -> c == '-' || c == '+');
    }

    private boolean isVerticalLine(String[] grid, int x, int y, int yi) {
        return Arrays.stream(grid).mapToInt(row -> row.charAt(x)).skip(y).limit(yi - y + 1).allMatch(c -> c == '|' || c == '+');
    }
}
