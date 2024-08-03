import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

class MinesweeperBoard {
    private final List<List<String>> board;
    private final int height;
    private final int width;

    MinesweeperBoard(List<String> board) {
        this.board = board.stream().map(line -> Arrays.asList(line.split(""))).collect(Collectors.toList());
        this.height = this.board.size();
        this.width = this.height > 0 ? this.board.get(0).size() : 0;
        addNumbers();
    }

    List<String> withNumbers() {
        return board.stream().map(line -> String.join("", line)).collect(Collectors.toList());
    }

    private void addNumbers() {
        for (int x = 0; x < width; ++x) {
            for (int y = 0; y < height; ++y) {
                if (board.get(y).get(x).equals(" ")) {
                    int numMines = 0;
                    List<Pos> adjacentPos = adjacentPositions(new Pos(x, y));
                    for (Pos pos : adjacentPos) {
                        if (board.get(pos.y).get(pos.x).equals("*")) {
                            ++numMines;
                        }
                    }
                    if (numMines != 0) {
                        board.get(y).set(x, String.valueOf(numMines));
                    }
                }
            }
        }
    }

    private List<Pos> adjacentPositions(Pos position) {
        List<Pos> positions = new ArrayList<>();
        for (int xdiff = -1; xdiff <= 1; ++xdiff) {
            for (int ydiff = -1; ydiff <= 1; ++ydiff) {
                if (xdiff != 0 || ydiff != 0) {
                    Pos newPos = new Pos(position.x + xdiff, position.y + ydiff);
                    if (validPosition(newPos)) {
                        positions.add(newPos);
                    }
                }
            }
        }
        return positions;
    }

    private boolean validPosition(Pos position) {
        return position.x >= 0 &&
                position.x < width &&
                position.y >= 0 &&
                position.y < height;
    }

    private static final class Pos {
        final int x;
        final int y;

        Pos(int x, int y) {
            this.x = x;
            this.y = y;
        }
    }
}
