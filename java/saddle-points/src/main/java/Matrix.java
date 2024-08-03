import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class Matrix {
    private final List<List<Integer>> matrix;

    Matrix(List<List<Integer>> values) {
        this.matrix = values;
    }

    Set<MatrixCoordinate> getSaddlePoints() {
        return allCoordinates().filter(
                coord -> rowCoordinates(coord.getRow()).allMatch(rowCoord -> valueAt(rowCoord) <= valueAt(coord))
        ).filter(
                coord -> colCoordinates(coord.getCol()).allMatch(colCoord -> valueAt(colCoord) >= valueAt(coord))
        ).collect(Collectors.toSet());
    }

    private int valueAt(MatrixCoordinate coordinate) {
        return matrix.get(coordinate.getRow()).get(coordinate.getCol());
    }

    private Stream<MatrixCoordinate> allCoordinates() {
        final int rows = matrix.size();
        final int cols = (rows > 0) ? matrix.get(0).size() : 0;
        return Stream.iterate(new MatrixCoordinate(0, 0),
                coord -> coord.getRow() < rows && coord.getCol() < cols,
                coord -> {
                    if (coord.getCol() < (cols - 1)) {
                        return new MatrixCoordinate(coord.getRow(), coord.getCol() + 1);
                    } else {
                        return new MatrixCoordinate(coord.getRow() + 1, 0);
                    }
                });
    }

    private Stream<MatrixCoordinate> rowCoordinates(int row) {
        final int cols = (matrix.size() > 0) ? matrix.get(0).size() : 0;
        return Stream.iterate(new MatrixCoordinate(row, 0),
                coord -> coord.getCol() < cols,
                coord -> new MatrixCoordinate(row, coord.getCol() + 1));
    }

    private Stream<MatrixCoordinate> colCoordinates(int col) {
        final int rows = matrix.size();
        return Stream.iterate(new MatrixCoordinate(0, col),
                coord -> coord.getRow() < rows,
                coord -> new MatrixCoordinate(coord.getRow() + 1, col));
    }
}
