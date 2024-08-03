import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

class Matrix {
    private final List<List<Integer>> matrix = new ArrayList<>();

    Matrix(String matrixAsString) {
        Objects.requireNonNull(matrixAsString).lines().forEach(
                line -> matrix.add(Arrays.stream(line.split("\\s+")).map(Integer::valueOf)
                                                                    .collect(Collectors.toList()))
        );
    }

    int[] getRow(int rowNumber) {
        if (rowNumber < 1 || rowNumber > matrix.size()) {
            throw new IllegalArgumentException("Invalid row number: " + rowNumber);
        }
        return matrix.get(rowNumber - 1).stream().mapToInt(Integer::intValue).toArray();
    }

    int[] getColumn(int columnNumber) {
        if (columnNumber < 1 || columnNumber > (matrix.size() >= 1 ? matrix.get(0).size() : 0)) {
            throw new IllegalArgumentException("Invalid column number: " + columnNumber);
        }
        return matrix.stream().mapToInt(row -> row.get(columnNumber - 1)).toArray();
    }
}
