import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class PascalsTriangleGenerator {
    public int[][] generateTriangle(int rows) {
        List<List<Integer>> output = new ArrayList<>();

        if (rows > 0) {
            output.add(Collections.singletonList(1));
            if (rows > 1) {
                output.add(Arrays.asList(1, 1));

                for (int i = 2; i < rows; ++i) {
                    List<Integer> prevRow = output.get(i - 1);
                    List<Integer> row = new ArrayList<>();
                    row.add(1);
                    for (int j = 1; j < i; ++j) {
                        row.add(prevRow.get(j - 1) + prevRow.get(j));
                    }
                    row.add(1);
                    output.add(row);
                }
            }
        }

        return output.stream().map(row -> {
            int[] aRow = new int[row.size()];
            for (int i = 0; i < row.size(); ++i) {
                aRow[i] = row.get(i);
            }
            return aRow;
        }).toArray(int[][]::new);
    }
}
