import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class DiamondPrinter {
    List<String> printToList(char a) {
        return IntStream.concat(IntStream.rangeClosed(0, a - 'A'),
                                IntStream.iterate((a - 'A') - 1, i -> i >= 0, i -> i - 1))
                        .mapToObj(i -> getLine(a, (char) ('A' + i)))
                        .collect(Collectors.toList());
    }

    private String getLine(char highest, char c) {
        if (c == 'A') {
            String outside = spaces(highest - 'A');
            return String.format("%sA%s", outside, outside);
        } else {
            String outside = spaces(highest - c);
            String inside = spaces((c - 'A') * 2 - 1);
            return String.format("%s%c%s%c%s", outside, c, inside, c, outside);
        }
    }

    private String spaces(int num) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < num; ++i) {
            sb.append(' ');
        }
        return sb.toString();
    }
}
