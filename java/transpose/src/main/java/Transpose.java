import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

class Transpose {
    private static final Pattern END_SPACE = Pattern.compile("\\s+$");

    String transpose(String input) {
        List<String> lines = Arrays.asList(input.split("\n"));
        int longestLineLength = lines.stream().mapToInt(String::length).max().orElse(0);
        List<String> newLines = IntStream.range(0, longestLineLength)
                                         .mapToObj(lineNum -> lines.stream()
                                                                   .map(line -> line.length() > lineNum ? line.substring(lineNum, lineNum + 1) : " ")
                                                                   .collect(Collectors.joining()))
                                         .collect(Collectors.toList());
        int start = 0;
        for (int i = newLines.size() - 1; i >= 0; --i) {
            String line = newLines.get(i);
            Matcher matcher = END_SPACE.matcher(line);
            if (matcher.find(start)) {
                start = matcher.start();
                newLines.set(i, line.substring(0, start));
            } else {
                break;
            }
        }
        return String.join("\n", newLines);
    }
}
