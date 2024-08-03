import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

class OpticalCharacterReader {
    String parse(List<String> input) {
        if (input.isEmpty() || input.size() % 4 != 0) {
            throw new IllegalArgumentException("Number of input rows must be a positive multiple of 4");
        } else if (input.get(0).isEmpty() || input.get(0).length() % 3 != 0) {
            throw new IllegalArgumentException("Number of input columns must be a positive multiple of 3");
        }

        List<List<String>> lines = new ArrayList<>();
        for (int i = 0; i < input.size(); i += 4) {
            lines.add(input.subList(i, i + 4));
        }
        return lines.stream().map(this::parseLine).collect(Collectors.joining(","));
    }

    private String parseLine(List<String> line) {
        List<String> digits = new ArrayList<>();
        for (int i = 0; i < line.get(0).length(); i += 3) {
            StringBuilder sb = new StringBuilder();
            for (String row : line) {
                sb.append(row, i, i + 3);
            }
            digits.add(sb.toString());
        }
        return digits.stream().map(this::parseDigit).collect(Collectors.joining());
    }

    private String parseDigit(String digit) {
        return DIGITS.getOrDefault(digit, "?");
    }

    private static final Map<String, String> DIGITS = new HashMap<>() {{
        put(" _ "
          + "| |"
          + "|_|"
          + "   ", "0");
        put("   "
          + "  |"
          + "  |"
          + "   ", "1");
        put(" _ "
          + " _|"
          + "|_ "
          + "   ", "2");
        put(" _ "
          + " _|"
          + " _|"
          + "   ", "3");
        put("   "
          + "|_|"
          + "  |"
          + "   ", "4");
        put(" _ "
          + "|_ "
          + " _|"
          + "   ", "5");
        put(" _ "
          + "|_ "
          + "|_|"
          + "   ", "6");
        put(" _ "
          + "  |"
          + "  |"
          + "   ", "7");
        put(" _ "
          + "|_|"
          + "|_|"
          + "   ", "8");
        put(" _ "
          + "|_|"
          + " _|"
          + "   ", "9");
    }};
}