import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GrepTool {
    public String grep(String stringPattern, List<String> options, List<String> filenames) {
        Flags flags = new Flags(options);
        Pattern pattern = buildPattern(stringPattern, flags);
        boolean manyFiles = filenames.size() > 1;

        return filenames.stream()
                        .flatMap(filename -> grepFile(pattern, flags, filename, manyFiles))
                        .collect(Collectors.joining("\n"));
    }

    private Pattern buildPattern(String stringPattern, Flags flags) {
        if (flags.entireLines) {
            stringPattern = "^" + stringPattern + "$";
        }
        int regexFlags = 0;
        if (flags.caseInsensitive) {
            regexFlags = Pattern.CASE_INSENSITIVE;
        }
        return Pattern.compile(stringPattern, regexFlags);
    }

    private Stream<String> grepFile(Pattern pattern, Flags flags, String filename, boolean manyFiles) {
        List<String> fileLines;
        try {
            Path filePath = Paths.get(filename);
            fileLines = Files.readAllLines(filePath, Charset.forName("UTF-8"));
        } catch (IOException ioexp) {
            fileLines = Collections.emptyList();
        }

        Predicate<String> matchPredicate = pattern.asPredicate();
        if (flags.invert) {
            matchPredicate = matchPredicate.negate();
        }
        List<NumberedLine> matchingLines = new ArrayList<>();
        for (int i = 0; i < fileLines.size(); ++i) {
            String line = fileLines.get(i);
            if (matchPredicate.test(line)) {
                matchingLines.add(new NumberedLine(i + 1, line));
            }
        }

        if (flags.namesOnly) {
            return matchingLines.size() > 0 ? Stream.of(filename) : Stream.empty();
        }
        return matchingLines.stream().map(numberedLine -> {
            StringBuilder sb = new StringBuilder();
            if (manyFiles) {
                sb.append(filename).append(':');
            }
            if (flags.lineNumbers) {
                sb.append(numberedLine.lineNumber).append(':');
            }
            sb.append(numberedLine.line);
            return sb.toString();
        });
    }

    private static class Flags {
        final boolean lineNumbers;
        final boolean namesOnly;
        final boolean caseInsensitive;
        final boolean invert;
        final boolean entireLines;

        Flags(List<String> options) {
            this.lineNumbers = options.contains("-n");
            this.namesOnly = options.contains("-l");
            this.caseInsensitive = options.contains("-i");
            this.invert = options.contains("-v");
            this.entireLines = options.contains("-x");
        }
    }

    private static class NumberedLine {
        final int lineNumber;
        final String line;

        NumberedLine(int lineNumber, String line) {
            this.lineNumber = lineNumber;
            this.line = line;
        }
    }
}
