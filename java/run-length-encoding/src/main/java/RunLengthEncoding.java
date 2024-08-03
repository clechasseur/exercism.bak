import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RunLengthEncoding {
    private static final Pattern ENCODE_REGEX = Pattern.compile("([a-zA-Z ])\\1*");
    private static final Pattern DECODE_REGEX = Pattern.compile("([0-9]+)?([a-zA-Z ])");


    public String encode(String input) {
        StringBuilder output = new StringBuilder();
        Matcher matcher = ENCODE_REGEX.matcher(input);
        while (matcher.find()) {
            if (matcher.group().length() > 1) {
                output.append(matcher.group().length());
            }
            output.append(matcher.group(1));
        }
        return output.toString();
    }

    public String decode(String input) {
        StringBuilder output = new StringBuilder();
        Matcher matcher = DECODE_REGEX.matcher(input);
        while (matcher.find()) {
            String count = matcher.group(1);
            String data = matcher.group(2);
            for (int i = 0; i < (count != null ? Integer.valueOf(count) : 1); ++i) {
                output.append(data);
            }
        }
        return output.toString();
    }
}
