import java.util.regex.Pattern;

public class PhoneNumber {
    private static final Pattern PUNCTUATIONS = Pattern.compile("[\\s.()-]");
    private static final Pattern LETTER = Pattern.compile("[a-zA-Z]");
    private static final Pattern NON_NUMBER = Pattern.compile("\\D");

    private String cleanedNumber;

    PhoneNumber(String inputNumber) {
        this.cleanedNumber = cleanup(inputNumber);
    }

    public String getNumber() {
        return cleanedNumber;
    }

    private static String cleanup(String inputNumber) {
        require(inputNumber != null, "number cannot be null");
        String cleanedNumber = PUNCTUATIONS.matcher(inputNumber).replaceAll("");
        if (cleanedNumber.startsWith("+")) {
            cleanedNumber = cleanedNumber.substring(1);
        }
        require(!LETTER.matcher(cleanedNumber).find(), "letters not permitted");
        require(!NON_NUMBER.matcher(cleanedNumber).find(), "punctuations not permitted");
        require(cleanedNumber.length() <= 11, "more than 11 digits");
        if (cleanedNumber.length() == 11) {
            require(cleanedNumber.startsWith("1"), "11 digits must start with 1");
            cleanedNumber = cleanedNumber.substring(1);
        }
        require(cleanedNumber.length() == 10, "incorrect number of digits");
        char areaCodeStart = cleanedNumber.charAt(0);
        require(areaCodeStart != '0', "area code cannot start with zero");
        require(areaCodeStart != '1', "area code cannot start with one");
        char exchangeCodeStart = cleanedNumber.charAt(3);
        require(exchangeCodeStart != '0', "exchange code cannot start with zero");
        require(exchangeCodeStart != '1', "exchange code cannot start with one");

        return cleanedNumber;
    }

    private static void require(Boolean condition, String message) {
        if (!condition) {
            throw new IllegalArgumentException(message);
        }
    }
}
