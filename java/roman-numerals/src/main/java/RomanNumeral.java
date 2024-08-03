import java.util.stream.Collectors;
import java.util.stream.IntStream;

class RomanNumeral {
    private static final NumeralInfo[] NUMERAL_INFOS = new NumeralInfo[] {
            new NumeralInfo('I', 'V', 'X'),
            new NumeralInfo('X', 'L', 'C'),
            new NumeralInfo('C', 'D', 'M'),
            new NumeralInfo('M', '?', '?'),
    };

    private final String romanNumeral;

    RomanNumeral(int input) {
        if (input <= 0) {
            throw new IllegalArgumentException("Romans did not know how to express zero.");
        } else if (input >= 4000) {
            throw new IllegalArgumentException("Romans seldom expressed numbers above 3000-ish.");
        }
        this.romanNumeral = toRomanNumeral(input);
    }

    String getRomanNumeral() {
        return romanNumeral;
    }

    private static String toRomanNumeral(int input) {
        String inputStr = Integer.toString(input);
        return IntStream.range(0, inputStr.length())
                        .mapToObj(i -> toRomanNumeral(inputStr.charAt(i) - '0',
                                            NUMERAL_INFOS[inputStr.length() - i - 1]))
                        .collect(Collectors.joining());
    }

    private static String toRomanNumeral(int input, NumeralInfo numeralInfo) {
        if (input == 9) {
            return String.format("%c%c", numeralInfo.getOne(), numeralInfo.getTen());
        } else if (input >= 5) {
            return String.format("%c%s", numeralInfo.getFive(),
                    Character.toString(numeralInfo.getOne()).repeat(input - 5));
        } else if (input == 4) {
            return String.format("%c%c", numeralInfo.getOne(), numeralInfo.getFive());
        } else {
            return Character.toString(numeralInfo.getOne()).repeat(input);
        }
    }

    private static final class NumeralInfo {
        private final char one;
        private final char five;
        private final char ten;

        NumeralInfo(char one, char five, char ten) {
            this.one = one;
            this.five = five;
            this.ten = ten;
        }

        char getOne() {
            return one;
        }

        char getFive() {
            return five;
        }

        char getTen() {
            return ten;
        }
    }
}
