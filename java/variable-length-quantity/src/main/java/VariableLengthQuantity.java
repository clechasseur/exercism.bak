import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

class VariableLengthQuantity {

    List<String> encode(List<Long> numbers) {
        return numbers.stream().flatMap(number -> this.encodeOne(number).stream()).collect(Collectors.toList());
    }

    List<String> decode(List<Long> bytes) {
        List<String> result = new ArrayList<>();
        int i = 0;
        while (i < bytes.size()) {
            List<Long> thisInt = new ArrayList<>();
            do {
                if (i >= bytes.size()) {
                    throw new IllegalArgumentException("Invalid variable-length quantity encoding");
                }
                thisInt.add(bytes.get(i++));
            } while ((thisInt.get(thisInt.size() - 1) & 0x80) != 0);
            result.add(this.decodeOne(thisInt));
        }
        return result;
    }

    private List<String> encodeOne(long number) {
        List<String> result = new ArrayList<>();
        if (number == 0) {
            result.add("0x0");
        } else {
            while (number > 0) {
                long thisByte = number % 0x80;
                number /= 0x80;
                if (!result.isEmpty()) {
                    thisByte |= 0x80;
                }
                result.add(String.format("0x%x", thisByte));
            }
        }
        Collections.reverse(result);
        return result;
    }

    private String decodeOne(List<Long> bytes) {
        long result = 0;
        long pow = 1;
        Collections.reverse(bytes);
        for (long thisByte : bytes) {
            result += (thisByte & 0x7f) * pow;
            pow *= 0x80;
        }
        return String.format("0x%x", result);
    }
}
