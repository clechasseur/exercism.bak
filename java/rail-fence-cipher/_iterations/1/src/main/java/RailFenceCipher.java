import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

class RailFenceCipher {
    private final int numRails;

    RailFenceCipher(int numRails) {
        this.numRails = numRails;
    }

    String getEncryptedData(String input) {
        List<StringBuilder> rails = new ArrayList<>();
        for (int i = 0; i < numRails; ++i) {
            rails.add(new StringBuilder());
        }
        List<Integer> railNums = getRailNums(input.length()).collect(Collectors.toList());
        for (int i = 0; i < input.length(); ++i) {
            rails.get(railNums.get(i)).append(input.charAt(i));
        }
        return rails.stream().map(StringBuilder::toString).collect(Collectors.joining());
    }

    String getDecryptedData(String input) {
        List<StringBuilder> rails = new ArrayList<>();
        for (int i = 0; i < numRails; ++i) {
            rails.add(new StringBuilder());
        }
        List<Integer> sortedRailNums = getRailNums(input.length()).sorted().collect(Collectors.toList());
        for (int i = 0; i < input.length(); ++i) {
            rails.get(sortedRailNums.get(i)).append(input.charAt(i));
        }
        return getRailNums(input.length()).map(railNum -> {
            StringBuilder sb = rails.get(railNum);
            String s = sb.substring(0, 1);
            sb.deleteCharAt(0);
            return s;
        }).collect(Collectors.joining());
    }

    private Stream<Integer> getRailNums(int inputSize) {
        return Stream.iterate(new OscillatorData(), data -> {
            OscillatorData newData = new OscillatorData(data.number + (data.up ? 1 : -1), data.up);
            if (newData.number >= numRails) {
                newData = new OscillatorData(data.number - 1, false);
            } else if (newData.number < 0) {
                newData = new OscillatorData(1, true);
            }
            return newData;
        }).limit(inputSize).map(data -> data.number);
    }

    private static final class OscillatorData {
        final int number;
        final boolean up;

        OscillatorData() {
            this.number = 0;
            this.up = true;
        }

        OscillatorData(int number, boolean up) {
            this.number = number;
            this.up = up;
        }
    }
}
