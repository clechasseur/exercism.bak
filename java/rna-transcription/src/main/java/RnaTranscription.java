import java.util.Objects;
import java.util.stream.Collectors;

class RnaTranscription {
    String transcribe(String dnaStrand) {
        return Objects.requireNonNull(dnaStrand).chars()
                                                .mapToObj(nucleotide -> String.valueOf(transcribe((char) nucleotide)))
                                                .collect(Collectors.joining());
    }

    private char transcribe(char nucleotide) {
        switch (nucleotide) {
            case 'G':
                return 'C';
            case 'C':
                return 'G';
            case 'T':
                return 'A';
            case 'A':
                return 'U';
            default:
                throw new IllegalArgumentException("Invalid nucleotide: " + nucleotide);
        }
    }
}
