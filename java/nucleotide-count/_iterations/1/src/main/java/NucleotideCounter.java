import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

public class NucleotideCounter {
    private Map<Character, Integer> nucleotides = new HashMap<>() {{
        put('A', 0);
        put('C', 0);
        put('G', 0);
        put('T', 0);
    }};

    public NucleotideCounter(String dnaStrand) {
        for (char nucleotide : Objects.requireNonNull(dnaStrand).toCharArray()) {
            Integer existingCount = this.nucleotides.get(nucleotide);
            if (existingCount == null) {
                throw new IllegalArgumentException("invalid nucleotide found: " + nucleotide);
            }
            this.nucleotides.put(nucleotide, existingCount + 1);
        }
        this.nucleotides = Collections.unmodifiableMap(this.nucleotides);
    }

    public Map<Character, Integer> nucleotideCounts() {
        return nucleotides;
    }
}
