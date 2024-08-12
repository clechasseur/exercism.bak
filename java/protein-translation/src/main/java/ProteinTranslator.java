import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class ProteinTranslator {
    private static final Map<String, String> CODONS = new HashMap<>() {{
        put("AUG", "Methionine");
        put("UUU", "Phenylalanine");
        put("UUC", "Phenylalanine");
        put("UUA", "Leucine");
        put("UUG", "Leucine");
        put("UCU", "Serine");
        put("UCC", "Serine");
        put("UCA", "Serine");
        put("UCG", "Serine");
        put("UAU", "Tyrosine");
        put("UAC", "Tyrosine");
        put("UGU", "Cysteine");
        put("UGC", "Cysteine");
        put("UGG", "Tryptophan");
        put("UAA", "");
        put("UAG", "");
        put("UGA", "");
    }};

    List<String> translate(String rnaSequence) {
        if (rnaSequence == null) {
            throw new IllegalArgumentException("rna sequence cannot be null.");
        }
        if (rnaSequence.length() % 3 != 0) {
            throw new IllegalArgumentException("rna sequence size must be divisible by 3.");
        }

        Pattern pattern = Pattern.compile(".{3}");
        Matcher matcher = pattern.matcher(rnaSequence);
        List<String> output = new ArrayList<>();
        while (matcher.find()) {
            String codon = matcher.group();
            String protein = CODONS.get(codon);
            if (protein == null) {
                throw new IllegalArgumentException("rna sequence contains invalid codon: " + codon);
            } else if (protein.isEmpty()) {
                break;
            }
            output.add(protein);
        }
        return output;
    }
}
