import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class Alphametics {
    private static final List<List<Integer>> INT_PERMUTATIONS = PermutationsHelper.getAllIntPermutations();

    private final Puzzle puzzle;

    Alphametics(String puzzle) {
        this.puzzle = new Puzzle(puzzle);
    }

    Map<Character, Integer> solve() throws UnsolvablePuzzleException {
        List<Map<Character, Integer>> permutations = getPermutations(this.puzzle.getLetters(), this.puzzle.getEdgeLetters());
        for (Map<Character, Integer> permutation : permutations) {
            if (this.puzzle.solve(permutation)) {
                return permutation;
            }
        }
        throw new UnsolvablePuzzleException();
    }

    private List<Map<Character, Integer>> getPermutations(Set<Character> letters, Set<Character> edgeLetters) {
        List<Map<Character, Integer>> permutations = new ArrayList<>();
        INT_PERMUTATIONS.forEach(intPermutation -> {
            SortedMap<Character, Integer> permutation = new TreeMap<>();
            int i = 0;
            for (Character c : letters) {
                permutation.put(c, intPermutation.get(i++));
            }
            if (isValidPermutation(permutation, edgeLetters)) {
                permutations.add(permutation);
            }
        });
        return permutations;
    }

    private boolean isValidPermutation(Map<Character, Integer> permutation, Set<Character> edgeLetters) {
        for (Character c : edgeLetters) {
            if (permutation.get(c) == 0) {
                return false;
            }
        }
        return true;
    }

    private static class Puzzle {
        private final List<String> leftSide = new ArrayList<>();
        private final String rightSide;
        private final Set<Character> letters = new HashSet<>();
        private final Set<Character> edgeLetters = new HashSet<>();

        Puzzle(String puzzle) {
            // Ultra-non-generic parsing Inc. (tm)
            Pattern pattern = Pattern.compile("[A-Z]+");
            Matcher matcher = pattern.matcher(puzzle);
            while (matcher.find()) {
                String block = matcher.group();
                edgeLetters.add(block.charAt(0));
                for (int i = 0; i < block.length(); ++i) {
                    letters.add(block.charAt(i));
                }
                leftSide.add(block);
            }
            if (!leftSide.isEmpty()) {
                rightSide = leftSide.remove(leftSide.size() - 1);
            } else {
                rightSide = "";
            }
        }

        Set<Character> getLetters() {
            return this.letters;
        }

        Set<Character> getEdgeLetters() {
            return this.edgeLetters;
        }

        boolean solve(Map<Character, Integer> permutation) {
            long leftTotal = 0;
            for (String block : this.leftSide) {
                leftTotal += getBlockTotal(block, permutation);
            }
            long rightTotal = getBlockTotal(this.rightSide, permutation);
            return leftTotal == rightTotal;
        }

        private long getBlockTotal(String block, Map<Character, Integer> permutation) {
            StringBuilder sb = new StringBuilder();
            for (char c : block.toCharArray()) {
                sb.append(permutation.get(c));
            }
            return Long.valueOf(sb.toString());
        }
    }

    private static class PermutationsHelper {
        static List<List<Integer>> getAllIntPermutations() {
            List<Integer> arr = new ArrayList<>();
            for (int i = 0; i <= 9; ++i) {
                arr.add(i);
            }
            List<List<Integer>> allPermutations = new ArrayList<>();
            permute(arr, 0, allPermutations);
            return allPermutations;
        }

        // Taken from https://stackoverflow.com/questions/2920315/permutation-of-array
        private static void permute(List<Integer> arr, int k, List<List<Integer>> allPermutations) {
            for (int i = k; i < arr.size(); i++) {
                Collections.swap(arr, i, k);
                permute(arr, k + 1, allPermutations);
                Collections.swap(arr, k, i);
            }
            if (k == arr.size() - 1 ){
                allPermutations.add(new ArrayList<>(arr));
            }
        }
    }
}
