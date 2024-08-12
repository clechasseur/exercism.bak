import java.util.*;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class ForthParser {
    private static final Pattern DEFINITION_REGEX = Pattern.compile("^: ([^ ]+) ([^ ]+( [^ ]+)*) ;$");

    private final Map<String, Function<String, ForthOperator>> creators = new HashMap<>() {{
        put("+", word -> new ForthOperators.Plus());
        put("-", word -> new ForthOperators.Minus());
        put("*", word -> new ForthOperators.Multiply());
        put("/", word -> new ForthOperators.Divide());
        put("DUP", word -> new ForthOperators.Dup());
        put("DROP", word -> new ForthOperators.Drop());
        put("SWAP", word -> new ForthOperators.Swap());
        put("OVER", word -> new ForthOperators.Over());
    }};

    List<ForthOperator> parseProgram(String program) {
        List<ForthOperator> operators = new ArrayList<>();
        for (int pos = 0; pos < program.length(); ) {
            if (program.charAt(pos) == ':') {
                int definitionEnd = program.indexOf(';', pos + 1);
                String definition = program.substring(pos, definitionEnd + 1);
                pos = definitionEnd + 1;
                Matcher definitionMatcher = DEFINITION_REGEX.matcher(definition);
                if (!definitionMatcher.matches()) {
                    throw new IllegalArgumentException("Invalid word definition");
                }
                String newWord = definitionMatcher.group(1);
                String newProgram = definitionMatcher.group(2);
                try {
                    Integer.parseInt(newWord);
                    throw new IllegalArgumentException("Cannot redefine numbers");
                } catch (NumberFormatException e) {
                    final List<ForthOperator> subProgram = parseProgram(newProgram);
                    creators.put(newWord.toUpperCase(), word -> new CustomOperator(subProgram));
                }
            } else {
                int nextSpace = program.indexOf(' ', pos + 1);
                String word =  program.substring(pos, nextSpace != -1 ? nextSpace : program.length());
                pos = nextSpace != -1 ? nextSpace + 1 : program.length();
                try {
                    int value = Integer.parseInt(word);
                    operators.add(new ForthOperators.Push(value));
                } catch (NumberFormatException e) {
                    Function<String, ForthOperator> creator = creators.get(word.toUpperCase());
                    if (creator == null) {
                        throw new IllegalArgumentException("No definition available for operator \"" + word + "\"");
                    }
                    operators.add(creator.apply(word));
                }
            }
        }
        return operators;
    }

    private static final class CustomOperator implements ForthOperator {
        private final List<ForthOperator> subOperators;

        CustomOperator(List<ForthOperator> subOperators) {
            this.subOperators = subOperators;
        }

        @Override
        public void apply(Deque<Integer> stack) {
            subOperators.forEach(operator -> operator.apply(stack));
        }
    }
}
