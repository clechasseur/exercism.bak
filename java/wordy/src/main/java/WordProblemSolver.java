import java.util.HashMap;
import java.util.Map;
import java.util.function.BinaryOperator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

class WordProblemSolver {
    private static final Pattern EXPRESSION = Pattern.compile(
            "^What is (-?\\d+)((?: (?:plus|minus|multiplied by|divided by) -?\\d+)*)\\?$");
    private static final Pattern OPERATIONS = Pattern.compile(" (plus|minus|multiplied by|divided by) (-?\\d+)");
    private static final Map<String, BinaryOperator<Integer>> OP_IMPLS = new HashMap<String, BinaryOperator<Integer>>() {{
        put("plus", Integer::sum);
        put("minus", (a, b) -> a - b);
        put("multiplied by", (a, b) -> a * b);
        put("divided by", (a, b) -> a / b);
    }};

    int solve(String input) {
        Matcher expressionMatcher = EXPRESSION.matcher(input);
        if (!expressionMatcher.find()) {
            throw new IllegalArgumentException("I'm sorry, I don't understand the question!");
        }

        int result = Integer.valueOf(expressionMatcher.group(1));
        String operations = expressionMatcher.group(2);
        if (!operations.isEmpty()) {
            Matcher operationMatcher = OPERATIONS.matcher(operations);
            while (operationMatcher.find()) {
                String operation = operationMatcher.group(1);
                int value = Integer.valueOf(operationMatcher.group(2));
                result = OP_IMPLS.get(operation).apply(result, value);
            }
        }

        return result;
    }
}
