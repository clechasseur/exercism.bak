import java.util.*;

public class BracketChecker {
    private static final Set<Character> OPENERS = new HashSet<Character>() {{
        add('[');
        add('{');
        add('(');
    }};
    private static final Map<Character, Character> FINISHERS = new HashMap<Character, Character>() {{
        put(']', '[');
        put('}', '{');
        put(')', '(');
    }};

    private boolean valid;

    public BracketChecker(String expression) {
        Deque<Character> stack = new ArrayDeque<>();
        int i = 0;
        while (i < expression.length()) {
            char c = expression.charAt(i);
            if (OPENERS.contains(c)) {
                stack.push(c);
            } else if (FINISHERS.containsKey(c)) {
                if (stack.peek() == FINISHERS.get(c)) {
                    stack.pop();
                } else {
                    break;
                }
            }
            ++i;
        }
        valid = i == expression.length() && stack.isEmpty();
    }

    public boolean areBracketsMatchedAndNestedCorrectly() {
        return valid;
    }
}
