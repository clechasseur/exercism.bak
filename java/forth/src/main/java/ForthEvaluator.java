import java.util.*;

class ForthEvaluator {
    List<Integer> evaluateProgram(List<String> programParts) {
        ForthParser parser = new ForthParser();
        Deque<Integer> stack = new LinkedList<>();
        programParts.stream()
                .flatMap(part -> parser.parseProgram(part).stream())
                .forEach(operator -> operator.apply(stack));
        Integer[] finalValues = stack.toArray(new Integer[0]);
        List<Integer> result = new ArrayList<>();
        for (int i = finalValues.length - 1; i >= 0; i--) {
            result.add(finalValues[i]);
        }
        return result;
    }
}
