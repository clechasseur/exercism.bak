import java.util.Deque;

final class ForthOperators {

    static final class Push implements ForthOperator {
        private final int value;

        Push(int value) {
            this.value = value;
        }

        @Override
        public void apply(Deque<Integer> stack) {
            stack.push(value);
        }
    }

    static final class Plus implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.size() < 2) {
                throw new IllegalArgumentException("Addition requires that the stack contain at least 2 values");
            }
            stack.push(stack.pop() + stack.pop());
        }
    }

    static final class Minus implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.size() < 2) {
                throw new IllegalArgumentException("Subtraction requires that the stack contain at least 2 values");
            }
            int b = stack.pop();
            int a = stack.pop();
            stack.push(a - b);
        }
    }

    static final class Multiply implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.size() < 2) {
                throw new IllegalArgumentException("Multiplication requires that the stack contain at least 2 values");
            }
            stack.push(stack.pop() * stack.pop());
        }
    }

    static final class Divide implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.size() < 2) {
                throw new IllegalArgumentException("Division requires that the stack contain at least 2 values");
            } else if (stack.peek() == 0) {
                throw new IllegalArgumentException("Division by 0 is not allowed");
            }
            int denominator = stack.pop();
            int numerator = stack.pop();
            stack.push(numerator / denominator);
        }
    }

    static final class Dup implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.isEmpty()) {
                throw new IllegalArgumentException("Duplicating requires that the stack contain at least 1 value");
            }
            int value = stack.pop();
            stack.push(value);
            stack.push(value);
        }
    }

    static final class Drop implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.isEmpty()) {
                throw new IllegalArgumentException("Dropping requires that the stack contain at least 1 value");
            }
            stack.pop();
        }
    }

    static final class Swap implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.size() < 2) {
                throw new IllegalArgumentException("Swapping requires that the stack contain at least 2 values");
            }
            int first = stack.pop();
            int second = stack.pop();
            stack.push(first);
            stack.push(second);
        }
    }

    static final class Over implements ForthOperator {
        @Override
        public void apply(Deque<Integer> stack) {
            if (stack.size() < 2) {
                throw new IllegalArgumentException("Overing requires that the stack contain at least 2 values");
            }
            int ignore = stack.pop();
            int toOver = stack.getFirst();
            stack.push(ignore);
            stack.push(toOver);
        }
    }

    private ForthOperators() {
        // No instances
    }
}
