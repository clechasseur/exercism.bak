class CollatzCalculator {
    int computeStepCount(int start) {
        if (start <= 0) {
            throw new IllegalArgumentException("Only natural numbers are allowed");
        }
        
        int steps = 0;
        int number = start;
        while (number != 1) {
            number = (number % 2 == 0) ? number / 2 : 3 * number + 1;
            ++steps;
        }
        return steps;
    }
}
