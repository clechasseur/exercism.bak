class LargestSeriesProduct {
    static largestProduct(String digits, int span) {
        if (span < 0) {
            throw new IllegalArgumentException("span must not be negative")
        } else if (span > digits.length()) {
            throw new IllegalArgumentException("span must be smaller than string length")
        } else if (!digits.isEmpty() && !digits.isBigInteger()) {
            throw new IllegalArgumentException("digits input must only contain digits")
        }

        // This is an optimization - the code below could find that answer,
        // but it would iterate over the entire input string for no good reason.
        if (span == 0) {
            return 1
        }

        def largest = 0
        for (def i = 0; i <= digits.length() - span; ++i) {
            def value = 1
            for (def j = i, end = i + span; j < end; ++j) {
                def digit = digits[j].toInteger()
                value *= digit
                if (digit == 0) {
                    i = j
                }
            }

            largest = Math.max(largest, value)
        }

        return largest
    }
}
