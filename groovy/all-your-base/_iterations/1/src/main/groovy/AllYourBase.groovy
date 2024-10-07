class AllYourBase {
    private int value = 0

    AllYourBase(int inputBase, List<Integer> digits) {
        if (inputBase < 2) {
            throw new ArithmeticException("input base must be >= 2")
        }

        digits.each { digit ->
            if (digit !in 0..<inputBase) {
                throw new ArithmeticException("each digit must be in 0..<inputBase")
            }

            value *= inputBase
            value += digit
        }
    }

    def rebase(int outputBase) {
        if (outputBase < 2) {
            throw new ArithmeticException("output base must be >= 2")
        }

        if (value == 0) {
            return [0]
        }

        def remain = value
        List<Integer> digits = []
        while (remain != 0) {
            digits << remain % outputBase
            remain = remain.intdiv(outputBase)
        }

        digits.reverse()
    }
}
