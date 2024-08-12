class RaindropConverter {
    String convert(int number) {
        return new Builder(number).appendIfFactor(3, "Pling")
                                  .appendIfFactor(5, "Plang")
                                  .appendIfFactor(7, "Plong")
                                  .build();
    }

    private static final class Builder {
        private final int number;
        private final StringBuilder sb = new StringBuilder();

        Builder(int number) {
            this.number = number;
        }

        public Builder appendIfFactor(int factor, String toAppend) {
            if (number % factor == 0) {
                sb.append(toAppend);
            }
            return this;
        }

        public String build() {
            if (sb.length() == 0) {
                sb.append(number);
            }
            return sb.toString();
        }
    }
}
