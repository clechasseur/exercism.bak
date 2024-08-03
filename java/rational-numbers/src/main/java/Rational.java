import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

class Rational {
    private int numerator;
    private int denominator;

    Rational(int numerator, int denominator) {
        this.numerator = numerator;
        this.denominator = denominator;
        reduce();
    }

    int getNumerator() {
        return numerator;
    }

    int getDenominator() {
        return denominator;
    }

    Rational abs() {
        return new Rational(Math.abs(numerator), Math.abs(denominator));
    }

    Rational add(Rational other) {
        int newNumerator = numerator * other.denominator + other.numerator * denominator;
        int newDenominator = denominator * other.denominator;
        return new Rational(newNumerator, newDenominator);
    }

    Rational subtract(Rational other) {
        return add(new Rational(-other.numerator, other.denominator));
    }

    Rational multiply(Rational other) {
        return new Rational(numerator * other.numerator, denominator * other.denominator);
    }

    Rational divide(Rational other) {
        int newNumerator = numerator * other.denominator;
        int newDenominator = other.numerator * denominator;
        if (newDenominator == 0) {
            throw new IllegalArgumentException("Divide by zero");
        }
        return new Rational(newNumerator, newDenominator);
    }

    Rational pow(int n) {
        int absn = Math.abs(n);
        return new Rational((int) Math.pow(numerator, absn), (int) Math.pow(denominator, absn));
    }

    double exp(double x) {
        double xpown = Math.pow(x, numerator);
        if (denominator == 2) {
            return Math.sqrt(xpown);
        } else if (denominator == 3) {
            return Math.cbrt(xpown);
        }
        return Math.pow(xpown, 1.0 / denominator);
    }

    @Override
    public String toString() {
        return String.format("%d/%d", this.getNumerator(), this.getDenominator());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || !this.getClass().isAssignableFrom(obj.getClass())) {
            return false;
        }

        Rational other = (Rational) obj;
        return this.getNumerator() == other.getNumerator()
            && this.getDenominator() == other.getDenominator();
    }

    @Override
    public int hashCode() {
        int prime = 31;
        int result = 1;
        result = prime * result + this.getNumerator();
        result = prime * result + this.getDenominator();

        return result;
    }

    private void reduce() {
        if (numerator != 0) {
            Set<Integer> commonFactorsSet = factors(Math.abs(numerator));
            commonFactorsSet.retainAll(factors(Math.abs(denominator)));
            Integer[] commonFactors = commonFactorsSet.toArray(new Integer[0]);
            int gcd = commonFactors[commonFactors.length - 1];
            numerator /= gcd;
            denominator /= gcd;
            if (denominator < 0) {
                numerator = -numerator;
                denominator = -denominator;
            }
        } else {
            denominator = 1;
        }
    }

    private static Set<Integer> factors(int n) {
        return IntStream.iterate(1, i -> i <= n, i -> i + 1)
                .filter(i -> n % i == 0)
                .collect(TreeSet::new, TreeSet::add, (s1, s2) -> new TreeSet<>(
                        Stream.concat(s1.stream(), s2.stream()).collect(Collectors.toList())));
    }
}
