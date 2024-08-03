class ComplexNumber {
    private double real;
    private double imag;

    ComplexNumber(double real, double imag) {
        this.real = real;
        this.imag = imag;
    }

    double getReal() {
        return real;
    }

    double getImag() {
        return imag;
    }

    ComplexNumber conjugate() {
        return new ComplexNumber(real, -imag);
    }

    double abs() {
        return Math.sqrt(real * real + imag * imag);
    }

    ComplexNumber add(ComplexNumber other) {
        return new ComplexNumber(real + other.real, imag + other.imag);
    }

    ComplexNumber minus(ComplexNumber other) {
        return new ComplexNumber(real - other.real, imag - other.imag);
    }

    ComplexNumber times(ComplexNumber other) {
        double resReal = real * other.real - imag * other.imag;
        double resImag = imag * other.real + real * other.imag;
        return new ComplexNumber(resReal, resImag);
    }

    ComplexNumber div(ComplexNumber other) {
        double divisor = other.real * other.real + other.imag * other.imag;
        double resReal = (real * other.real + imag * other.imag) / divisor;
        double resImag = (imag * other.real - real * other.imag) / divisor;
        return new ComplexNumber(resReal, resImag);
    }

    ComplexNumber exponentialOf() {
        return new ComplexNumber(Math.exp(real), 0.0).times(new ComplexNumber(Math.cos(imag), Math.sin(imag)));
    }
}
