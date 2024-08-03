export class ComplexNumber {
    constructor(real = 0, imag = 0) {
        this.real = real;
        this.imag = imag;
    }

    add(other) {
        return new ComplexNumber(this.real + other.real, this.imag + other.imag);
    }

    sub(other) {
        return new ComplexNumber(this.real - other.real, this.imag - other.imag);
    }

    mul(other) {
        return new ComplexNumber(this.real * other.real - this.imag * other.imag,
                                 this.imag * other.real + this.real * other.imag);
    }

    div(other) {
        return new ComplexNumber((this.real * other.real + this.imag * other.imag) / (other.real**2 + other.imag**2),
                                 (this.imag * other.real - this.real * other.imag) / (other.real**2 + other.imag**2));
    }

    get abs() {
        return Math.sqrt(this.real**2 + this.imag**2);
    }

    get conj() {
        return new ComplexNumber(this.real, this.imag != 0 ? -this.imag : 0);
    }

    get exp() {
        return new ComplexNumber(Math.exp(this.real)).mul(new ComplexNumber(Math.cos(this.imag), Math.sin(this.imag)));
    }
}
