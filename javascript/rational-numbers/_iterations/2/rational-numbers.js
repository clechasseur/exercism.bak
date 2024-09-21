export class Rational {
    constructor(num, den) {
        if (den === 0) {
            throw new Error('Denominator cannot be zero');
        }
        this.num = den > 0 ? num : -num;
        this.den = Math.abs(den);
        this.simplify();
    }

    add(n) {
        return new Rational(this.num * n.den + n.num * this.den, this.den * n.den);
    }

    sub(n) {
        return new Rational(this.num * n.den - n.num * this.den, this.den * n.den);
    }

    mul(n) {
        return new Rational(this.num * n.num, this.den * n.den);
    }

    div(n) {
        return new Rational(this.num * n.den, n.num * this.den);
    }

    abs() {
        return new Rational(Math.abs(this.num), Math.abs(this.den));
    }

    exprational(n) {
        if (n >= 0) {
            return new Rational(this.num ** n, this.den ** n);
        } else {
            const m = Math.abs(n);
            return new Rational(this.den ** m, this.num ** m);
        }
    }

    expreal(n) {
        // This should work but we get precision issues.
        //return Math.pow(n ** this.num, 1 / this.den);
        // This works, I took it from VicenteFreire's solution (thanks!)
        return 10.0 ** (Math.log10(n ** this.num) / this.den);
    }

    reduce() {
        return this;
    }

    simplify() {
        if (this.num === 0) {
            this.den = 1;
        } else if (this.num === this.den) {
            this.num = 1;
            this.den = 1;
        } else {
            for (let gcd = 2; gcd <= Math.abs(this.num) && gcd <= Math.abs(this.den); gcd++) {
                if (this.num % gcd === 0 && this.den % gcd === 0) {
                    this.num = this.num / gcd;
                    this.den = this.den / gcd;
                    break;
                }
            }
        }
    }
}
