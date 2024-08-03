import BigInt from './lib/big-integer';

export class Grains {
    square(n) {
        let i = BigInt(1);
        while (n-- > 1) {
            i = i.multiply(2);
        }
        return i.toString();
    }

    total() {
        return BigInt(this.square(65)).minus(1).toString();
    }
}
