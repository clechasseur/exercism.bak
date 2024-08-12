export function convert(digits, fromBase, toBase) {
    if (!fromBase || fromBase < 2 || !Number.isInteger(fromBase)) {
        throw new Error('Wrong input base');
    }
    if (!toBase || toBase < 2 || !Number.isInteger(toBase)) {
        throw new Error('Wrong output base');
    }
    const hasInvalidDigit = () => digits.some((digit) => digit < 0 || digit >= fromBase);
    if (digits.length == 0 || (digits[0] == 0 && digits.length > 1) || hasInvalidDigit()) {
        throw new Error('Input has wrong format');
    }
    let intermediate = Array.from(digits).reverse()
                                         .map((digit, idx) => digit * (fromBase ** idx))
                                         .reduce((prev, cur) => prev + cur, 0);
    let outDigits = [];
    do {
        outDigits.push(intermediate % toBase);
        intermediate = Math.floor(intermediate / toBase);
    } while (intermediate > 0);
    return outDigits.reverse();
}
