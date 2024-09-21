// Returns the number of bits set in the given number.
export function popCount(n) {
    let pop = 0;

    while (n !== 0) {
        if ((n & 1) !== 0) {
            ++pop;
        }
        n >>= 1;
    }

    return pop;
}

// Returns the least-significant bit from the given number.
// If the number has no bit set, returns 0.
//
// Note: the return value is the bit, not its index. For example:
//
// ```js
// let n = 0b0110;
// const bit = firstBit(n); // bit === 0b0010
// n &= ~bit; // n === 0b0100
// ```
export function firstBit(n) {
    if (n === 0) {
        return 0;
    }

    let bit = 1;
    while ((n & bit) === 0) {
        bit <<= 1;
    }

    return bit;
}
