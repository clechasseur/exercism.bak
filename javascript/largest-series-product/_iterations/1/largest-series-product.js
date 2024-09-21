function* intRange(min, max) {
    for(let i = min; i <= max; i++) {
        yield i;
    }
}

export function largestProduct(digitsStr, size) {
    if (!/^\d*$/.test(digitsStr) || size < 0) {
        throw new Error('Invalid input.');
    }
    if (size > digitsStr.length) {
        throw new Error('Slice size is too big.');
    }
    if (size === 0) {
        return 1;
    }
    const digits = [...digitsStr].map((c) => Number(c));
    return [...intRange(0, digits.length - size)].map((i) => digits.slice(i, i + size).reduce((acc, i) => acc * i))
                                                 .reduce((prev, prod) => prev < prod ? prod : prev);
}
