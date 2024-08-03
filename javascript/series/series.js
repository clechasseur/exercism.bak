function* intRange(min, max) {
    for (let i = min; i <= max; i++) {
        yield i;
    }
}

export class Series {
    constructor(digitsStr) {
        this.digits = [...digitsStr].map((c) => Number(c));
    }

    slices(size) {
        if (this.digits.length < size) {
            throw new Error('Slice size is too big.');
        }
        return [...intRange(0, this.digits.length - size)].map((i) => this.digits.slice(i, i + size));
    }
}
