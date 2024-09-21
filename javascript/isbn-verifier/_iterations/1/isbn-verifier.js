export class ISBN {
    constructor(isbn) {
        const cleanedIsbn = isbn.replace(/-/g, '');
        this.valid = /^[0-9]{9}[0-9X]$/g.test(cleanedIsbn) &&
                     cleanedIsbn.split('')
                                .map((s) => s === 'X' ? 10 : Number(s))
                                .reduce((acc, x, idx) => acc + (x * (10 - idx)), 0) % 11 === 0;
    }

    isValid() {
        return this.valid;
    }
}
