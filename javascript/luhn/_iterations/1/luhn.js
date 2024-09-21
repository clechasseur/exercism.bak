export class Luhn {
    constructor(input) {
        const cleanedInput = input.replace(/\s+/g, '');
        if (cleanedInput.length <= 1 || cleanedInput.match(/[^\d]/) !== null) {
            this.valid = false;
        } else {
            this.valid = [...cleanedInput].reverse()
                                          .map((c) => Number(c))
                                          .map((i, idx) => idx % 2 != 0 ? i * 2 : i)
                                          .map((i) => i > 9 ? i - 9 : i)
                                          .reduce((acc, i) => acc + i) % 10 == 0;
        }
    }
}
