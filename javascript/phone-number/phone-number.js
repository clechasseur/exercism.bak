export class PhoneNumber {
    constructor(input) {
        const cleanedInput = input.replace(/[^0-9]/g, '');
        const match = /^1?([2-9][0-9]{2}[2-9][0-9]{6})$/.exec(cleanedInput);
        this.parsedNumber = match ? match[1] : null;
    }

    number() {
        return this.parsedNumber;
    }
}
