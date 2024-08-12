export class Say {
    constructor() {
        this.smallNums = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine',
                          'ten', 'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'];
        this.largeNums = ['twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'];
        this.scale = ['', 'thousand', 'million', 'billion'];
    }

    block(num, explicitZero = false) {
        if (num === 0) {
            if (explicitZero) {
                return this.smallNums[0];
            } else {
                return '';
            }
        } else {
            const hundreds = Math.floor(num / 100);
            let output = (hundreds !== 0) ? `${this.smallNums[hundreds]} hundred ` : '';
            const tens = Math.floor((num % 100) / 10);
            if (tens <= 1) {
                if ((num % 100) !== 0) {
                    output = `${output}${this.smallNums[num % 100]}`;
                }
            } else {
                output = `${output}${this.largeNums[tens - 2]}`;
                const remain = (num % 100) - (tens * 10);
                if (remain !== 0) {
                    output = `${output}-${this.smallNums[remain]}`;
                }
            }
            return output.trim();
        }
    }

    blockForScale(num, scale, explicitZero = false) {
        const scaled = 1000 ** scale;
        const block = this.block(Math.floor(num / scaled) % 1000, explicitZero);
        return `${block} ${block.length !== 0 ? this.scale[scale] : ''}`.trim();
    }

    inEnglish(num) {
        if (num < 0 || num > 999999999999) {
            throw new Error('Number must be between 0 and 999,999,999,999.');
        }
        return [3, 2, 1, 0].map((scale) => this.blockForScale(num, scale, scale === 0 && num === 0))
                           .filter((block) => block.length !== 0)
                           .join(' ')
                           .trim();
    }
}
