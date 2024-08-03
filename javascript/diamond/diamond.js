export class Diamond {
    constructor() {
        this.letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    }

    *intSeq(from, to, step) {
        for (let i = from; (step > 0 ? i <= to : i >= to); i += step) {
            yield i;
        }
    }

    makeLine(n, size) {
        const inside = ' '.repeat(2 * n - 1);
        const outside = ' '.repeat(size - n);
        return `${outside}${this.letters[n]}${inside}${this.letters[n]}${outside}`;
    }

    makeDiamond(letter) {
        const size = this.letters.indexOf(letter);
        if (size === 0) {
            return 'A\n';
        } else {
            const aLine = `${' '.repeat(size)}A${' '.repeat(size)}`;
            return `${aLine}
${[...this.intSeq(1, size, 1)].concat([...this.intSeq(size - 1, 1, -1)]).map((i) => this.makeLine(i, size)).join('\n')}
${aLine}
`;
        }
    }
}
