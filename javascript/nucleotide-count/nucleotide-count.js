export class NucleotideCounts {
    static parse(input) {
        const output = { A: 0, C: 0, G: 0, T: 0 };
        [...input].forEach(c => {
            if (!(c in output)) {
                throw new Error('Invalid nucleotide in strand');
            }
            output[c]++;
        });
        return Object.values(output).join(' ');
    }
}
