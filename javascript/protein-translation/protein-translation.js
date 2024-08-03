const CODONS = new Map([
    ['AUG', 'Methionine'],
    ['UUU', 'Phenylalanine'],
    ['UUC', 'Phenylalanine'],
    ['UUA', 'Leucine'],
    ['UUG', 'Leucine'],
    ['UCU', 'Serine'],
    ['UCC', 'Serine'],
    ['UCA', 'Serine'],
    ['UCG', 'Serine'],
    ['UAU', 'Tyrosine'],
    ['UAC', 'Tyrosine'],
    ['UGU', 'Cysteine'],
    ['UGC', 'Cysteine'],
    ['UGG', 'Tryptophan'],
    ['UAA', 'STOP'],
    ['UAG', 'STOP'],
    ['UGA', 'STOP']
]);

export function translate(codons = '') {
    let proteins = [];
    let left = codons;
    while (left.length != 0) {
        let codon = left.slice(0, 3);
        left = left.slice(3);
        let protein = CODONS.get(codon);
        if (protein == undefined) {
            throw new Error('Invalid codon');
        }
        if (protein == 'STOP') {
            break;
        }
        proteins.push(protein);
    }
    return proteins;
}
