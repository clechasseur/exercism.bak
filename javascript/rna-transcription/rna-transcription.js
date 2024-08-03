const TRANSCRIPTION = {
    'C': 'G',
    'G': 'C',
    'T': 'A',
    'A': 'U'
};

export const toRna = (dna) => Array.from(dna).map((nucleotide) => TRANSCRIPTION[nucleotide]).join('');
