const dnaToRna: Record<string, string> = {
  'G': 'C',
  'C': 'G',
  'T': 'A',
  'A': 'U',
};

function toComplement(nucleotide: string): string {
  const complement = dnaToRna[nucleotide];
  if (!complement) {
    throw new Error('Invalid input DNA.');
  }
  return complement;
}

export function toRna(dna: string): string {
  return dna.split('').map((nucleotide) => toComplement(nucleotide)).join('');
}
