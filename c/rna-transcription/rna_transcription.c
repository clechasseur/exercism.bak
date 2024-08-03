#include "rna_transcription.h"

#include <stdlib.h>
#include <string.h>

static char translate_nucleotide(char nucleotide)
{
    switch (nucleotide) {
        case 'G':
            return 'C';
        case 'C':
            return 'G';
        case 'T':
            return 'A';
        case 'A':
            return 'U';
        default:
            return '\0';
    }
}

char *to_rna(const char *dna)
{
    char *rna = (char *) malloc(strlen(dna) + 1);

    char *nucleotide = rna;
    for (; *dna != '\0'; ++dna) {
        *nucleotide++ = translate_nucleotide(*dna);
    }
    *nucleotide = '\0';

    return rna;
}
