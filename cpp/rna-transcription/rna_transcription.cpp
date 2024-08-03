#include "rna_transcription.h"
#include <stdexcept>
#include <algorithm>

namespace rna_transcription {
std::string to_rna(const char dna)
{
    return to_rna(std::string {dna});
}

std::string to_rna(const std::string& dna)
{
    std::string rna_result(dna);
    std::transform(rna_result.begin(), rna_result.end(), rna_result.begin(), map_dna_rna);
    return rna_result;
}

char map_dna_rna(const char dna)
{
    switch(dna)
    {
    case 'G':
        return 'C';
    case 'C':
        return 'G'; 
    case 'T':
        return 'A';
    case 'A':
        return 'U'; 
    default:
        throw std::invalid_argument("DNA letter must be GCTA only");
    }
}
}  // namespace  rna_transcription