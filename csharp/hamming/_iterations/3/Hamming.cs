using System;
using System.Linq;

public static class Hamming
{
    public static int Distance(string firstStrand, string secondStrand)
    {
        if (firstStrand.Length != secondStrand.Length) {
            throw new ArgumentException("Both strands need to have equal length");
        }

        return firstStrand.Zip(secondStrand)
            .Count((nucleotides) => nucleotides.First !=  nucleotides.Second);
    }
}
