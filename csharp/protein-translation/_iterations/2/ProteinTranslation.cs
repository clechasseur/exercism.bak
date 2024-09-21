using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

public static class ProteinTranslation
{
    private static readonly Dictionary<string, List<string>> ProteinToCodons = new()
    {
        { "Methionine",    new() { "AUG" } },
        { "Phenylalanine", new() { "UUU", "UUC" } },
        { "Leucine",       new() { "UUA", "UUG" } },
        { "Serine",        new() { "UCU", "UCC", "UCA", "UCG" } },
        { "Tyrosine",      new() { "UAU", "UAC" } },
        { "Cysteine",      new() { "UGU", "UGC" } },
        { "Tryptophan",    new() { "UGG" } },
        { "STOP",          new() { "UAA", "UAG", "UGA" } },
    };

    private static readonly ImmutableDictionary<string, string> CodonToProtein =
        ProteinToCodons.SelectMany(ptoc => MakeCodonProteinPairs(ptoc.Key, ptoc.Value))
                       .ToImmutableDictionary();

    public static string[] Proteins(string strand) =>
        strand.Chunked(3)
              .Select(codon => CodonToProtein[codon])
              .TakeWhile(protein => protein != "STOP")
              .ToArray();

    private static IEnumerable<string> Chunked(this string input, int size)
    {
        for (var i = 0; i < input.Length; i += size)
        {
            yield return input.Substring(i, size);
        }
    }

    private static IEnumerable<KeyValuePair<string, string>> MakeCodonProteinPairs(string protein, IEnumerable<string> codons) =>
        codons.Select(c => new KeyValuePair<string, string>(c, protein));
}
