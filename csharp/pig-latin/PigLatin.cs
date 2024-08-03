using System;
using System.Text.RegularExpressions;

public static partial class PigLatin
{
    private static readonly Regex PigLatinRegex = CreatePigLatinRegex();
    private const string PigLatinReplacement = "$2$1ay";

    public static string Translate(string word) => PigLatinRegex.Replace(word, PigLatinReplacement);

    private static Regex CreatePigLatinRegex() => new(
        @"
            # Pig Latin has four rules, but actually there are only two states:
            # either the word begins with 'consonants' or it begins with 'vowels'.
            # If it begins with 'vowels', you just add 'ay' at the end.
            # If it begins with 'consonants', you move them to the end before adding 'ay'.
            #
            # So basically we simply need to identify what Pig Latin considers a group of
            # 'consonants'. So what does the rules tell us about this?
            #
            # * Problem statement tells us that vowels are 'a', 'e', 'i', 'o' and 'u'.
            # * Rule #1 tells us that 'xr' or 'yt' at the beginning of a word are considered 'vowels'.
            # * Rule #3 tells us that 'qu' are considered 'consonants' when at the beginning of a word
            #   (optionally preceded by zero or more real consonants).
            # * Rule #4 tells us that if the word begins with consonants followed by 'y',
            #   that 'y' is considered a 'vowel'.
            #
            # Armed with this knowledge, we can craft a single regex that captures any
            # consonant at the beginning of the word and moves them to the end before adding 'ay'.

            # First, anchor at word boundaries so our regex applies to each word in the sentence.
            \b

            # Next, handle one of two possible type of 'consonant' groups (handles rule #2):
            (
                # #1: zero or more consonant _except_ 'q', followed by 'qu' (handles rule #3)
                (?:[^aeiouq\s]*qu)

                # OR
                |

                # #2: one consonant, followed by zero or more consonant _except_ 'y' (handles rule #4)
                #     _as long as_ all those consonants are not preceded by 'xr' or 'yt' (handles rule #1)
                (?:
                    (?!xr|yt) # This is what is called a 'zero-width negative lookahead'.
                              # It ensures that the match does not start with 'xr' or 'yt'.
                    [^aeiou\s][^aeiouy\s]*
                )?

                # You might have noticed a '?' at the end of the last expression.
                # This allows the expression to match _zero consonant_ and will allow
                # us to use the same regex for words beginning with 'vowels'.
            )

            # Everything else after that first match is the 'rest' of the word.
            (\S+)

            # One final word boundary anchor and we're good. Also note that all
            # character groups used in the regex exclude whitespace - this is so we do
            # not accidentally match two or more words at the same time.
            \b

            # When this regex matches a word, the first group ($1) matches any consonant
            # at the beginning of the word (which might be zero consonant) and the second
            # group ($2) matches the rest of the word. So to perform a replace that converts
            # the word to Pig Latin, we can use the replacement expression '$2$1ay'.
            # If the word begins with 'vowels', $2 will be empty and we'll just add 'ay' at the end.
        ",
        RegexOptions.Compiled | RegexOptions.IgnorePatternWhitespace
    );

    // Note: if you run this locally, you can replace the `CreatePigLatinRegex` implementation above
    // with the one below that uses `GeneratedRegexAttribute` to generate the regex parsing code
    // at compile time (instead of at runtime).
    // This doesn't work on the Exercism test runner though, so I commented it out.

    //[GeneratedRegex(@"\b((?:[^aeiouq\s]*qu)|(?:(?!xr|yt)[^aeiou\s][^aeiouy\s]*)?)(\S+)\b")]
    //private static partial Regex CreatePigLatinRegex();
}
