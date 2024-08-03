pub fn abbreviate(phrase: &str) -> String {
    // This kind of problem is often solved using regular expressions.
    // However, Rust does not have regex support in its standard library.
    // The `regex` crate does provide it and is pretty ubiquitous, but
    // let's see if we can do this without regexes for a change.

    // The algorithm used is as follows:
    // 1. Find all acronym letters
    //   a. A character is an acronym letter if it's alphabetic and
    //     i. it's preceded by a non-"word" character (*), or
    //    ii. it's uppercase and is preceded by a lowercase "word" character (*)
    // 2. Convert all acronym letters to uppercase
    // 3. Collect them all into a `String`
    //
    // (*): a "word" character is defined as an alphanumeric character OR a quote.
    //
    // To simplify things, we won't handle Unicode graphemes and assume the strings
    // only contain ASCII characters.

    fn is_word_char(c: char) -> bool {
        c.is_alphanumeric() || c == '\''
    }

    let mut result = String::with_capacity(phrase.len());

    let mut prev_c = '\0';
    for c in phrase.chars() {
        if c.is_alphanumeric()
            && (!is_word_char(prev_c) || (c.is_ascii_uppercase() && prev_c.is_ascii_lowercase()))
        {
            result.push(c.to_ascii_uppercase());
        }

        prev_c = c;
    }

    result
}

// This is a version of the function above, but using iterators instead of a `for` loop.
// It was my first implementation. Benchmarks show it's a little slower (maybe about 15-20%).
// (I've included the benchmarks with the submission.)
pub fn abbreviate_with_iterators(phrase: &str) -> String {
    use std::iter;

    fn is_word_char(c: char) -> bool {
        c.is_alphanumeric() || c == '\''
    }

    phrase
        .chars()
        .zip(iter::once('\0').chain(phrase.chars()))
        .filter(|&(c, prev_c)| {
            c.is_alphabetic()
                && (!is_word_char(prev_c)
                    || (c.is_ascii_uppercase() && prev_c.is_ascii_lowercase()))
        })
        .map(|(c, _)| c.to_ascii_uppercase())
        .collect()
}
