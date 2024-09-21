/// Determine whether a sentence is a pangram.
pub fn is_pangram(sentence: &str) -> bool {
    let mut letters: Vec<_> = sentence.to_lowercase()
        .chars()
        .filter(|c| c.is_ascii_alphabetic())
        .collect();
    letters.sort();
    letters.dedup();
    letters.len() == 26
}
