use bit_set::BitSet;

const NUM_LETTERS_IN_ALPHABET: usize = 26;

/// Determine whether a sentence is a pangram.
pub fn is_pangram(sentence: &str) -> bool {
    let mut letters = BitSet::with_capacity(NUM_LETTERS_IN_ALPHABET);
    
    for ch in sentence.chars().filter(char::is_ascii_alphabetic) {
        letters.insert(ch.to_ascii_lowercase() as usize - 'a' as usize);
    }
    
    letters.len() == NUM_LETTERS_IN_ALPHABET
}
