use std::collections::HashMap;

fn get_word_counts<'a>(words: &'a [&str]) -> HashMap<&'a str, u32> {
    let mut word_counts = HashMap::new();
    for &word in words {
        word_counts.entry(word).and_modify(|count| *count += 1).or_insert(1);
    }
    word_counts
}

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let magazine_word_counts = get_word_counts(magazine);
    get_word_counts(note).iter().all(|(&word, count)| {
        magazine_word_counts.get(word).unwrap_or(&0) >= count
    })
}
