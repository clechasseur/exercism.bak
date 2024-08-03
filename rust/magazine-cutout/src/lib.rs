use std::collections::hash_map::Entry;
use std::collections::HashMap;

pub fn can_construct_note(magazine: &[&str], note: &[&str]) -> bool {
    let mut word_counts = HashMap::new();
    for &word in note {
        *word_counts.entry(word).or_insert(0) += 1;
    }
    for &word in magazine {
        if let Entry::Occupied(o) = word_counts.entry(word).and_modify(|count| *count -= 1) {
            if *o.get() == 0 {
                o.remove();
                if word_counts.is_empty() {
                    return true;
                }
            }
        }
    }
    word_counts.is_empty()
}
