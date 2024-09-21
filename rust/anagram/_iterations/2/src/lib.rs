use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let word = word.to_lowercase();
    let normalized_word = normalized(&word);
    possible_anagrams.iter()
        .filter_map(|&pa| match pa.to_lowercase() {
            lpa if lpa != word && normalized(&lpa) == normalized_word => Some(pa),
            _ => None,
        })
        .collect()
}

fn normalized(word: &str) -> Vec<char> {
    let mut chars: Vec<_> = word.chars().collect();
    chars.sort_unstable();
    chars
}
