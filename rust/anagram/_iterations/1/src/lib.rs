use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let word = word.to_lowercase();
    let normalized_word = normalized(&word);
    possible_anagrams.iter()
        .map(|&pa| (pa, pa.to_lowercase()))
        .filter(|(_, lpa)| lpa != &word)
        .filter(|(_, lpa)| normalized(&lpa) == normalized_word)
        .map(|(pa, _)| pa)
        .collect()
}

fn normalized(word: &str) -> String {
    let mut chars: Vec<_> = word.chars().collect();
    chars.sort();
    String::from_iter(chars)
}
