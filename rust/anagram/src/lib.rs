use std::collections::HashSet;

pub fn anagrams_for<'a>(word: &str, possible_anagrams: &[&'a str]) -> HashSet<&'a str> {
    let word = word.to_uppercase();
    let normalized_word = normalized(&word);
    possible_anagrams.iter()
        .cloned()
        .filter(|&pa| {
            let pa = pa.to_uppercase();
            pa != word && normalized(&pa) == normalized_word
        })
        .collect()
}

fn normalized(word: &str) -> Vec<char> {
    let mut chars: Vec<_> = word.chars().collect();
    chars.sort_unstable();
    chars
}
