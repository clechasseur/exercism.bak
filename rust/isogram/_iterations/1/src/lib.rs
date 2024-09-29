/// Checks if the given candidate is an isogram.
///
/// An isogram is a word or phrase without repeated letters.
/// Hyphens and whitespace are allowed and can be repeated, however.
///
/// # Notes
///
/// [`itertools`] has a nice helper that can be used to solve this easily: [`all_unique`].
/// I won't use it here just so I can ~~show off~~ try manually.
///
/// [`itertools`]: https://docs.rs/itertools/latest/itertools
/// [`all_unique`]: https://docs.rs/itertools/latest/itertools/trait.Itertools.html#method.all_unique
pub fn check(candidate: &str) -> bool {
    // 26 letters ought to be enough for everybody
    let mut letters = 0usize;
    for c in candidate.chars().filter(|c| c.is_alphabetic()) {
        let letter_mask = 1 << (c.to_ascii_lowercase() as usize - 'a' as usize);
        if letters & letter_mask != 0 {
            return false;
        }
        letters |= letter_mask;
    }

    true
}
