use std::convert::identity;

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
    // If you work hard enough, you too can avoid regular loops and `if`s! Apply today.
    candidate
        .chars()
        .filter(|c| c.is_alphabetic())
        .map(|c| 1usize << (c.to_ascii_lowercase() as usize - 'a' as usize))
        .scan(0usize, |acc, mask| {
            let valid = *acc & mask == 0;
            *acc |= mask;
            Some(valid)
        })
        .all(identity)
}
