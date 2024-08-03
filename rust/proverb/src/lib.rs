use std::iter::{once, zip};

pub fn build_proverb(list: &[&str]) -> String {
    zip(list, list.iter().skip(1))
        .map(|(&want, &lost)| format!("For want of a {want} the {lost} was lost.\n"))
        .chain(once(
            list.first()
                .map(|&first| format!("And all for the want of a {first}."))
                .unwrap_or_default(),
        ))
        .collect()
}
