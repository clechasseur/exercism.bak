use std::collections::HashSet;
use std::iter::empty;

use either::Either;

pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    factors
        .iter()
        .flat_map(|&factor| multiples(limit, factor))
        .collect::<HashSet<_>>()
        .into_iter()
        .sum()
}

fn multiples(limit: u32, factor: u32) -> impl Iterator<Item = u32> {
    match factor {
        0 => Either::Left(empty()),
        f => Either::Right((f..).step_by(f as usize).take_while(move |&n| n < limit)),
    }
}
