//! Solution to the [`sum-of-multiples`](https://exercism.org/tracks/rust/exercises/sum-of-multiples) exercise of the Exercism `rust` track.
//!
//! # Approaches
//!
//! This submission tests two approaches:
//!
//! 1. Compute multiples of all `factors` that are below `limit`, deduplicate them and get the sum
//! 2. Iterate all numbers from 1 to `limit`, pick those that have a factor in `factors` and get the sum
//!
//! I've created a benchmark to try to determine which approach is best. The benchmark includes two of the tests that
//! are included with the exercise. For the benchmark code, see `benches/sum_of_multiples_bench.rs`.
//!
//! Results:
//!
//! ```text
//! from_factors: much_larger_factors
//!                         time:   [7.2049 µs 7.2478 µs 7.3010 µs]
//!
//! from_factors: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3
//!                         time:   [198.67 µs 199.60 µs 200.51 µs]
//!
//! from_range: much_larger_factors
//!                         time:   [54.956 µs 55.176 µs 55.423 µs]
//!
//! from_range: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3
//!                         time:   [60.713 µs 61.056 µs 61.488 µs]
//! ```
//!
//! Both tests have the same limit (10,000), so it seems that if you have many factors, it's quicker to scan the entire
//! range, while if you have few of them (or they are large enough not to produce too many multiples, I guess), it's
//! much faster to use the deduplicated multiples.
//!
//! # Nightly Rust and `partition_dedup`
//!
//! In Nightly Rust, we have access to [`slice::partition_dedup`]. This method partitions the slice in two, leaving
//! consecutive distinct elements in the first partition. When used on a sorted slice, it only keeps distinct elements.
//! Best of all, it works in-place so doesn't allocate any new memory.
//!
//! Using this method, we can speed up our implementation of the first approach by ~50%:
//!
//! ```text
//! from_factors: much_larger_factors
//!                         time:   [3.3717 µs 3.3833 µs 3.3983 µs]
//!                         change: [-52.969% -52.574% -52.079%] (p = 0.00 < 0.05)
//!                         Performance has improved.
//!
//! from_factors: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3
//!                         time:   [113.29 µs 113.65 µs 114.04 µs]
//!                         change: [-43.684% -43.332% -42.915%] (p = 0.00 < 0.05)
//!                         Performance has improved.
//! ```
//!
//! The second approach is still faster when you have a lot of small multiples, but it's interesting to see the
//! improvement with the new method.
//!
//! # `sort` vs `sort_unstable`
//!
//! In the Nightly Rust implementation, we need to sort the slice before calling [`slice::partition_dedup`]. For this,
//! we use [`slice::sort`] and _not_ [`slice::sort_unstable`]. Although the latter is often recommended when ordering
//! of duplicates does not matter, here we have a case where [`slice::sort`] actually outperforms: multiple _sorted_
//! sequences concatenated one after another. If we use [`slice::sort_unstable`] instead, we get performance that is
//! actually _worse_ than the stable Rust implementation:
//!
//! ```text
//! from_factors: much_larger_factors
//!                         time:   [4.8328 µs 4.8499 µs 4.8680 µs]
//!                         change: [+42.734% +43.330% +43.917%] (p = 0.00 < 0.05)
//!                         Performance has regressed.
//!
//! from_factors: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3
//!                         time:   [341.23 µs 345.11 µs 349.85 µs]
//!                         change: [+194.88% +198.21% +201.93%] (p = 0.00 < 0.05)
//!                         Performance has regressed.
//! ```

#![cfg_attr(nightly_rustc, feature(slice_partition_dedup))]

use std::iter::empty;

use either::Either;

#[inline(always)]
pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    sum_of_multiples_from_factors(limit, factors)
}

/// First approach (see [crate documentation](crate)).
pub fn sum_of_multiples_from_factors(limit: u32, factors: &[u32]) -> u32 {
    distinct_sum(factors.iter().flat_map(|&factor| multiples(limit, factor)))
}

/// Second approach (see [crate documentation](crate)).
pub fn sum_of_multiples_from_range(limit: u32, factors: &[u32]) -> u32 {
    (1..limit)
        .filter(|&n| factors.iter().any(|&f| f != 0 && n % f == 0))
        .sum()
}

fn multiples(limit: u32, factor: u32) -> impl Iterator<Item = u32> {
    match factor {
        0 => Either::Left(empty()),
        f => Either::Right((f..).step_by(f as usize).take_while(move |&n| n < limit)),
    }
}

fn distinct_sum<I>(values: I) -> u32
where
    I: IntoIterator<Item = u32>,
{
    // See crate comment for details on this dual implementation.

    #[cfg(not(nightly_rustc))]
    return values
        .into_iter()
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .sum();

    #[cfg(nightly_rustc)]
    {
        let mut values: Vec<_> = values.into_iter().collect();
        values.sort();
        let (distinct_values, _) = values.partition_dedup();
        distinct_values.iter().sum()
    }
}
