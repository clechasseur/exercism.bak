use std::collections::BTreeSet;
use std::iter::empty;

use either::Either;

// This submission tests two approaches:
//
// 1. Compute multiples of all `factors` that are below `limit`, deduplicate them and get the sum
// 2. Iterate all numbers from 1 to `limit`, pick those that have a factor in `factors` and get the sum
//
// I've created a benchmark to try to determine which approach is best. See the end of this file.

#[inline(always)]
pub fn sum_of_multiples(limit: u32, factors: &[u32]) -> u32 {
    sum_of_multiples_from_factors(limit, factors)
}

pub fn sum_of_multiples_from_factors(limit: u32, factors: &[u32]) -> u32 {
    factors
        .iter()
        .flat_map(|&factor| multiples(limit, factor))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .sum()
}

fn multiples(limit: u32, factor: u32) -> impl Iterator<Item = u32> {
    match factor {
        0 => Either::Left(empty()),
        f => Either::Right((f..).step_by(f as usize).take_while(move |&n| n < limit)),
    }
}

pub fn sum_of_multiples_from_range(limit: u32, factors: &[u32]) -> u32 {
    (1..limit)
        .filter(|&n| factors.iter().any(|&f| f != 0 && n % f == 0))
        .sum()
}

// The benchmark includes two of the tests that are included with the exercise.
// For the benchmark code, see [`benches/sum_of_multiples_bench.rs`].
//
// Results:
//
// ```
//      Running benches/sum_of_multiples_bench.rs (target/release/deps/sum_of_multiples_bench-fb21da7330e8be3a)
// Gnuplot not found, using plotters backend
// from_factors: much_larger_factors
//                         time:   [7.2049 µs 7.2478 µs 7.3010 µs]
//
// Benchmarking from_factors: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3: Collecting 100 samples in estimated 5.0581 s (25k iteratio
// from_factors: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3
//                         time:   [198.67 µs 199.60 µs 200.51 µs]
// Found 5 outliers among 100 measurements (5.00%)
//   1 (1.00%) low mild
//   3 (3.00%) high mild
//   1 (1.00%) high severe
//
// from_range: much_larger_factors
//                         time:   [54.956 µs 55.176 µs 55.423 µs]
// Found 10 outliers among 100 measurements (10.00%)
//  9 (9.00%) high mild
//  1 (1.00%) high severe
//
// Benchmarking from_range: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3: Collecting 100 samples in estimated 5.2653 s (86k iterations
// from_range: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3
//                         time:   [60.713 µs 61.056 µs 61.488 µs]
// Found 3 outliers among 100 measurements (3.00%)
//  3 (3.00%) high severe
// ```
//
// Both tests have the same limit (10_000), so it seems that if you have many factors, it's quicker to scan the entire
// range, while if you have few of them (or they are large enough not to produce too many multiples, I guess), it's
// much faster to use the deduplicated multiples.
