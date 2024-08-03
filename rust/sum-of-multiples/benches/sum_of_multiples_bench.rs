//! To run this benchmark, uncomment all lines in `Cargo.toml` and run
//!
//! ```sh
//! cargo bench
//! ```

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sum_of_multiples::{sum_of_multiples_from_factors, sum_of_multiples_from_range};

fn from_factors_benchmark(c: &mut Criterion) {
    c.bench_function("from_factors: much_larger_factors", |b| {
        b.iter(|| sum_of_multiples_from_factors(black_box(10000), black_box(&[43, 47])))
    });
    c.bench_function(
        "from_factors: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3",
        |b| {
            b.iter(|| sum_of_multiples_from_factors(black_box(10000), black_box(&[2, 3, 5, 7, 11])))
        },
    );
}

fn from_range_benchmark(c: &mut Criterion) {
    c.bench_function("from_range: much_larger_factors", |b| {
        b.iter(|| sum_of_multiples_from_range(black_box(10000), black_box(&[43, 47])))
    });
    c.bench_function(
        "from_range: solutions_using_include_exclude_must_extend_to_cardinality_greater_than_3",
        |b| b.iter(|| sum_of_multiples_from_range(black_box(10000), black_box(&[2, 3, 5, 7, 11]))),
    );
}

criterion_group!(benches, from_factors_benchmark, from_range_benchmark);
criterion_main!(benches);
