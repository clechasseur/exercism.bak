#[cfg(feature = "benchmarks")]
use criterion::{black_box, Criterion, criterion_group, criterion_main};
#[cfg(feature = "benchmarks")]
use difference_of_squares::{difference_classic, difference_smart};

#[cfg(feature = "benchmarks")]
fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("difference_of_squares_classic", |b| {
        b.iter(|| difference_classic(black_box(25_164_150)));
    });
    c.bench_function("difference_of_squares_smart", |b| {
        b.iter(|| difference_smart(black_box(25_164_150)));
    });
}

#[cfg(feature = "benchmarks")]
criterion_group!(benches, criterion_benchmark);
#[cfg(feature = "benchmarks")]
criterion_main!(benches);
