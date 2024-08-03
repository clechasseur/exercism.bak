use acronym::{abbreviate, abbreviate_with_iterators};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};

const PHRASES: &[&str] = &[
    "Portable Network Graphics",
    "Ruby on Rails",
    "First In, First Out",
    "GNU Image Manipulation Program",
    "Complementary metal-oxide semiconductor",
    "Rolling On The Floor Laughing So Hard That My Dogs Came Over And Licked Me",
    "Something - I made up from thin air",
    "Halley's Comet",
    "The Road _Not_ Taken",
    "HyperText Markup Language",
];

fn bench_abbreviate(c: &mut Criterion) {
    let mut group = c.benchmark_group("abbreviate");
    for &phrase in PHRASES {
        group.bench_with_input(BenchmarkId::new("abbreviate", phrase), phrase, |b, p| {
            b.iter(|| abbreviate(p))
        });
        group.bench_with_input(
            BenchmarkId::new("abbreviate_with_iterators", phrase),
            phrase,
            |b, p| b.iter(|| abbreviate_with_iterators(p)),
        );
    }
    group.finish();
}

criterion_group!(benches, bench_abbreviate);
criterion_main!(benches);
