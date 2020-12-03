use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::fs;
use twig::parse;

fn parsing_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex.html.twig")
        .expect("can't find fixtures/complex.html.twig in project folder");

    c.bench_function("parsing complex.html.twig", |b| {
        b.iter(|| parse(black_box(&input)))
    });
}

criterion_group!(benches, parsing_benchmark);
criterion_main!(benches);
