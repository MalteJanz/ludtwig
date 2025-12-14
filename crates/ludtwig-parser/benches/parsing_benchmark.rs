use criterion::{Criterion, criterion_group, criterion_main};
use ludtwig_parser::parse;
use std::fs;
use std::hint::black_box;

fn parsing_complex_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("../../fixtures/complex.html.twig")
        .expect("can't find fixtures/complex.html.twig in project folder");

    c.bench_function("parsing complex.html.twig", |b| {
        b.iter(|| {
            let result = parse(&input);
            black_box(result)
        })
    });
}

fn parsing_complex_failing_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("../../fixtures/complex-syntax-error.html.twig")
        .expect("can't find fixtures/complex-syntax-error.html.twig in project folder");

    c.bench_function("parsing complex-syntax-error.html.twig", |b| {
        b.iter(|| {
            let result = parse(&input);
            black_box(result)
        })
    });
}

criterion_group!(
    benches,
    parsing_complex_benchmark,
    parsing_complex_failing_benchmark
);
criterion_main!(benches);
