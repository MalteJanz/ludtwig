use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ludtwig_parser::parse;
use std::fs;

fn parsing_synthetic_minimal_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/synthetic-minimal.html.twig")
        .expect("can't find fixtures/synthetic-minimal.html.twig in project folder");

    c.bench_function("parsing synthetic-minimal.html.twig", |b| {
        b.iter(|| {
            let result = parse(&input);
            black_box(result)
        })
    });
}

fn parsing_complex_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex.html.twig")
        .expect("can't find fixtures/complex.html.twig in project folder");

    c.bench_function("parsing complex.html.twig", |b| {
        b.iter(|| {
            let result = parse(&input);
            black_box(result)
        })
    });
}

fn parsing_complex_failing_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex-failing.html.twig")
        .expect("can't find fixtures/complex-failing.html.twig in project folder");

    c.bench_function("parsing complex-failing.html.twig", |b| {
        b.iter(|| {
            let result = parse(&input);
            black_box(result)
        })
    });
}

criterion_group!(
    benches,
    parsing_synthetic_minimal_benchmark,
    parsing_complex_benchmark,
    parsing_complex_failing_benchmark
);
criterion_main!(benches);
