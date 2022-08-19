use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ludtwig_parser::parse;
use std::fs;

fn parsing_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex.html.twig")
        .expect("can't find fixtures/complex.html.twig in project folder");

    c.bench_function("parsing complex.html.twig", |b| {
        b.iter(|| parse(black_box(&input)))
    });
}

fn failing_parsing_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex-failing.html.twig")
        .expect("can't find fixtures/complex-failing.html.twig in project folder");

    c.bench_function("parsing complex-failing.html.twig", |b| {
        b.iter(|| parse(black_box(&input)))
    });

    let err = parse(&input).expect_err(
        "expected failed parsing for fixtures/complex-failing.html.twig in project folder",
    );

    c.bench_function("generating human error from failed parsing", |b| {
        b.iter(|| err.pretty_helpful_error_string(black_box(&input)))
    });
}

fn ast_basic_iteration_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex.html.twig")
        .expect("can't find fixtures/complex.html.twig in project folder");

    let ast = parse(&input).expect("can't parse complex.html.twig");

    c.bench_function("basic iteration over AST of complex.html.twig", |b| {
        b.iter(|| {
            for item in ast.iter() {
                black_box(item);
            }
        })
    });
}

fn ast_context_iteration_benchmark(c: &mut Criterion) {
    let input = fs::read_to_string("./fixtures/complex.html.twig")
        .expect("can't find fixtures/complex.html.twig in project folder");

    let ast = parse(&input).expect("can't parse complex.html.twig");

    c.bench_function("context iteration over AST of complex.html.twig", |b| {
        b.iter(|| {
            for item in ast.context_iter() {
                black_box(item);
            }
        })
    });
}

criterion_group!(
    benches,
    parsing_benchmark,
    failing_parsing_benchmark,
    ast_basic_iteration_benchmark,
    ast_context_iteration_benchmark
);
criterion_main!(benches);
