use criterion::{black_box, criterion_group, criterion_main, Criterion};
use day16::part2;

pub fn criterion_benchmark(c: &mut Criterion) {
    let s = include_str!("../input/input.txt");
    c.bench_function("part2", |b| b.iter(|| part2(black_box(s))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
