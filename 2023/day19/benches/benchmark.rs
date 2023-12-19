use criterion::{black_box, criterion_group, criterion_main, Criterion};
use day19::{part1, part2};

pub fn criterion_benchmark1(c: &mut Criterion) {
    let s = include_str!("../input/input.txt");
    c.bench_function("part1", |b| b.iter(|| part1(black_box(s))));
}
pub fn criterion_benchmark2(c: &mut Criterion) {
    let s = include_str!("../input/input.txt");
    c.bench_function("part2", |b| b.iter(|| part2(black_box(s))));
}

criterion_group!(benches, criterion_benchmark1, criterion_benchmark2);
criterion_main!(benches);
