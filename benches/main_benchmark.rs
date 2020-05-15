use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ecology::simulate;

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("simulate 20", |b| b.iter(|| simulate(black_box(35), false)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
