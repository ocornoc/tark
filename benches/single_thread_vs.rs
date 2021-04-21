use std::rc::{Rc, Weak as WeakRc};
use std::sync::{Arc, Weak as WeakArc};
use tark::{Tark, WeakTark};
use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize, Throughput};

fn new_bench(c: &mut Criterion) {
    const AMORTIZED: u64 = 1000;
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::new(), once");
    group.throughput(Throughput::Elements(1));
    group.bench_with_input("Rc", &(), |b, _| {
        b.iter(|| black_box(Rc::new(black_box(0))));
    });
    group.bench_with_input("Arc", &(), |b, _| {
        b.iter(|| black_box(Arc::new(black_box(0))));
    });
    group.bench_with_input("Tark", &(), |b, _| {
        b.iter(|| black_box(Tark::new(black_box(0))));
    });
    group.finish();

    let mut group = c.benchmark_group("[Rc/Arc/Tark]::new(), amortized");
    let ns = (0..AMORTIZED).into_iter().collect::<Vec<_>>();
    group.throughput(Throughput::Elements(AMORTIZED));
    group.bench_with_input("Rc", &ns, |b, ns| {
        b.iter_batched(
            || ns.clone(),
            |vs| black_box(vs.into_iter().map(Rc::new).collect::<Vec<_>>()),
            BatchSize::PerIteration,
        )
    });
    group.bench_with_input("Arc", &ns, |b, ns| {
        b.iter_batched(
            || ns.clone(),
            |vs| black_box(vs.into_iter().map(Arc::new).collect::<Vec<_>>()),
            BatchSize::PerIteration,
        )
    });
    group.bench_with_input("Tark", &ns, |b, ns| {
        b.iter_batched(
            || ns.clone(),
            |vs| black_box(vs.into_iter().map(Tark::new).collect::<Vec<_>>()),
            BatchSize::PerIteration,
        )
    });
    group.finish();
    black_box(ns);
}

fn clone_bench(c: &mut Criterion) {
    const AMORTIZED: u64 = 1000;
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::clone(), once");
    group.throughput(Throughput::Elements(1));
    let rc = Rc::new(0);
    let arc = Arc::new(0);
    let tark = Tark::new(0);
    group.bench_with_input("Rc", &rc, |b, rc| {
        b.iter_with_large_drop(|| black_box(rc.clone()));
    });
    group.bench_with_input("Arc", &arc, |b, arc| {
        b.iter_with_large_drop(|| black_box(arc.clone()));
    });
    group.bench_with_input("Tark", &tark, |b, tark| {
        b.iter_with_large_drop(|| black_box(tark.clone()));
    });
    group.finish();
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::clone(), amortized");
    group.throughput(Throughput::Elements(AMORTIZED));
    let rcs = std::iter::repeat(rc.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    let arcs = std::iter::repeat(arc.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    let tarks = std::iter::repeat(tark.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    group.bench_with_input("Rc", &rcs, |b, rcs| {
        b.iter_batched(
            || rcs,
            |vs| black_box(vs.iter().map(Rc::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("Arc", &arcs, |b, arcs| {
        b.iter_batched(
            || arcs,
            |vs| black_box(vs.iter().map(Arc::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("Tark", &tarks, |b, tarks| {
        b.iter_batched(
            || tarks,
            |vs| black_box(vs.iter().map(Tark::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.finish();
    black_box(rc);
    black_box(arc);
    black_box(tark);
    black_box(rcs);
    black_box(arcs);
    black_box(tarks);
}

fn deref_bench(c: &mut Criterion) {
    let mut group = c.benchmark_group("[Rc/Arc/Tark] deref, once");
    group.throughput(Throughput::Elements(1));
    let rc = Rc::new(0);
    let arc = Arc::new(0);
    let tark = Tark::new(0);
    group.bench_with_input("Rc", &rc, |b, rc| {
        b.iter_with_large_drop(|| black_box(&*rc));
    });
    group.bench_with_input("Arc", &arc, |b, arc| {
        b.iter_with_large_drop(|| black_box(&*arc));
    });
    group.bench_with_input("Tark", &tark, |b, tark| {
        b.iter_with_large_drop(|| black_box(&*tark));
    });
    group.finish();
    black_box(rc);
    black_box(arc);
    black_box(tark);
}

fn weaken_bench(c: &mut Criterion) {
    const AMORTIZED: u64 = 1000;
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::downgrade(), once");
    group.throughput(Throughput::Elements(1));
    let rc = Rc::new(0);
    let arc = Arc::new(0);
    let tark = Tark::new(0);
    group.bench_with_input("Rc", &rc, |b, rc| {
        b.iter_with_large_drop(|| black_box(Rc::downgrade(black_box(rc))));
    });
    group.bench_with_input("Arc", &arc, |b, arc| {
        b.iter_with_large_drop(|| black_box(Arc::downgrade(black_box(arc))));
    });
    group.bench_with_input("Tark", &tark, |b, tark| {
        b.iter_with_large_drop(|| black_box(Tark::downgrade(black_box(tark))));
    });
    group.finish();
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::downgrade(), amortized");
    group.throughput(Throughput::Elements(AMORTIZED));
    let rcs = std::iter::repeat(rc.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    let arcs = std::iter::repeat(arc.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    let tarks = std::iter::repeat(tark.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    group.bench_with_input("Rc", &rcs, |b, rcs| {
        b.iter_batched(
            || rcs,
            |vs| black_box(vs.iter().map(Rc::downgrade).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("Arc", &arcs, |b, arcs| {
        b.iter_batched(
            || arcs,
            |vs| black_box(vs.iter().map(Arc::downgrade).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("Tark", &tarks, |b, tarks| {
        b.iter_batched(
            || tarks,
            |vs| black_box(vs.iter().map(Tark::downgrade).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.finish();
    black_box(rc);
    black_box(arc);
    black_box(tark);
    black_box(rcs);
    black_box(arcs);
    black_box(tarks);
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = new_bench, clone_bench, deref_bench, weaken_bench,
}

criterion_main!(benches);
