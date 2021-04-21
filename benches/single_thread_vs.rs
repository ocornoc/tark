use std::rc::{Rc, Weak as WeakRc};
use std::sync::{Arc, Weak as WeakArc};
use tark::{Tark, WeakTark, swap::{Tark as SwapTark, WeakTark as SwapWeakTark}};
use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize, Throughput};

fn new_bench(c: &mut Criterion) {
    const AMORTIZED: u64 = 1000;
    let mut group = c.benchmark_group("[Rc/Arc/Tark/SwapTark]::new(), once");
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
    group.bench_with_input("SwapTark", &(), |b, _| {
        b.iter(|| black_box(SwapTark::new(black_box(0))));
    });
    group.finish();

    let mut group = c.benchmark_group("[Rc/Arc/Tark/SwapTark]::new(), amortized");
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
    group.bench_with_input("SwapTark", &ns, |b, ns| {
        b.iter_batched(
            || ns.clone(),
            |vs| black_box(vs.into_iter().map(SwapTark::new).collect::<Vec<_>>()),
            BatchSize::PerIteration,
        )
    });
    group.finish();
}

fn clone_bench(c: &mut Criterion) {
    const AMORTIZED: u64 = 1000;
    let mut group = c.benchmark_group("[Rc/Arc/Tark/SwapTark]::clone(), once");
    group.throughput(Throughput::Elements(1));
    let rc = Rc::new(0);
    let arc = Arc::new(0);
    let tark = Tark::new(0);
    let swaptark = SwapTark::new(0);
    group.bench_with_input("Rc", &rc, |b, rc| {
        b.iter_with_large_drop(|| black_box(rc.clone()));
    });
    group.bench_with_input("Arc", &arc, |b, arc| {
        b.iter_with_large_drop(|| black_box(arc.clone()));
    });
    group.bench_with_input("Tark", &tark, |b, tark| {
        b.iter_with_large_drop(|| black_box(tark.clone()));
    });
    group.bench_with_input("SwapTark", &tark, |b, tark| {
        b.iter_with_large_drop(|| black_box(tark.clone()));
    });
    group.finish();
    let mut group = c.benchmark_group("[Rc/Arc/Tark/SwapTark]::clone(), amortized");
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
    let swaptarks = std::iter::repeat(swaptark.clone())
        .take(AMORTIZED as usize)
        .into_iter()
        .collect::<Vec<_>>();
    group.bench_with_input("Rc", &rcs, |b, rcs| {
        b.iter_batched(
            || black_box(rcs.clone()),
            |vs| black_box(vs.iter().map(Rc::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("Arc", &arcs, |b, arcs| {
        b.iter_batched(
            || black_box(arcs.clone()),
            |vs| black_box(vs.iter().map(Arc::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("Tark", &tarks, |b, tarks| {
        b.iter_batched(
            || black_box(tarks.clone()),
            |vs| black_box(vs.iter().map(Tark::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.bench_with_input("SwapTark", &swaptarks, |b, swaptarks| {
        b.iter_batched(
            || black_box(swaptarks.clone()),
            |vs| black_box(vs.iter().map(SwapTark::clone).collect::<Vec<_>>()),
            BatchSize::NumIterations(1000),
        )
    });
    group.finish();
}

criterion_group! {
    name = new;
    config = Criterion::default();
    targets = new_bench, clone_bench,
}

criterion_main!(new);
