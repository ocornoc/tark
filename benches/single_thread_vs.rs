use std::rc::{Rc, Weak as WeakRc};
use std::sync::{Arc, Weak as WeakArc};
use tark::{Tark, WeakTark};
use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize};

fn new_bench(c: &mut Criterion) {
    const RANGES: &[usize] = &[1000];
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::new()");

    group.bench_with_input("1 usize, Rc", &(), |b, _| {
        b.iter(|| black_box(Rc::new(black_box(0))));
    });

    group.bench_with_input("1 usize, Arc", &(), |b, _| {
        b.iter(|| black_box(Arc::new(black_box(0))));
    });

    group.bench_with_input("1 usize, Tark", &(), |b, _| {
        b.iter(|| black_box(Tark::new(black_box(0))));
    });

    for &end in RANGES {
        let ns = (0..end).into_iter().collect::<Vec<_>>();

        group.bench_with_input(format!("{} usizes, Rc", end), &ns, |b, ns| {
            b.iter_batched(
                || black_box(ns.clone()),
                |vs| vs.into_iter().map(Rc::new).collect::<Vec<_>>(),
                BatchSize::PerIteration,
            )
        });

        group.bench_with_input(format!("{} usizes, Arc", end), &ns, |b, ns| {
            b.iter_batched(
                || black_box(ns.clone()),
                |vs| vs.into_iter().map(Arc::new).collect::<Vec<_>>(),
                BatchSize::PerIteration,
            )
        });

        group.bench_with_input(format!("{} usizes, Tark", end), &ns, |b, ns| {
            b.iter_batched(
                || black_box(ns.clone()),
                |vs| vs.into_iter().map(Tark::new).collect::<Vec<_>>(),
                BatchSize::PerIteration,
            )
        });
    }
}

fn clone_bench(c: &mut Criterion) {
    const RANGES: &[usize] = &[1000];
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::clone()");
    let rc = Rc::new(0);
    let arc = Arc::new(0);
    let tark = Tark::new(0);

    group.bench_with_input("1 clone, Rc", &rc, |b, rc| {
        b.iter_with_large_drop(|| black_box(rc.clone()));
    });

    group.bench_with_input("1 clone, Arc", &arc, |b, arc| {
        b.iter_with_large_drop(|| black_box(arc.clone()));
    });

    group.bench_with_input("1 clone, Tark", &tark, |b, tark| {
        b.iter_with_large_drop(|| black_box(tark.clone()));
    });

    for &end in RANGES {
        let rcs = std::iter::repeat(rc.clone()).take(end).into_iter().collect::<Vec<_>>();
        let arcs = std::iter::repeat(arc.clone()).take(end).into_iter().collect::<Vec<_>>();
        let tarks = std::iter::repeat(tark.clone()).take(end).into_iter().collect::<Vec<_>>();

        group.bench_with_input(format!("{} clones, Rc", end), &rcs, |b, rcs| {
            b.iter_batched(
                || black_box(rcs.clone()),
                |vs| black_box(vs.iter().map(Rc::clone).collect::<Vec<_>>()),
                BatchSize::NumIterations(1000),
            )
        });

        group.bench_with_input(format!("{} clones, Arc", end), &arcs, |b, arcs| {
            b.iter_batched(
                || black_box(arcs.clone()),
                |vs| black_box(vs.iter().map(Arc::clone).collect::<Vec<_>>()),
                BatchSize::NumIterations(1000),
            )
        });

        group.bench_with_input(format!("{} clones, Tark", end), &tarks, |b, tarks| {
            b.iter_batched(
                || black_box(tarks.clone()),
                |vs| black_box(vs.iter().map(Tark::clone).collect::<Vec<_>>()),
                BatchSize::NumIterations(1000),
            )
        });
    }
}

criterion_group! {
    name = new;
    config = Criterion::default().sample_size(300);
    targets = new_bench, clone_bench,
}

criterion_main!(new);
