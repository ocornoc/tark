use std::rc::{Rc, Weak as WeakRc};
use std::sync::{Arc, Weak as WeakArc};
use tark::{Tark, WeakTark};
use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize};

fn new_bench(c: &mut Criterion) {
    const CLONE_RANGES: &[usize] = &[1, 1000, 10000];
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::new()");

    for &end in CLONE_RANGES {
        let iter = (0..end).into_iter().collect::<Vec<_>>();

        group.bench_with_input(format!("{} usizes, Rc", end), &iter, |b, iter| {
            b.iter(|| for &i in iter.iter() {
                black_box(Rc::new(i));
            });
        });

        group.bench_with_input(format!("{} usizes, Arc", end), &iter, |b, iter| {
            b.iter(|| for &i in iter.iter() {
                black_box(Arc::new(i));
            });
        });

        group.bench_with_input(format!("{} usizes, Tark", end), &iter, |b, iter| {
            b.iter(|| for &i in iter.iter() {
                black_box(Tark::new(i));
            });
        });
    }
}

fn clone_bench(c: &mut Criterion) {
    const CLONE_RANGES: &[usize] = &[1, 1000, 10000];
    let mut group = c.benchmark_group("[Rc/Arc/Tark]::clone()");

    for num in CLONE_RANGES {
        let rc = Rc::new(0);
        let arc = Arc::new(0);
        let tark = Tark::new(0);

        group.bench_with_input(format!("{} clones, Rc", num), &(), |b, _| {
            b.iter(|| for _ in 0..*num {
                black_box(black_box(&rc).clone());
            })
        });

        group.bench_with_input(format!("{} clones, Arc", num), &(), |b, _| {
            b.iter(|| for _ in 0..*num {
                black_box(black_box(&arc).clone());
            })
        });

        group.bench_with_input(format!("{} clones, Tark", num), &(), |b, _| {
            b.iter(|| for _ in 0..*num {
                black_box(black_box(&tark).clone());
            })
        });
    }
}

criterion_group! {
    name = new;
    config = Criterion::default().sample_size(300);
    targets = new_bench, clone_bench,
}

criterion_main!(new);
