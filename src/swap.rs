use std::fmt::{Debug, Display, Formatter, Result as FmtResult, Pointer};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::borrow::Borrow;
use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, AtomicPtr, Ordering};
use std::num::NonZeroUsize;
use std::ptr::NonNull;
use std::mem::ManuallyDrop;

struct TarkInner<T: ?Sized> {
    strong: AtomicUsize,
    data: T,
}

impl<T: ?Sized> TarkInner<T> {
    fn dec_maybe_drop_nonnull(inner: NonNull<TarkInner<T>>) {
        // SAFE: `inner` is assumed valid
        if unsafe { inner.as_ref() }.strong.fetch_sub(1, Ordering::AcqRel) == 1 {
            // SAFE: `inner` was allocated as a box, and thus can be dropped as
            // one.
            unsafe { drop_nonnull(inner) };
        }
    }
}

impl<T> TarkInner<T> {
    fn dec_maybe_drop_atomic(inner: &AtomicPtr<ManuallyDrop<TarkInner<T>>>) {
        // SAFE: `inner` is assumed valid
        if unsafe { inner.load(Ordering::Acquire).read() }.strong.fetch_sub(1, Ordering::AcqRel) == 1 {
            // SAFE: `inner` was allocated as a box, and thus can be dropped as
            // one.
            unsafe { drop_atomic(inner) };
        }
    }

    fn inc(inner: &AtomicPtr<ManuallyDrop<TarkInner<T>>>) {
        // SAFE: `inner` is assumed valid
        unsafe { inner.load(Ordering::Acquire).read() }.strong.fetch_add(1, Ordering::Release);
    }

    const fn new(data: T) -> Self {
        TarkInner {
            strong: AtomicUsize::new(1),
            data,
        }
    }
}

impl<T: ?Sized + Hash> Hash for TarkInner<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state)
    }
}

impl<T: ?Sized + PartialEq> PartialEq for TarkInner<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data.eq(&other.data)
    }

    fn ne(&self, other: &Self) -> bool {
        self.data.ne(&other.data)
    }
}

impl<T: ?Sized + Eq> Eq for TarkInner<T> {}

impl<T: ?Sized + PartialOrd> PartialOrd for TarkInner<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.data.partial_cmp(&other.data)
    }

    fn lt(&self, other: &Self) -> bool {
        self.data.lt(&other.data)
    }

    fn le(&self, other: &Self) -> bool {
        self.data.le(&other.data)
    }

    fn gt(&self, other: &Self) -> bool {
        self.data.gt(&other.data)
    }

    fn ge(&self, other: &Self) -> bool {
        self.data.ge(&other.data)
    }
}

impl<T: ?Sized + Ord> Ord for TarkInner<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.data.cmp(&other.data)
    }
}

impl<T: ?Sized + Debug> Debug for TarkInner<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        T::fmt(&self.data, f)
    }
}

impl<T: ?Sized + Display> Display for TarkInner<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        T::fmt(&self.data, f)
    }
}

pub struct TarkSend<T: Send + Sync> {
    inner: AtomicPtr<ManuallyDrop<TarkInner<T>>>,
}

impl<T: Send + Sync> TarkSend<T> {
    pub fn new(t: T) -> Self
    where
        T: Sized,
    {
        Self::from_raw(alloc_atomic(TarkInner::new(t)))
    }

    pub fn atomic_count(this: &Self) -> NonZeroUsize {
        // SAFE: The atomic refcount is guaranteed non-zero.
        unsafe { NonZeroUsize::new_unchecked(
            (*this.inner.load(Ordering::Acquire)).strong.load(Ordering::Acquire),
        ) }
    }

    fn from_raw(inner: AtomicPtr<ManuallyDrop<TarkInner<T>>>) -> Self {
        TarkInner::inc(&inner);
        TarkSend { inner }
    }

    pub fn promote(this: Self) -> Tark<T> {
        let t = Tark {
            inner: Cell::new(atomic_to_nonnull(&this.inner)),
            strong_weak: StrongWeak::alloc(),
        };
        std::mem::forget(this);
        t
    }

    pub fn promote_ref(this: &Self) -> Tark<T> {
        TarkInner::inc(&this.inner);
        Tark {
            inner: Cell::new(atomic_to_nonnull(&this.inner)),
            strong_weak: StrongWeak::alloc(),
        }
    }

    pub fn swap(this: &Self, other: &Self) {
        this.inner.swap(other.inner.load(Ordering::Acquire), Ordering::AcqRel);
    }
}

impl<T: Send + Sync + Hash> Hash for TarkSend<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<T: Send + Sync + PartialEq> PartialEq for TarkSend<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }

    fn ne(&self, other: &Self) -> bool {
        self.as_ref().ne(other.as_ref())
    }
}

impl<T: Send + Sync + Eq> Eq for TarkSend<T> {}

impl<T: Send + Sync + PartialOrd> PartialOrd for TarkSend<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }

    fn lt(&self, other: &Self) -> bool {
        self.as_ref().lt(other.as_ref())
    }

    fn le(&self, other: &Self) -> bool {
        self.as_ref().le(other.as_ref())
    }

    fn gt(&self, other: &Self) -> bool {
        self.as_ref().gt(other.as_ref())
    }

    fn ge(&self, other: &Self) -> bool {
        self.as_ref().ge(other.as_ref())
    }
}

impl<T: Send + Sync + Ord> Ord for TarkSend<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T: Send + Sync> Clone for TarkSend<T> {
    fn clone(&self) -> Self {
        Self::from_raw(AtomicPtr::new(self.inner.load(Ordering::Acquire)))
    }

    fn clone_from(&mut self, source: &Self) {
        std::mem::drop(std::mem::replace(self, source.clone()));
    }
}

impl<T: Send + Sync> Drop for TarkSend<T> {
    fn drop(&mut self) {
        TarkInner::dec_maybe_drop_atomic(&self.inner);
    }
}

// SAFE: `T` is Send and Sync, meaning a pointer to it is as well.
unsafe impl<T: Send + Sync> Send for TarkSend<T> {}

// SAFE: `T` is Send and Sync, meaning a pointer to it is as well.
unsafe impl<T: Send + Sync> Sync for TarkSend<T> {}

impl<T: Send + Sync> AsRef<T> for TarkSend<T> {
    fn as_ref(&self) -> &T {
        // SAFE: `inner` is a Box pointer, which upholds all the same invariants
        // necessary for .as_ref() except mutable aliasing. we also only allow
        // non-mutable references, so it all works out.
        &unsafe { self.inner.load(Ordering::Relaxed).as_ref() }.unwrap().data
    }
}

impl<T: Send + Sync> Borrow<T> for TarkSend<T> {
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<T: Send + Sync> Deref for TarkSend<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: Send + Sync + Debug> Debug for TarkSend<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_tuple("TarkSend")
            .field(&self.as_ref())
            .finish()
    }
}

impl<T: Send + Sync + Display> Display for TarkSend<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self, f)
    }
}

impl<T: Send + Sync> Pointer for TarkSend<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Pointer::fmt(&self.inner, f)
    }
}

pub struct Tark<T: ?Sized> {
    inner: Cell<NonNull<TarkInner<T>>>,
    strong_weak: NonNull<StrongWeak>,
}

pub type TarkLocal<T> = Tark<T>;

impl<T: ?Sized> Tark<T> {
    pub fn new(t: T) -> Self
    where
        T: Sized,
    {
        let inner = Cell::new(alloc_nonnull(TarkInner::new(t)));
        Tark {
            inner,
            strong_weak: StrongWeak::alloc(),
        }
    }

    fn strong(this: &Self) -> &Cell<usize> {
        // SAFE: `strong_weak` is a Box pointer, which upholds all the same
        // invariants necessary for .as_ref() except mutable aliasing. we also
        // only allow non-mutable references, so it all works out.
        &unsafe { this.strong_weak.as_ref() }.strong
    }

    fn weak(this: &Self) -> &Cell<usize> {
        // SAFE: `strong_weak` is a Box pointer, which upholds all the same
        // invariants necessary for .as_ref() except mutable aliasing. we also
        // only allow non-mutable references, so it all works out.
        &unsafe { this.strong_weak.as_ref() }.weak
    }

    pub fn atomic_count(this: &Self) -> NonZeroUsize {
        // SAFE: The atomic refcount is guaranteed non-zero.
        unsafe { NonZeroUsize::new_unchecked(
            this.inner.get().as_ref().strong.load(Ordering::Acquire),
        ) }
    }

    pub fn strong_count(this: &Self) -> usize {
        Self::strong(this).get()
    }

    pub fn weak_count(this: &Self) -> usize {
        Self::weak(this).get()
    }

    pub fn downgrade(this: &Self) -> Weak<T> {
        let weak = Self::weak(this);
        weak.set(weak.get() + 1);
        WeakTark {
            inner: this.inner.clone(),
            strong_weak: this.strong_weak,
        }
    }

    pub fn swap(this: &Self, other: &Self) {
        this.inner.swap(&other.inner);
    }
}

impl<T: Send + Sync> Tark<T> {
    pub fn sendable(this: Self) -> TarkSend<T> {
        TarkSend::from_raw(nonnull_to_atomic(this.inner.get()))
    }
}

impl<T: ?Sized + Hash> Hash for Tark<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Tark<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }

    fn ne(&self, other: &Self) -> bool {
        self.as_ref().ne(other.as_ref())
    }
}

impl<T: ?Sized + Eq> Eq for Tark<T> {}

impl<T: ?Sized + PartialOrd> PartialOrd for Tark<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }

    fn lt(&self, other: &Self) -> bool {
        self.as_ref().lt(other.as_ref())
    }

    fn le(&self, other: &Self) -> bool {
        self.as_ref().le(other.as_ref())
    }

    fn gt(&self, other: &Self) -> bool {
        self.as_ref().gt(other.as_ref())
    }

    fn ge(&self, other: &Self) -> bool {
        self.as_ref().ge(other.as_ref())
    }
}

impl<T: ?Sized + Ord> Ord for Tark<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T: ?Sized> Clone for Tark<T> {
    fn clone(&self) -> Self {
        let strong = Self::strong(self);
        strong.set(strong.get() + 1);
        Tark {
            inner: self.inner.clone(),
            strong_weak: self.strong_weak,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        std::mem::drop(std::mem::replace(self, source.clone()));
    }
}

impl<T: ?Sized> Drop for Tark<T> {
    fn drop(&mut self) {
        let strong = Self::strong(self);
        let count = strong.get();

        if count == 1 {
            TarkInner::dec_maybe_drop_nonnull(self.inner.get());

            if Self::weak_count(self) == 0 {
                // SAFE: strong_weak was allocated as a box, and thus can be
                // dropped as one.
                unsafe { drop_nonnull(self.strong_weak); }
            }
        }

        strong.set(count - 1);
    }
}

impl<T: ?Sized> AsRef<T> for Tark<T> {
    fn as_ref(&self) -> &T {
        // SAFE: `inner` is a Box pointer, which upholds all the same invariants
        // necessary for .as_ref() except mutable aliasing. we also only allow
        // non-mutable references, so it all works out.
        if let Some(r) = unsafe { self.inner.get().as_ptr().as_ref() } {
            &r.data
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }
}

impl<T: ?Sized> Borrow<T> for Tark<T> {
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<T: ?Sized> Deref for Tark<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: ?Sized + Debug> Debug for Tark<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_tuple("Tark")
            .field(&self.as_ref())
            .finish()
    }
}

impl<T: ?Sized + Display> Display for Tark<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self, f)
    }
}

impl<T: ?Sized> Pointer for Tark<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Pointer::fmt(&self.inner.get(), f)
    }
}

pub struct WeakTark<T: ?Sized> {
    inner: Cell<NonNull<TarkInner<T>>>,
    strong_weak: NonNull<StrongWeak>,
}

pub type Weak<T> = WeakTark<T>;

impl<T: ?Sized> Weak<T> {
    fn strong(this: &Self) -> &Cell<usize> {
        // SAFE: `strong_weak` is a Box pointer, which upholds all the same
        // invariants necessary for .as_ref() except mutable aliasing. we also
        // only allow non-mutable references, so it all works out.
        &unsafe { this.strong_weak.as_ref() }.strong
    }

    fn weak(this: &Self) -> &Cell<usize> {
        // SAFE: `strong_weak` is a Box pointer, which upholds all the same
        // invariants necessary for .as_ref() except mutable aliasing. we also
        // only allow non-mutable references, so it all works out.
        &unsafe { this.strong_weak.as_ref() }.weak
    }

    pub fn strong_count(this: &Self) -> usize {
        Self::strong(this).get()
    }

    pub fn weak_count(this: &Self) -> usize {
        Self::weak(this).get()
    }

    pub fn atomic_count(this: &Self) -> Option<NonZeroUsize> {
        if Self::strong_count(this) == 0 {
            None
        } else {
            // SAFE: if the strong count is non-zero, there is some Tark, and
            // thus the atomic count is non-zero.
            Some(unsafe { NonZeroUsize::new_unchecked(
                this.inner.get().as_ref().strong.load(Ordering::Relaxed),
            ) })
        }
    }

    pub fn upgrade(this: &Self) -> Option<Tark<T>> {
        if Weak::strong_count(this) == 0 {
            None
        } else {
            let strong = Weak::strong(this);
            strong.set(strong.get() + 1);
            Some(Tark {
                inner: this.inner.clone(),
                strong_weak: this.strong_weak,
            })
        }
    }

    pub fn swap(this: &Self, other: &Self) {
        this.inner.swap(&other.inner);
    }
}

impl<T: ?Sized> Clone for Weak<T> {
    fn clone(&self) -> Self {
        let weak = Self::weak(self);
        weak.set(weak.get() + 1);
        Weak {
            inner: self.inner.clone(),
            strong_weak: self.strong_weak,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        std::mem::drop(std::mem::replace(self, source.clone()));
    }
}

impl<T: ?Sized> Drop for Weak<T> {
    fn drop(&mut self) {
        let weak = Weak::weak(self);

        if weak.get() == 1 && Weak::strong_count(self) == 0 {
            // SAFE: strong_weak was allocated as a box, and thus can be
            // dropped as one.
            unsafe { drop_nonnull(self.strong_weak); }
        } else {
            weak.set(weak.get() - 1);
        }
    }
}

impl<T: ?Sized> Pointer for Weak<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Pointer::fmt(&self.inner.get(), f)
    }
}

struct StrongWeak {
    strong: Cell<usize>,
    weak: Cell<usize>,
}

impl StrongWeak {
    fn alloc() -> NonNull<StrongWeak> {
        alloc_nonnull(StrongWeak {
            strong: Cell::new(1),
            weak: Cell::new(0),
        })
    }
}

fn alloc_nonnull<T>(t: T) -> NonNull<T> {
    // SAFE: `Box` itself holds a `Unique` which is guaranteed non-null, so the
    // raw pointer must be non-null too.
    unsafe { NonNull::new_unchecked(Box::into_raw(Box::new(t))) }
}

fn alloc_atomic<T>(t: T) -> AtomicPtr<ManuallyDrop<T>> {
    AtomicPtr::new(Box::into_raw(Box::new(ManuallyDrop::new(t))))
}

fn atomic_to_nonnull<T>(ptr: &AtomicPtr<ManuallyDrop<T>>) -> NonNull<T> {
    // SAFE: the original ptr is never null, so we dont need to check if this is
    unsafe { NonNull::new_unchecked(ptr.load(Ordering::Release).cast()) }
}

fn nonnull_to_atomic<T>(ptr: NonNull<T>) -> AtomicPtr<ManuallyDrop<T>> {
    AtomicPtr::new(ptr.as_ptr().cast())
}

#[cold]
unsafe fn drop_nonnull<T: ?Sized>(mut ptr: NonNull<T>) {
    std::mem::drop(Box::from_raw(ptr.as_mut()))
}

#[cold]
unsafe fn drop_atomic<T>(ptr: &AtomicPtr<ManuallyDrop<T>>) {
    let b: Box<T> = Box::from_raw(ptr.load(Ordering::Acquire).cast());
    std::mem::drop(b);
}
