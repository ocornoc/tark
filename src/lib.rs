use std::fmt::{Debug, Display, Formatter, Result as FmtResult, Pointer};
use std::hash::{Hash, Hasher};
use std::ptr::NonNull;
use std::ops::Deref;
use std::borrow::Borrow;
use std::cell::Cell;
use std::sync::atomic::{AtomicUsize, Ordering};

struct TarkInner<T: ?Sized> {
    strong: AtomicUsize,
    data: T,
}

impl<T: ?Sized> TarkInner<T> {
    fn dec_maybe_drop(inner: NonNull<TarkInner<T>>) {
        if unsafe { inner.as_ref() }.strong.fetch_sub(1, Ordering::AcqRel) == 1 {
            unsafe { drop(inner); }
        }
    }

    fn inc(inner: NonNull<TarkInner<T>>) {
        unsafe { inner.as_ref() }.strong.fetch_add(1, Ordering::Release);
    }

    const fn new(data: T) -> Self
    where
        T: Sized,
    {
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

pub struct TarkSend<T: ?Sized + Send + Sync> {
    inner: NonNull<TarkInner<T>>,
}

impl<T: ?Sized + Send + Sync> TarkSend<T> {
    pub fn new(t: T) -> Self
    where
        T: Sized,
    {
        Self::from_raw(unsafe { alloc_nonnull(TarkInner::new(t)) })
    }

    fn from_raw(inner: NonNull<TarkInner<T>>) -> Self {
        TarkInner::inc(inner);
        TarkSend { inner }
    }

    pub fn promote(this: Self) -> Tark<T> {
        let t = Tark {
            inner: this.inner,
            strong_weak: unsafe { StrongWeak::alloc() },
        };
        std::mem::forget(this);
        t
    }

    pub fn promote_ref(this: &Self) -> Tark<T> {
        TarkInner::inc(this.inner);
        Tark {
            inner: this.inner,
            strong_weak: unsafe { StrongWeak::alloc() },
        }
    }

    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.inner.eq(&other.inner)
    }
}

impl<T: ?Sized + Send + Sync + Hash> Hash for TarkSend<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<T: ?Sized + Send + Sync + PartialEq> PartialEq for TarkSend<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }

    fn ne(&self, other: &Self) -> bool {
        self.as_ref().ne(other.as_ref())
    }
}

impl<T: ?Sized + Send + Sync + Eq> Eq for TarkSend<T> {}

impl<T: ?Sized + Send + Sync + PartialOrd> PartialOrd for TarkSend<T> {
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

impl<T: ?Sized + Send + Sync + Ord> Ord for TarkSend<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T: ?Sized + Send + Sync> Clone for TarkSend<T> {
    fn clone(&self) -> Self {
        Self::from_raw(self.inner)
    }

    fn clone_from(&mut self, source: &Self) {
        if self.inner != source.inner {
            std::mem::drop(std::mem::replace(self, source.clone()));
        }
    }
}

impl<T: ?Sized + Send + Sync> Drop for TarkSend<T> {
    fn drop(&mut self) {
        TarkInner::dec_maybe_drop(self.inner);
    }
}

unsafe impl<T: ?Sized + Send + Sync> Send for TarkSend<T> {}
unsafe impl<T: ?Sized + Send + Sync> Sync for TarkSend<T> {}

impl<T: ?Sized + Send + Sync> AsRef<T> for TarkSend<T> {
    fn as_ref(&self) -> &T {
        &unsafe { self.inner.as_ref() }.data
    }
}

impl<T: ?Sized + Send + Sync> Borrow<T> for TarkSend<T> {
    fn borrow(&self) -> &T {
        self.as_ref()
    }
}

impl<T: ?Sized + Send + Sync> Deref for TarkSend<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl<T: ?Sized + Send + Sync + Debug> Debug for TarkSend<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f
            .debug_tuple("TarkSend")
            .field(&self.as_ref())
            .finish()
    }
}

impl<T: ?Sized + Send + Sync + Display> Display for TarkSend<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Display::fmt(&self, f)
    }
}

impl<T: ?Sized + Send + Sync> Pointer for TarkSend<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Pointer::fmt(&self.inner, f)
    }
}

pub struct Tark<T: ?Sized> {
    inner: NonNull<TarkInner<T>>,
    strong_weak: NonNull<StrongWeak>,
}

impl<T: ?Sized> Tark<T> {
    pub fn new(t: T) -> Self
    where
        T: Sized,
    {
        let inner = unsafe { alloc_nonnull(TarkInner::new(t)) };
        unsafe { Tark {
            inner,
            strong_weak: StrongWeak::alloc(),
        } }
    }

    fn strong(this: &Self) -> &Cell<usize> {
        &unsafe { this.strong_weak.as_ref() }.strong
    }

    fn weak(this: &Self) -> &Cell<usize> {
        &unsafe { this.strong_weak.as_ref() }.weak
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
            inner: this.inner,
            strong_weak: this.strong_weak,
        }
    }
}

impl<T: ?Sized + Send + Sync> Tark<T> {
    pub fn sendable(this: Self) -> TarkSend<T> {
        TarkSend::from_raw(this.inner)
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
            inner: self.inner,
            strong_weak: self.strong_weak,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        if self.inner != source.inner {
            std::mem::drop(std::mem::replace(self, source.clone()));
        }
    }
}

impl<T: ?Sized> Drop for Tark<T> {
    fn drop(&mut self) {
        let strong = Self::strong(self);
        let count = strong.get();

        if count == 1 {
            TarkInner::dec_maybe_drop(self.inner);

            if Self::weak_count(self) == 0 {
                unsafe { drop(self.strong_weak); }
            }
        }

        strong.set(count - 1);
    }
}

impl<T: ?Sized> AsRef<T> for Tark<T> {
    fn as_ref(&self) -> &T {
        &unsafe { self.inner.as_ref() }.data
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
        Pointer::fmt(&self.inner, f)
    }
}

pub struct WeakTark<T: ?Sized> {
    inner: NonNull<TarkInner<T>>,
    strong_weak: NonNull<StrongWeak>,
}

pub type Weak<T> = WeakTark<T>;

impl<T: ?Sized> Weak<T> {
    fn strong(this: &Self) -> &Cell<usize> {
        &unsafe { this.strong_weak.as_ref() }.strong
    }

    fn weak(this: &Self) -> &Cell<usize> {
        &unsafe { this.strong_weak.as_ref() }.weak
    }

    pub fn strong_count(this: &Self) -> usize {
        Self::strong(this).get()
    }

    pub fn weak_count(this: &Self) -> usize {
        Self::weak(this).get()
    }

    pub fn upgrade(this: &Self) -> Option<Tark<T>> {
        if Weak::strong_count(this) == 0 {
            None
        } else {
            let strong = Weak::strong(this);
            strong.set(strong.get() + 1);
            Some(Tark {
                inner: this.inner,
                strong_weak: this.strong_weak,
            })
        }
    }
}

impl<T: ?Sized> Clone for Weak<T> {
    fn clone(&self) -> Self {
        let weak = Self::weak(self);
        weak.set(weak.get() + 1);
        Weak {
            inner: self.inner,
            strong_weak: self.strong_weak,
        }
    }

    fn clone_from(&mut self, source: &Self) {
        if self.inner != source.inner {
            std::mem::drop(std::mem::replace(self, source.clone()));
        }
    }
}

impl<T: ?Sized> Drop for Weak<T> {
    fn drop(&mut self) {
        let weak = Weak::weak(self);

        if weak.get() == 1 && Weak::strong_count(self) == 0 {
            unsafe { drop(self.strong_weak); }
        } else {
            weak.set(weak.get() - 1);
        }
    }
}

impl<T: ?Sized> Pointer for Weak<T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        Pointer::fmt(&self.inner, f)
    }
}

struct StrongWeak {
    strong: Cell<usize>,
    weak: Cell<usize>,
}

impl StrongWeak {
    unsafe fn alloc() -> NonNull<StrongWeak> {
        alloc_nonnull(StrongWeak {
            strong: Cell::new(1),
            weak: Cell::new(0),
        })
    }
}

unsafe fn alloc_nonnull<T>(t: T) -> NonNull<T> {
    NonNull::new_unchecked(Box::into_raw(Box::new(t)))
}

#[cold]
unsafe fn drop<T: ?Sized>(ptr: NonNull<T>) {
    std::mem::drop(Box::from_raw(ptr.as_ptr()))
}
