pub(crate) mod sealed {
    use std::num::NonZero;

    pub trait Numeric: Copy + PartialEq + Eq + PartialOrd + Ord {}

    impl Numeric for u8 {}
    impl Numeric for u16 {}
    impl Numeric for u32 {}
    impl Numeric for u64 {}
    impl Numeric for u128 {}

    impl Numeric for NonZero<u8> {}
    impl Numeric for NonZero<u16> {}
    impl Numeric for NonZero<u32> {}
    impl Numeric for NonZero<u64> {}
    impl Numeric for NonZero<u128> {}
}

/// Asserts the size and alignment of `T` and `U` are the same. This function will fail to compile
/// if the requirements are not met.
pub(crate) const fn assert_same_size_and_align<T, U>() {
    const { assert!(size_of::<T>() == size_of::<U>()) };
    const { assert!(align_of::<T>() == align_of::<U>()) };
}

/// Maps a [`Vec<T>`] to a [`Vec<U>`] as long as `T` and `U` have the same size and alignment. This
/// function will fail to compile if the requirements are not met.
pub(crate) fn map_vec<T, U, F>(mut vec: Vec<T>, mut f: F) -> Vec<U>
where
    F: FnMut(T) -> U,
{
    assert_same_size_and_align::<T, U>();

    let (ptr, len, cap) = (vec.as_mut_ptr(), vec.len(), vec.capacity());

    #[expect(clippy::mem_forget, reason = "we do want to avoid dropping the vec")]
    std::mem::forget(vec);

    for index in 0..len {
        // SAFETY: the resulting pointer is within the same allocated object (the vec)
        let offset_ptr = unsafe { ptr.add(index) };
        // SAFETY: the pointer is aligned and points to a properly initialized T
        let value = unsafe { std::ptr::read(offset_ptr) };
        // SAFETY: the pointer is aligned (asserted at the start of the function)
        unsafe { std::ptr::write(offset_ptr.cast(), f(value)) }
    }

    // SAFETY: all the invariants are met
    unsafe { Vec::from_raw_parts(ptr.cast::<U>(), len, cap) }
}

/// A trait for types which can be used as the index of a stadium.
///
/// # Safety
/// - `next` and `previous` must be inverses
/// - `from_usize` and `to_usize` must only panic on out-of-range values
pub unsafe trait Index: sealed::Numeric {
    fn next(self) -> Option<Self>;
    fn previous(self) -> Option<Self>;

    fn from_usize(value: usize) -> Self;
    fn to_usize(self) -> usize;
}

macro_rules! impl_index {
    ($($ty:ty),*) => {
        $(
            // SAFETY: invariants upheld
            unsafe impl Index for $ty {
                #[inline(always)]
                fn next(self) -> Option<Self> {
                    self.checked_add(1)
                }

                #[inline(always)]
                fn previous(self) -> Option<Self> {
                    self.checked_sub(1)
                }

                #[inline(always)]
                fn from_usize(value: usize) -> Self {
                    Self::try_from(value).unwrap()
                }

                #[inline(always)]
                fn to_usize(self) -> usize {
                    usize::try_from(self).unwrap()
                }
            }

            unsafe impl Index for std::num::NonZero<$ty> {
                #[inline(always)]
                fn next(self) -> Option<Self> {
                    self.checked_add(1)
                }

                #[inline(always)]
                fn previous(self) -> Option<Self> {
                    Self::new(self.get() - 1)
                }

                #[inline(always)]
                fn from_usize(value: usize) -> Self {
                    Self::new(<$ty>::try_from(value + 1).unwrap()).unwrap()
                }

                #[inline(always)]
                fn to_usize(self) -> usize {
                    usize::try_from(self.get() - 1).unwrap()
                }
            }
        )*
    };
}

impl_index! {
    u8,
    u16,
    u32,
    u64,
    u128
}
