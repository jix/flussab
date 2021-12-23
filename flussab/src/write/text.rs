//! Utilities for emitting text based formats using a [`DeferredWriter`].
use crate::DeferredWriter;

mod sealed {
    pub trait Sealed: itoap::Integer {}

    impl Sealed for i8 {}
    impl Sealed for u8 {}
    impl Sealed for i16 {}
    impl Sealed for u16 {}
    impl Sealed for i32 {}
    impl Sealed for u32 {}
    impl Sealed for i64 {}
    impl Sealed for u64 {}
    impl Sealed for i128 {}
    impl Sealed for u128 {}
    impl Sealed for isize {}
    impl Sealed for usize {}
}

/// Primitive integer types that can be efficiently written into a [`DeferredWriter`].
pub trait Integer: sealed::Sealed {}

impl Integer for i8 {}
impl Integer for u8 {}
impl Integer for i16 {}
impl Integer for u16 {}
impl Integer for i32 {}
impl Integer for u32 {}
impl Integer for i64 {}
impl Integer for u64 {}
impl Integer for i128 {}
impl Integer for u128 {}
impl Integer for isize {}
impl Integer for usize {}

/// Write a decimal number using ASCII digits.
#[inline]
pub fn ascii_digits<I>(writer: &mut DeferredWriter, value: I)
where
    I: Integer,
{
    let ptr = writer.buf_write_ptr(I::MAX_LEN);
    if ptr.is_null() {
        ascii_digits_cold(writer, value)
    } else {
        // SAFETY above we requested space for `I::MAX_LEN` which is the most `itoap::write_to_ptr`
        // will write. It returns the number of bytes written, so we can safely advance by it.
        unsafe {
            let len = itoap::write_to_ptr(ptr, value);
            writer.advance_unchecked(len);
        }
    }
}

#[inline(never)]
#[cold]
fn ascii_digits_cold<I>(writer: &mut DeferredWriter, value: I)
where
    I: Integer,
{
    let _ = itoap::write(writer, value);
}
