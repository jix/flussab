use std::io::{self, Write};

/// A buffered writer with deferred error checking.
///
/// This can be used like [`std::io::BufWriter`], but like [`DeferredReader`][crate::DeferredReader]
/// this performs deferred error checking. This means that any call to [`write`][Write::write], will
/// always succeed. IO errors that occur during writing will be reported during the next call to
/// [`flush`][Write::flush] or [`check_io_error`][Self::check_io_error]. Any data written after an
/// IO error occured, before it is eventually reported, will be discarded.
///
/// Deferring error checks like this can result in a significant speed up for some usage patterns.
pub struct DeferredWriter<'a> {
    write: Box<dyn Write + 'a>,
    buf: Vec<u8>,
    io_error: Option<io::Error>,
    panicked: bool,
}

impl<'a> DeferredWriter<'a> {
    const DEFAULT_CHUNK_SIZE: usize = 16 << 10;

    /// Creates a [`DeferredWriter`] writing data to a [`Write`] instance.
    pub fn from_write(write: impl Write + 'a) -> Self {
        Self::from_boxed_dyn_write(Box::new(write))
    }

    /// Creates a [`DeferredWriter`] writing data to a boxed [`Write`] instance.
    #[inline(never)]
    pub fn from_boxed_dyn_write(write: Box<dyn Write + 'a>) -> Self {
        DeferredWriter {
            write,
            buf: Vec::with_capacity(Self::DEFAULT_CHUNK_SIZE),
            io_error: None,
            panicked: false,
        }
    }

    /// Flush the buffered data to the underlying [`Write`] instance, deferring IO errors.
    pub fn flush_defer_err(&mut self) {
        // Silently discard data if we errored before but haven't reported it yet
        if self.io_error.is_none() {
            self.panicked = true;
            if let Err(err) = self.write.write_all(&self.buf) {
                self.io_error = Some(err);
            }
            self.panicked = false;
        }
        self.buf.clear();
    }

    /// Write a slice of bytes, deferring IO errors.
    ///
    /// Both, [`write`][Write::write] and [`write_all`][Write::write_all] directly call this method.
    /// Unlike them, this does not return a `#[must_use]` value, making it clear that this cannot
    /// return an error.
    #[inline]
    pub fn write_all_defer_err(&mut self, buf: &[u8]) {
        let old_len = self.buf.len();
        // SAFETY add cannot overflow as both are at most `isize::MAX`.
        let new_len = old_len + buf.len();
        if new_len <= self.buf.capacity() {
            unsafe {
                // SAFETY this writes to `old_len..new_len` which we just checked to be within the
                // capacity of `self.buf`
                self.buf
                    .as_mut_ptr()
                    .add(old_len)
                    .copy_from_nonoverlapping(buf.as_ptr(), buf.len());
                self.buf.set_len(new_len)
            }
        } else {
            self.write_all_defer_err_cold(buf);
        }
    }

    #[inline(never)]
    #[cold]
    fn write_all_defer_err_cold(&mut self, mut buf: &[u8]) {
        // If the passed `buf` is small enough that we don't need an individual write for it alone,
        // fill our internal `buf` up to capacity.
        if buf.len() < self.buf.capacity() {
            // This assumes that we bailed out of the fast path in `write_all_defer_err`, otherwise
            // the index passed to split_at could be out of bounds and this would panic.
            let (buf_first, buf_second) = buf.split_at(self.buf.capacity() - self.buf.len());
            self.buf.extend_from_slice(buf_first);
            buf = buf_second;
        }

        // This will leaves us an empty buffer, even if an IO error occured.
        self.flush_defer_err();

        if buf.len() < self.buf.capacity() {
            self.buf.extend_from_slice(buf);
        } else {
            // If the passed `buf` does not fit into our internal `buf`, we directly write it, again
            // deferring any IO errors.

            // Silently discard data if we errored before but haven't reported it yet
            if self.io_error.is_none() {
                self.panicked = true;
                if let Err(err) = self.write.write_all(buf) {
                    self.io_error = Some(err);
                }
                self.panicked = false;
            }
        }
    }

    /// Returns a pointer to the current write pointer within the internal buffer if sufficient
    /// space is available.
    ///
    /// If only fewer than `len` bytes are available, this returns a null pointer.
    ///
    /// Can be used in conjunction with [`advance_unchecked`][Self::advance_unchecked] to construct
    /// output data directly within the output buffer, potentially avoiding a redundant copy.
    #[inline]
    pub fn buf_write_ptr(&mut self, len: usize) -> *mut u8 {
        let old_len = self.buf.len();
        // SAFETY add cannot overflow as both are at most `isize::MAX`.
        let new_len = old_len + len;
        if new_len <= self.buf.capacity() {
            // SAFETY this returns the offset to `old_len` which is always in range
            unsafe { self.buf.as_mut_ptr().add(old_len) }
        } else {
            std::ptr::null_mut()
        }
    }

    /// Advances the write pointer within the internal buffer.
    ///
    /// # Safety
    ///
    /// This assumes that a) there is sufficient space left in the buffer and b) that the bytes the
    /// pointer is advanced over were initialized prior to calling this (via
    /// [`buf_write_ptr`](Self::buf_write_ptr)).
    ///
    /// If either assumption does not hold calling this results in undefined behavior.
    #[inline]
    pub unsafe fn advance_unchecked(&mut self, len: usize) {
        let old_len = self.buf.len();
        // SAFETY add cannot overflow as both are at most `isize::MAX`.
        let new_len = old_len + len;
        debug_assert!(new_len <= self.buf.capacity());
        self.buf.set_len(new_len)
    }

    /// Returns an encountered IO errors as `Err(io_err)`.
    ///
    /// This resets the stored IO error and returns `Ok(())` if no IO error is stored.
    #[inline]
    pub fn check_io_error(&mut self) -> io::Result<()> {
        if let Some(err) = self.io_error.take() {
            Err(err)
        } else {
            Ok(())
        }
    }
}

impl Write for DeferredWriter<'_> {
    #[inline]
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.write_all_defer_err(buf);
        Ok(buf.len())
    }

    #[inline]
    fn flush(&mut self) -> io::Result<()> {
        self.flush_defer_err();
        self.check_io_error()
    }

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> io::Result<()> {
        self.write_all_defer_err(buf);
        Ok(())
    }
}

impl Drop for DeferredWriter<'_> {
    fn drop(&mut self) {
        if !self.panicked {
            self.flush_defer_err();
        }
    }
}
