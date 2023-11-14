use std::io::{self, BufReader, Cursor, Read};

/// A buffered reader optimized for efficient parsing.
///
/// Like `std`'s [`BufReader`], this provides buffering to coalesce many small reads into fewer
/// larger reads of the underlying data source. The difference is that `DeferredReader` is optimized
/// for efficient parsing. This includes asynchronous handling of IO errors, position tracking, and
/// dynamic contiguous look-ahead.
pub struct DeferredReader<'a> {
    read: Box<dyn Read + 'a>,
    buf: Vec<u8>,
    // SAFETY `buf[pos_in_buf..pos_in_buf+valid_len]` must _always_ be valid
    pos_in_buf: usize,
    valid_len: usize,
    complete: bool,
    io_error: Option<io::Error>,
    pos_of_buf: usize,
    mark_in_buf: usize,
    chunk_size: usize,
}

impl<'a> DeferredReader<'a> {
    const DEFAULT_CHUNK_SIZE: usize = 16 << 10;

    /// Creates a [`DeferredReader`] for the data of a [`BufReader`].
    pub fn from_buf_reader(buf_reader: BufReader<impl Read + 'a>) -> Self {
        // Avoid double buffering without discarding any already buffered contents.
        let buf_data = buf_reader.buffer().to_vec();
        if buf_data.is_empty() {
            Self::from_read(buf_reader.into_inner())
        } else {
            Self::from_read(Cursor::new(buf_data).chain(buf_reader.into_inner()))
        }
    }

    /// Creates a [`DeferredReader`] for the data of a [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    pub fn from_read(read: impl Read + 'a) -> Self {
        Self::from_boxed_dyn_read(Box::new(read))
    }

    /// Creates a [`DeferredReader`] for the data of a boxed [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    #[inline(never)]
    pub fn from_boxed_dyn_read(read: Box<dyn Read + 'a>) -> Self {
        DeferredReader {
            read,
            buf: vec![],
            pos_in_buf: 0,
            valid_len: 0,
            complete: false,
            io_error: None,
            pos_of_buf: 0,
            mark_in_buf: 0,
            chunk_size: Self::DEFAULT_CHUNK_SIZE,
        }
    }

    /// Sets the number of bytes that are read at once.
    ///
    /// This sets the size of the [`read`][Read::read] requests made. Note that this is just an
    /// upper bound. Depending on the [`Read`] implementation, smaller amounts may be read at once.
    /// To enable interactive line based input, `DeferredReader` on its own will not issue more read
    /// requests than necessary.
    pub fn set_chunk_size(&mut self, size: usize) {
        self.chunk_size = size;
    }

    /// Returns the currently buffered data in front of the cursor.
    ///
    /// You can call [`is_complete`][Self::is_complete] to check whether the returned data contains
    /// all remaining input data.
    #[inline]
    pub fn buf(&self) -> &[u8] {
        unsafe {
            // SAFETY `self.pos_in_buf..self.pos_in_buf + self.valid_len` are always kept within
            // range
            debug_assert!(self
                .buf
                .get(self.pos_in_buf..self.pos_in_buf + self.valid_len)
                .is_some());
            self.buf
                .get_unchecked(self.pos_in_buf..self.pos_in_buf + self.valid_len)
        }
    }

    /// Returns the length of the currently buffered data.
    ///
    /// This returns the same value as `reader.buf().len()` but unlike [`reader.buf()`][Self::buf]
    /// this does not create an intermediate reference to the buffered data. This can make a
    /// difference in safety when raw pointers are used to access the buffered data.
    #[inline]
    pub fn buf_len(&self) -> usize {
        self.valid_len
    }

    /// Returns a pointer to the currently buffered data.
    ///
    /// This returns the same value as `reader.buf().as_ptr()` but unlike
    /// [`reader.buf()`][Self::buf] this does not create an intermediate reference to the buffered
    /// data. You can use [`reader.buf_len()`][Self::buf_len] to obtain the length of the buffered
    /// data.
    #[inline]
    pub fn buf_ptr(&self) -> *const u8 {
        unsafe {
            // SAFETY `self.pos_in_buf` is always kept in range
            self.buf.as_ptr().add(self.pos_in_buf)
        }
    }

    /// Advances the cursor by a given number of already buffered bytes.
    ///
    /// This will panic if the number of bytes exceeds the amount of buffered data.
    #[inline]
    pub fn advance(&mut self, n: usize) {
        let (next_len, overflow) = self.valid_len.overflowing_sub(n);
        self.valid_len = next_len;
        if overflow {
            self.advance_cold();
        }
        self.pos_in_buf += n;
        // SAFETY ^ we already subtracted n from len and checked for overflow so we cannot overflow
        // the buffer here.
    }

    #[inline(never)]
    #[cold]
    fn advance_cold(&self) -> ! {
        panic!("advanced past the current buffer size");
    }

    /// Advances the cursor by a given number of already buffered bytes, returning a reference to
    /// those bytes.
    ///
    /// This will panic if the number of bytes exceeds the amount of buffered data.
    #[inline]
    pub fn advance_with_buf(&mut self, n: usize) -> &[u8] {
        self.advance(n);
        unsafe {
            // SAFETY since we just called advance which did not panic, we know that
            // `self.pos_in_buf` was just advanced by `n` bytes, pointing into a valid buffer before
            // and after.
            debug_assert!(self.buf.get(self.pos_in_buf - n..self.pos_in_buf).is_some());
            self.buf.get_unchecked(self.pos_in_buf - n..self.pos_in_buf)
        }
    }

    /// Advances the cursor by a given number of already buffered bytes without checking if
    /// sufficient bytes are buffered.
    ///
    /// # Safety
    ///
    /// The passed value for `n` may not exceed the value returned by [`buf_len()`][Self::buf_len].
    #[inline]
    pub unsafe fn advance_unchecked(&mut self, n: usize) {
        debug_assert!(self.valid_len >= n);
        self.valid_len -= n;
        self.pos_in_buf += n;
    }

    /// Total number of bytes the cursor was advanced so far.
    ///
    /// This wraps around every `usize::MAX` bytes.
    #[inline]
    pub fn position(&self) -> usize {
        self.pos_of_buf.wrapping_add(self.pos_in_buf)
    }

    /// Returns currently marked position.
    ///
    /// Initially this is position `0`, but can be changed using [`set_mark`][Self::set_mark] and
    /// [`set_mark_to_position`][Self::set_mark_to_position].
    ///
    /// Setting the mark to the start of a token before advancing over it can be useful for error
    /// reporting.
    #[inline]
    pub fn mark(&self) -> usize {
        self.pos_of_buf.wrapping_add(self.mark_in_buf)
    }

    /// Marks the current position.
    ///
    /// Calling this will make [`mark`](Self::mark) return the current position.
    #[inline]
    pub fn set_mark(&mut self) {
        self.mark_in_buf = self.pos_in_buf
    }

    /// Sets the position returned by [`mark`](Self::mark).
    #[inline]
    pub fn set_mark_to_position(&mut self, position: usize) {
        self.mark_in_buf = position.wrapping_sub(self.pos_of_buf)
    }

    /// Returns whether all remaining data is buffered.
    ///
    /// If this returns `true` [`buf`][Self::buf] will contain all the remaining data. This can
    /// happen when the end was reached or when an IO error was encountered. You can use
    /// [`check_io_error`][Self::check_io_error] to determine whether an IO error occured.
    #[inline]
    pub fn is_complete(&self) -> bool {
        self.complete
    }

    /// Returns whether the cursor is at the end of the available data.
    ///
    /// This can be the end of the input or all data before an IO error was encountered. You can
    /// use [`check_io_error`][Self::check_io_error] to determine whether an IO error occured.
    #[inline]
    pub fn is_at_end(&self) -> bool {
        self.complete && (self.valid_len == 0)
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

    /// Returns a reference to an encountered IO error.
    ///
    /// This does not reset the stored IO error and erturns `None` if no IO error is stored.
    #[inline]
    pub fn io_error(&self) -> Option<&io::Error> {
        self.io_error.as_ref()
    }

    /// Tries to extend the buffer by reading more data until it reaches the requested length.
    ///
    /// Returns a slice to _all_ of the buffered data, not only the requested amount.
    ///
    /// This fails when the end of the input is reached or an IO error occured before enough
    /// data was read, in which case a smaller buffer than requested is returned.
    #[inline]
    pub fn request(&mut self, len: usize) -> &[u8] {
        if self.valid_len < len {
            self.request_cold(len);
        }
        self.buf()
    }

    #[cold]
    #[inline(never)]
    fn request_cold(&mut self, len: usize) {
        while self.valid_len < len && self.request_more() {}
    }

    /// Tries to extend the buffer by reading more data until it contains at least one byte.
    ///
    /// Returns the next byte.
    ///
    /// This fails when the end of the input is reached or an IO error occured before enough
    /// data was read, in which case `None` is returned.
    #[inline]
    pub fn request_byte(&mut self) -> Option<u8> {
        self.request_byte_at_offset(0)
    }

    /// Tries to extend the buffer by reading more data until it contains at least the byte at the
    /// given offset from the current position.
    ///
    /// Returns that byte.
    ///
    /// This fails when the end of the input is reached or an IO error occured before enough data
    /// was read, in which case `None` is returned.
    #[inline]
    pub fn request_byte_at_offset(&mut self, offset: usize) -> Option<u8> {
        if offset < self.valid_len {
            unsafe {
                // SAFETY within `pos_in_buf..pos_in_buf + valid_len`
                Some(*self.buf.get_unchecked(self.pos_in_buf + offset))
            }
        } else {
            self.request_byte_at_offset_cold(offset)
        }
    }

    #[cold]
    #[inline(never)]
    fn request_byte_at_offset_cold(&mut self, offset: usize) -> Option<u8> {
        while self.valid_len <= offset {
            if !self.request_more() {
                return None;
            }
        }
        Some(self.buf[self.pos_in_buf + offset])
    }

    /// Tries to extend the buffer by reading more data.
    #[cold]
    #[inline(never)]
    pub fn request_more(&mut self) -> bool {
        if self.complete {
            return false;
        }

        // Only realign if we have advanced over sufficiently more data to
        let realign = self.pos_in_buf > self.chunk_size * 2;

        if realign {
            self.buf
                .copy_within(self.pos_in_buf..self.pos_in_buf + self.valid_len, 0);
            self.pos_of_buf = self.pos_of_buf.wrapping_add(self.pos_in_buf);
            self.pos_in_buf = 0;
            self.mark_in_buf = self.mark_in_buf.wrapping_sub(self.pos_in_buf);

            // If our buffer is four times as large as it needs to be for the current data and an
            // additional chunk, shrink it.
            if self.buf.len() > 4 * (self.pos_in_buf + self.valid_len + self.chunk_size) {
                self.buf.truncate(self.buf.len() / 2);
                self.buf.shrink_to_fit();
            }
        }

        let target_end = self.pos_in_buf + self.valid_len + self.chunk_size;

        // Make sure we have enough buffer space for another chunk
        if self.buf.len() < target_end {
            self.buf.resize(target_end, 0);
        }

        // Do only a single successful read (to make line buffered repls usable), but do retry on
        // `Interrupted`.
        loop {
            match self
                .read
                .read(&mut self.buf[self.pos_in_buf + self.valid_len..target_end])
            {
                Ok(0) => self.complete = true,
                Ok(n) => {
                    // SAFETY this assert is load bearing, as `self.valid_len` is trusted but Read
                    // implementations aren't
                    assert!(
                        n <= self.chunk_size,
                        "invariant of std::io::Read trait violated"
                    );
                    self.valid_len += n
                }
                Err(err) if err.kind() == io::ErrorKind::Interrupted => continue,
                Err(err) => {
                    self.io_error = Some(err);
                    self.complete = true;
                }
            }
            break;
        }

        true
    }
}
