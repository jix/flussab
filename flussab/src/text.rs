//! Utilities for parsing text based formats using a [`DeferredReader`].
use std::{fmt, io};

use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub},
    FromPrimitive, Zero,
};

use crate::DeferredReader;

/// Passes over ASCII digits and parses them as decimal number.
///
/// Increments `offset` as long as it points to an ASCII digit. It returns the value of the digits
/// parsed as decimal number (or `None` on overflow) and the resulting `offset`.
#[inline]
pub fn ascii_digits<I>(reader: &mut DeferredReader, mut offset: usize) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingMul,
{
    let mut value = I::zero();
    let mut overflow = false;

    while let Some(digit @ b'0'..=b'9') = reader.request_byte_at_offset(offset) {
        offset += 1;

        let (new_value, overflowed) = value.overflowing_mul(&I::from_u8(10).unwrap());
        overflow |= overflowed;
        value = new_value;

        let (new_value, overflowed) = value.overflowing_add(&I::from_u8(digit - b'0').unwrap());
        overflow |= overflowed;
        value = new_value;
    }

    ((!overflow).then_some(value), offset)
}

#[cold]
#[inline(never)]
/// Used as positive cold path for SWAR variants
fn ascii_digits_cont_pos<I>(
    reader: &mut DeferredReader,
    mut offset: usize,
    value: Option<I>,
) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingMul,
{
    #![allow(clippy::or_fun_call)]
    let mut overflow = value.is_none();
    let mut value = value.unwrap_or(I::zero());

    while let Some(digit @ b'0'..=b'9') = reader.request_byte_at_offset(offset) {
        offset += 1;

        let (new_value, overflowed) = value.overflowing_mul(&I::from_u8(10).unwrap());
        overflow |= overflowed;
        value = new_value;

        let (new_value, overflowed) = value.overflowing_add(&I::from_u8(digit - b'0').unwrap());
        overflow |= overflowed;
        value = new_value;
    }

    ((!overflow).then_some(value), offset)
}

#[cold]
#[inline(never)]
/// Used as negative cold path for SWAR variants
fn ascii_digits_cont_neg<I>(
    reader: &mut DeferredReader,
    mut offset: usize,
    value: Option<I>,
) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingSub + OverflowingMul,
{
    #![allow(clippy::or_fun_call)]
    let mut overflow = value.is_none();
    let mut value = value.unwrap_or(I::zero());

    while let Some(digit @ b'0'..=b'9') = reader.request_byte_at_offset(offset) {
        offset += 1;

        let (new_value, overflowed) = value.overflowing_mul(&I::from_u8(10).unwrap());
        overflow |= overflowed;
        value = new_value;

        let (new_value, overflowed) = value.overflowing_sub(&I::from_u8(digit - b'0').unwrap());
        overflow |= overflowed;
        value = new_value;
    }

    ((!overflow).then_some(value), offset)
}

/// Passes over ASCII digits, optionally prefixed by `'-`', and parses them as decimal number.
///
/// If `offset` points to a `'-'` character followed by at least one digit, this returns
/// `ascii_digits(input, offset + 1)` with the parsed value negated (except that `None` is only
/// returned if the final result would overflow). Otherwise this returns `ascii_digits(input,
/// offset)`. In particular this means that this will not advance over a lone `'-'` character that
/// is not followed by a digit.
///
/// This also does not handle an explicit positive sign `'+'`.
#[inline]
pub fn signed_ascii_digits<I>(reader: &mut DeferredReader, mut offset: usize) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingSub + OverflowingMul,
{
    let mut value = I::zero();
    let mut overflow = false;
    if let Some(b'-') = reader.request_byte_at_offset(offset) {
        if let Some(digit @ b'0'..=b'9') = reader.request_byte_at_offset(offset + 1) {
            value = I::zero() - (I::from_u8(digit - b'0').unwrap());
            offset += 2;
            while let Some(digit @ b'0'..=b'9') = reader.request_byte_at_offset(offset) {
                offset += 1;

                let (new_value, overflowed) = value.overflowing_mul(&I::from_u8(10).unwrap());
                overflow |= overflowed;
                value = new_value;

                let (new_value, overflowed) =
                    value.overflowing_sub(&I::from_u8(digit - b'0').unwrap());
                overflow |= overflowed;
                value = new_value;
            }
        }
    } else {
        while let Some(digit @ b'0'..=b'9') = reader.request_byte_at_offset(offset) {
            offset += 1;

            let (new_value, overflowed) = value.overflowing_mul(&I::from_u8(10).unwrap());
            overflow |= overflowed;
            value = new_value;

            let (new_value, overflowed) = value.overflowing_add(&I::from_u8(digit - b'0').unwrap());
            overflow |= overflowed;
            value = new_value;
        }
    }

    ((!overflow).then_some(value), offset)
}

/// Optimized version of `ascii_digits`.
///
/// This is equivalent to `ascii_digits` but tries to process more bytes at once. Depending on the
/// size distribution of the parsed numbers this can be faster or slower than `ascii_digits`, so
/// both variants are provided.
#[inline]
pub fn ascii_digits_multi<I>(reader: &mut DeferredReader, offset: usize) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingMul,
{
    #![allow(clippy::or_fun_call)]
    if reader.buf_len() < offset + 8 {
        return ascii_digits_multi_cold(reader, offset);
    }
    let word = unsafe { u64::from_le_bytes(*(reader.buf_ptr().add(offset) as *const [u8; 8])) };

    let (value, matching_digits) = swar_ascii_digits_u64_le(word);

    let value = I::from_u32(value);

    if matching_digits == 8 {
        return ascii_digits_cont_pos(reader, offset + 8, value);
    }

    (value, offset + matching_digits)
}

#[cold]
#[inline(never)]
fn ascii_digits_multi_cold<I>(reader: &mut DeferredReader, offset: usize) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingMul,
{
    ascii_digits(reader, offset)
}

/// Optimized version of `signed_ascii_digits`.
///
/// This is equivalent to `signed_ascii_digits` but tries to process more bytes at once. Depending
/// on the size distribution of the parsed numbers this can be faster or slower than
/// `signed_ascii_digits`, so both variants are provided.
#[inline]
pub fn signed_ascii_digits_multi<I>(
    reader: &mut DeferredReader,
    offset: usize,
) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingSub + OverflowingMul,
{
    #![allow(clippy::or_fun_call)]
    if reader.buf_len() < offset + 8 {
        return signed_ascii_digits_multi_cold(reader, offset);
    }
    let word = unsafe { u64::from_le_bytes(*(reader.buf_ptr().add(offset) as *const [u8; 8])) };

    if word & 0xff == b'-' as u64 {
        let word = word >> 8;

        let (value, matching_digits) = swar_ascii_digits_u64_le(word);

        let value = I::from_i32(-(value as i32));

        if matching_digits == 7 {
            return ascii_digits_cont_neg(reader, offset + 8, value);
        }

        (
            value,
            offset + ((matching_digits != 0) as usize) + matching_digits,
        )
    } else {
        let (value, matching_digits) = swar_ascii_digits_u64_le(word);

        let value = I::from_u32(value);

        if matching_digits == 8 {
            return ascii_digits_cont_pos(reader, offset + 8, value);
        }

        (value, offset + matching_digits)
    }
}

#[cold]
#[inline(never)]
fn signed_ascii_digits_multi_cold<I>(
    reader: &mut DeferredReader,
    offset: usize,
) -> (Option<I>, usize)
where
    I: Zero + FromPrimitive + OverflowingAdd + OverflowingSub + OverflowingMul,
{
    signed_ascii_digits(reader, offset)
}

#[inline]
fn swar_ascii_digits_u64_le(word: u64) -> (u32, usize) {
    // Iff the high nibble of a byte cannot match a digit, produce a non-zero high nibble (low
    // nibble arbitrary).
    let high_nibble_matches = word ^ 0x3030303030303030;

    let low_nibbles = word & 0x0f0f0f0f0f0f0f0f;

    // Iff the low nibble of a byte cannot match a digit, produce a non-zero high nibble (low nibble
    // again arbitrary).
    let low_nibble_matches = low_nibbles.wrapping_add(0x0606060606060606);

    // Combine both values and discard the arbitrary low nibbles. An input byte is a digit iff the
    // resulting byte is zero.
    let matches = (high_nibble_matches | low_nibble_matches) & 0xf0f0f0f0f0f0f0f0;

    // This is 8 times the amount of matching digits
    let shift = matches.trailing_zeros() & !7;

    if shift == 0 {
        // Otherwise the shift of low_nibbles would overflow
        return (0, 0);
    }

    // Sum up pairs, groups of 4 and then groups of 8 digits
    let partial = (low_nibbles << (64 - shift)).wrapping_mul(2561) >> 8;
    let partial = (partial & 0x00ff00ff00ff00ff).wrapping_mul(6553601) >> 16;
    let value = (partial & 0x0000ffff0000ffff).wrapping_mul(42949672960001) >> 32;

    (value as u32, (shift / 8) as usize)
}

/// Passes over tab and space characters.
///
/// Increments `offset` as long as it points to either a tab (`'\t'`) or a space (`' '`) character
/// and returns the resulting value.
#[inline]
pub fn tabs_or_spaces(input: &mut DeferredReader, mut offset: usize) -> usize {
    while let Some(b' ') | Some(b'\t') = input.request_byte_at_offset(offset) {
        offset += 1;
    }
    offset
}

/// Passes over a single newline if present.
///
/// This increments `offset` by 1 if it points to `"\n"` and by 2 if it points to `"\r\n"`, leaving
/// it unchanged otherwise. It returns the resulting value.
#[inline]
pub fn newline(input: &mut DeferredReader, offset: usize) -> usize {
    match input.request_byte_at_offset(offset) {
        Some(b'\n') => offset + 1,
        Some(b'\r') if matches!(input.request_byte_at_offset(offset + 1), Some(b'\n')) => {
            offset + 2
        }
        _ => offset,
    }
}

/// Passes over the next newline.
///
/// This increments `offset` until it finds a [`newline`], over which it also passes, or reaches the
/// end of the input. It returns the resulting value.
#[inline]
pub fn next_newline(input: &mut DeferredReader, mut offset: usize) -> usize {
    // TODO this can be made faster, but wasn't important for the formats I implemented so far
    while !matches!(input.request_byte_at_offset(offset), Some(b'\n') | None) {
        offset += 1;
    }
    offset + input.request_byte_at_offset(offset).is_some() as usize
}

/// Passes over a fixed sequence if present.
///
/// If the following data matches `fixed`, `offset` is incremented by the length of `fixed`,
/// otherwise it is left unchanged. The resulting value is returned.
///
/// This does not read more data than the length of `fixed` or up to the first byte that does not
/// match `fixed`, whichever comes first.
#[inline]
pub fn fixed(input: &mut DeferredReader, offset: usize, fixed: &[u8]) -> usize {
    // TODO can also be made faster, especially if `fixed` has a size known at compile time
    for (i, &byte) in fixed.iter().enumerate() {
        if input.request_byte_at_offset(offset + i) != Some(byte) {
            return offset;
        }
    }
    offset + fixed.len()
}

/// Source location consisting of a line and column number.
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub struct LineColumn {
    /// The source line.
    ///
    /// This follows the convention where the first line is line `1`.
    pub line: usize,
    /// The (byte based) source column.
    ///
    /// Note that for UTF-8 input this can differ from both the number of codepoints as well as the
    /// column when the output is displayed using a monospace font. Both these alternatives are used
    /// as columns by various tools, but require keeping track of more data than [`LineReader`]
    /// does.
    ///
    /// This follows the convention where the first column is column `1`.
    pub column: usize,
}

impl fmt::Display for LineColumn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A simple syntax error type for text based formats.
#[derive(Debug)]
pub struct SyntaxError {
    /// The source location of the error.
    pub location: LineColumn,
    /// The error message.
    pub msg: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.location, self.msg)
    }
}

impl std::error::Error for SyntaxError {}

/// Wraps a [`DeferredReader`] to count track lines.
///
/// For performance reasons, counting lines isn't done automatically. Instead, you are expected to
/// read directly from the underlying [`DeferredReader`] and to call
/// [`line_at_offset`](Self::line_at_offset) whenever you advance past the end of a line.
///
/// If this is done, [`give_up`](Self::give_up) can be used to generate `SyntaxError`'s that contain
/// the source location and display the line containing the syntax error.
pub struct LineReader<'a> {
    /// The wrapped `DeferredReader`.
    pub reader: DeferredReader<'a>,
    /// The current line, starting at `1`.
    pub line: usize,
    /// The position where the current line starts, as returned by [`DeferredReader::position`].
    pub line_start: usize,
}

impl<'a> From<DeferredReader<'a>> for LineReader<'a> {
    fn from(reader: DeferredReader<'a>) -> Self {
        Self::new(reader)
    }
}

impl<'a> LineReader<'a> {
    /// Creates a `LineReader` assuming line 1 starts at the current position of the passed
    /// [`DeferredReader`].
    pub fn new(reader: DeferredReader<'a>) -> Self {
        Self {
            line: 1,
            line_start: reader.position(),
            reader,
        }
    }

    /// Returns a mutable reference to the [`DeferredReader`].
    ///
    /// While the `reader` field is directly accessible, using this method can be more convenient if
    /// a mutable reference is needed.
    #[inline]
    pub fn reader(&mut self) -> &mut DeferredReader<'a> {
        &mut self.reader
    }

    /// Advance the line counter and record the start of a new line.
    ///
    /// The `offset` value is relative to the current [`position()`][DeferredReader::position] as
    /// returned by `self.reader`.
    #[inline]
    pub fn line_at_offset(&mut self, offset: usize) {
        self.line += 1;
        self.line_start = self.reader.position() + offset;
    }

    /// Generate a syntax error at the current reader position.
    #[inline]
    pub fn give_up<E>(&mut self, msg: impl Into<String>) -> E
    where
        E: From<io::Error> + From<SyntaxError>,
    {
        self.give_up_at_cold(self.reader.position(), msg.into())
    }

    /// Generate a syntax error at `position`.
    ///
    /// This assumes that `position` is on the current line, otherwise the resulting `LineColumn`
    /// will be incorrect.
    #[inline]
    pub fn give_up_at<E>(&mut self, position: usize, msg: impl Into<String>) -> E
    where
        E: From<io::Error> + From<SyntaxError>,
    {
        self.give_up_at_cold(position, msg.into())
    }

    #[cold]
    #[inline(never)]
    fn give_up_at_cold<E>(&mut self, position: usize, msg: String) -> E
    where
        E: From<io::Error> + From<SyntaxError>,
    {
        if let Err(err) = self.reader.check_io_error() {
            return err.into();
        }

        (SyntaxError {
            location: LineColumn {
                line: self.line,
                column: position - self.line_start + 1,
            },
            msg,
        })
        .into()
    }
}
