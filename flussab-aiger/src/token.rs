use flussab::{
    text::{self, LineReader},
    Parsed::{self, Fallthrough, Res},
};
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingMul},
    FromPrimitive, Zero,
};

use crate::ParseError;

#[cold]
pub fn unexpected(input: &mut LineReader, expected: &str) -> ParseError {
    let mut unexpected_bytes = vec![];

    if text::newline(input.reader(), 0) != 0 {
        return input.give_up(format!("expected {expected}, found end of line"));
    } else if input.reader.is_at_end() {
        return input.give_up(format!("expected {expected}, found end of file"));
    }

    while unexpected_bytes.len() < 60 {
        match input.reader.request_byte_at_offset(unexpected_bytes.len()) {
            Some(b'\n') | Some(b'\r') | Some(b'\t') | Some(b' ')
                if !unexpected_bytes.is_empty() =>
            {
                break
            }
            None => break,
            Some(byte) => unexpected_bytes.push(byte),
        }
    }

    input.give_up(format!(
        "expected {}, found {:?}",
        expected,
        String::from_utf8_lossy(&unexpected_bytes)
    ))
}

/// Parses a fixed sequence of bytes without consuming any following spaces.
#[inline]
pub fn fixed(input: &mut LineReader, fixed: &[u8]) -> Parsed<(), ParseError> {
    let offset = text::fixed(input.reader(), 0, fixed);
    if offset != 0 {
        input.reader.advance(offset);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Parses a fixed sequence of bytes without consuming any following spaces unless the token would end the line.
#[inline]
pub fn fixed_not_eol(input: &mut LineReader, fixed: &[u8]) -> Parsed<(), ParseError> {
    let offset = text::fixed(input.reader(), 0, fixed);
    if offset != 0 && !matches!(input.reader().request_byte_at_offset(offset), Some(b'\n')) {
        input.reader.advance(offset);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Parses a single space.
#[inline]
pub fn space(input: &mut LineReader) -> Parsed<(), ParseError> {
    if matches!(input.reader.request_byte(), Some(b' ')) {
        input.reader.advance(1);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Parses a required single space.
#[inline]
pub fn required_space(input: &mut LineReader) -> Result<(), ParseError> {
    space(input).or_give_up(|| unexpected(input, "a space character"))
}

/// Parses a single newline.
#[inline]
pub fn newline(input: &mut LineReader) -> Parsed<(), ParseError> {
    if matches!(input.reader.request_byte(), Some(b'\n')) {
        input.reader.advance(1);
        input.line_at_offset(0);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Parses a required single newline.
#[inline]
pub fn required_newline(input: &mut LineReader) -> Result<(), ParseError> {
    newline(input).or_give_up(|| unexpected(input, "a newline"))
}

/// Parses a required single newline or space, returns whether a space was found.
#[inline]
pub fn required_newline_or_space(input: &mut LineReader) -> Result<bool, ParseError> {
    let byte = input.reader.request_byte();
    if matches!(byte, Some(b'\n' | b' ')) {
        input.reader.advance(1);
        if matches!(byte, Some(b'\n')) {
            input.line_at_offset(0);
            Ok(false)
        } else {
            Ok(true)
        }
    } else {
        Err(unexpected(input, "a space or newline"))
    }
}

/// Parses a non-negative integer.
#[inline]
pub fn uint<T>(input: &mut LineReader) -> Parsed<T, String>
where
    T: Zero + FromPrimitive + OverflowingAdd + OverflowingMul,
{
    let (value, offset) = text::ascii_digits_multi(input.reader(), 0);
    if offset != 0 {
        if input.reader.buf()[0] != b'0' || offset == 1 {
            if let Some(value) = value {
                input.reader.advance(offset);
                return Res(Ok(value));
            }
        }

        Res(Err(std::str::from_utf8(&input.reader.buf()[..offset])
            .unwrap()
            .to_owned()))
    } else {
        Fallthrough
    }
}

#[cold]
fn binary_uint(input: &mut LineReader) -> Result<usize, ParseError> {
    // TODO optimize this
    let reader = input.reader();

    let mut byte_len = 0;

    loop {
        if let Some(byte) = reader.request_byte_at_offset(byte_len) {
            byte_len += 1;
            if byte & 0x80 == 0 {
                break;
            }
            if byte_len == (usize::BITS as usize + 7) / 8 {
                return Err(input.give_up(
                    "binary encoded value uses more bytes than required for any supported value",
                ));
            }
        } else {
            return Err(unexpected(input, "end of file"));
        }
    }

    let mut value = 0;

    for byte in reader.buf()[..byte_len].iter().rev() {
        let next_value = value << 7;
        if next_value >> 7 != value {
            return Err(input.give_up("binary encoded value is larger than supported"));
        }
        value = next_value | (byte & 0x7f) as usize;
    }

    reader.advance(byte_len);

    Ok(value)
}

#[inline]
pub fn delta_code(
    input: &mut LineReader,
    code: usize,
    target: &str,
    reference: &str,
) -> Result<usize, ParseError> {
    input.reader().set_mark();
    let delta = binary_uint(input)?;

    if delta > code {
        return delta_code_err(input, code, delta, target, reference);
    }

    Ok(code - delta)
}

#[cold]
pub fn delta_code_err(
    input: &mut LineReader,
    code: usize,
    delta: usize,
    target: &str,
    reference: &str,
) -> Result<usize, ParseError> {
    Err(input.give_up_at(
        input.reader.mark(),
        format!("encoded delta {delta} for {target} is larger than {reference} {code}"),
    ))
}

#[inline]
pub fn header_field(
    input: &mut LineReader,
    name: &str,
    limit: usize,
    hard_limit: bool,
) -> Result<usize, ParseError> {
    input.reader.set_mark();
    uint::<usize>(input)
        .map_err(|count| {
            exceeds_count(
                input,
                name,
                "remaining available variable count",
                &count,
                limit,
                hard_limit,
            )
        })
        .and_also(|&mut count| {
            if count > limit {
                Err(exceeds_count(
                    input,
                    name,
                    "remaining available variable count",
                    &count.to_string(),
                    limit,
                    hard_limit,
                ))
            } else {
                Ok(())
            }
        })
        .or_give_up(|| unexpected(input, name))
}

#[cold]
#[inline(never)]
pub fn exceeds_count(
    input: &mut LineReader,
    what: &str,
    limit_name: &str,
    value: &str,
    limit: usize,
    hard_limit: bool,
) -> ParseError {
    if value.starts_with('0') {
        input.give_up_at(
            input.reader.mark(),
            format!("{what} {value} has leading zeros which are not allowed in AIGER files"),
        )
    } else {
        input.give_up_at(
            input.reader.mark(),
            format!(
                "{} {} exceeds the {} {} of {}",
                what,
                value,
                if hard_limit { "supported" } else { "specified" },
                limit_name,
                limit
            ),
        )
    }
}

#[cold]
#[inline(never)]
pub fn not_assigning(
    input: &mut LineReader,
    int_value: usize,
    what: &str,
    value: &str,
) -> ParseError {
    if int_value == 0 {
        input.give_up_at(
            input.reader.mark(),
            format!("{what} {value} is constant false"),
        )
    } else {
        input.give_up_at(
            input.reader.mark(),
            format!("{what} {value} is negated (odd)"),
        )
    }
}

#[cold]
#[inline(never)]
pub fn invalid_initialization(input: &mut LineReader, found: usize, latch: usize) -> ParseError {
    input.give_up_at(
        input.reader.mark(),
        format!(
            "invalid initialization literal {found} for latch {latch}, expected 0, 1 or {latch}"
        ),
    )
}

#[inline]
pub fn lit(
    input: &mut LineReader,
    name: &str,
    limit: usize,
    assigning: bool,
) -> Result<usize, ParseError> {
    input.reader.set_mark();
    uint::<usize>(input)
        .map_err(|count| exceeds_count(input, name, "maximum literal", &count, limit, false))
        .and_also(|&mut count| {
            if assigning && (count == 0 || count & 1 != 0) {
                Err(not_assigning(input, count, name, &count.to_string()))
            } else if count > limit {
                Err(exceeds_count(
                    input,
                    name,
                    "maximum literal",
                    &count.to_string(),
                    limit,
                    false,
                ))
            } else {
                Ok(())
            }
        })
        .or_give_up(|| unexpected(input, name))
}

#[inline]
pub fn symbol_index(input: &mut LineReader, name: &str, limit: usize) -> Result<usize, ParseError> {
    input.reader.set_mark();
    uint::<usize>(input)
        .map_err(|count| exceeds_count(input, name, "count", &count, limit, false))
        .and_also(|&mut count| {
            if count > limit {
                Err(exceeds_count(
                    input,
                    name,
                    "count",
                    &count.to_string(),
                    limit,
                    false,
                ))
            } else {
                Ok(())
            }
        })
        .or_give_up(|| unexpected(input, name))
}

#[inline]
pub fn remaining_line_content<'a>(input: &'a mut LineReader) -> Result<&'a str, ParseError> {
    let mut offset = 0;
    while !matches!(
        input.reader.request_byte_at_offset(offset),
        Some(b'\n') | None
    ) {
        offset += 1;
    }

    if input.reader.request_byte_at_offset(offset).is_none() {
        input.reader.advance(offset);
        return Err(unexpected(input, "a newline"));
    }
    input.line_at_offset(offset + 1);

    let bytes = &input.reader.buf()[..offset];

    match std::str::from_utf8(bytes) {
        Ok(_line) => {
            // SAFETY we just checked this,
            Ok(unsafe {
                std::str::from_utf8_unchecked(&input.reader.advance_with_buf(offset + 1)[..offset])
            })
        }
        Err(err) => {
            input.reader.advance(err.valid_up_to());
            Err(unexpected(input, "a valid utf-8 character"))
        }
    }
}

#[inline]
pub fn remaining_file_content<'a>(input: &'a mut LineReader) -> Result<&'a str, ParseError> {
    while input
        .reader
        .request_byte_at_offset(input.reader.buf_len())
        .is_some()
    {}

    let bytes = input.reader.buf();

    match (std::str::from_utf8(bytes), bytes.last()) {
        (Ok(_), Some(b'\n')) | (Ok(""), None) => {
            let content_len = bytes.len().saturating_sub(1);

            // SAFETY we just checked this,
            Ok(unsafe {
                std::str::from_utf8_unchecked(
                    &input.reader.advance_with_buf(input.reader.buf_len())[..content_len],
                )
            })
        }
        (err, _) => {
            let (mut valid_up_to, expected) = if let Err(err) = err {
                (err.valid_up_to(), "a valid utf-8 character")
            } else {
                (bytes.len(), "a final newline")
            };
            let bytes = &bytes[..valid_up_to];

            if let Some(last_line_bytes) = bytes.iter().rev().position(|&b| b == b'\n') {
                let advance = bytes.len() - last_line_bytes;
                let skip_lines = bytes[..advance - 1].iter().filter(|&&b| b == b'\n').count();
                input.line += skip_lines;
                input.reader.advance(advance);
                input.line_at_offset(0);
                valid_up_to -= advance;
            }

            input.reader.advance(valid_up_to);
            Err(unexpected(input, expected))
        }
    }
}

#[inline]
pub fn eof(input: &mut LineReader) -> Parsed<(), ParseError> {
    if input.reader.request_byte().is_none() && input.reader.io_error().is_none() {
        Res(Ok(()))
    } else {
        Fallthrough
    }
}
