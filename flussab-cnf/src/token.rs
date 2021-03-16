use std::fmt::Display;

use flussab::{
    text::{self, LineReader},
    Parsed,
};
use num_traits::{
    ops::overflowing::{OverflowingAdd, OverflowingMul, OverflowingSub},
    Bounded, FromPrimitive, Zero,
};
use Parsed::{Fallthrough, Res};

use crate::{error::ParseError, Dimacs};

/// Returns true when the next character is one of `" \t\r\n"` or we are at the EOF.
#[inline]
pub fn is_end_of_word(input: &mut LineReader, offset: usize) -> bool {
    matches!(
        input.reader.request_byte_at_offset(offset),
        Some(b' ') | Some(b'\t') | Some(b'\r') | Some(b'\n') | None
    )
}

/// Parses a fixed sequence of bytes ending at an [end of a word][is_end_of_word].
#[inline]
pub fn word(input: &mut LineReader, fixed: &[u8]) -> Parsed<(), ParseError> {
    let offset = text::fixed(input.reader(), 0, fixed);
    if offset != 0 && is_end_of_word(input, offset) {
        let offset = text::tabs_or_spaces(input.reader(), offset);
        input.reader.advance(offset);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

/// Parses a non-negative integer.
#[inline]
pub fn uint<T>(input: &mut LineReader) -> Parsed<T, String>
where
    T: Zero + FromPrimitive + OverflowingAdd + OverflowingMul,
{
    let (value, offset) = text::ascii_digits_multi(input.reader(), 0);
    if offset != 0 && is_end_of_word(input, offset) {
        if let Some(value) = value {
            let offset = text::tabs_or_spaces(input.reader(), offset);
            input.reader.advance(offset);
            Res(Ok(value))
        } else {
            Res(Err(std::str::from_utf8(&input.reader.buf()[..offset])
                .unwrap()
                .to_owned()))
        }
    } else {
        Fallthrough
    }
}

/// Parses an integer.
#[inline]
pub fn int<T>(input: &mut LineReader) -> Parsed<T, String>
where
    T: Zero + FromPrimitive + OverflowingAdd + OverflowingSub + OverflowingMul,
{
    let (value, offset) = text::signed_ascii_digits_multi(input.reader(), 0);
    if offset != 0 && is_end_of_word(input, offset) {
        if let Some(value) = value {
            let offset = text::tabs_or_spaces(input.reader(), offset);
            input.reader.advance(offset);
            Res(Ok(value))
        } else {
            Res(Err(std::str::from_utf8(&input.reader.buf()[..offset])
                .unwrap()
                .to_owned()))
        }
    } else {
        Fallthrough
    }
}

#[inline]
pub fn comment(input: &mut LineReader) -> Parsed<(), ParseError> {
    if let Some(b'c') = input.reader.request_byte() {
        let offset = text::next_newline(input.reader(), 1);
        input.line_at_offset(offset);
        let offset = text::tabs_or_spaces(input.reader(), offset);
        input.reader.advance(offset);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

#[inline]
pub fn newline(input: &mut LineReader) -> Parsed<(), ParseError> {
    let offset = text::newline(input.reader(), 0);
    if offset != 0 {
        input.line_at_offset(offset);
        let offset = text::tabs_or_spaces(input.reader(), offset);
        input.reader.advance(offset);

        Res(Ok(()))
    } else {
        Fallthrough
    }
}

#[inline]
pub fn interactive_newline(input: &mut LineReader) -> Parsed<(), ParseError> {
    let offset = text::newline(input.reader(), 0);
    if offset != 0 {
        input.line_at_offset(offset);
        input.reader.advance(offset);

        Res(Ok(()))
    } else {
        Fallthrough
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

#[inline]
pub fn interactive_end_of_line(input: &mut LineReader) -> Parsed<(), ParseError> {
    interactive_newline(input).or_parse(|| eof(input))
}

#[inline]
pub fn skip_whitespace(input: &mut LineReader) {
    let skip = text::tabs_or_spaces(input.reader(), 0);
    input.reader.advance(skip);
}

#[inline]
pub fn unexpected(input: &mut LineReader, expected: &str) -> ParseError {
    let mut unexpected_bytes = vec![];

    if text::newline(input.reader(), 0) != 0 {
        return input.give_up(format!("expected {}, found end of line", expected));
    } else if input.reader.is_at_end() {
        return input.give_up(format!("expected {}, found end of file", expected));
    }

    while unexpected_bytes.len() < 60 {
        match input.reader.request_byte_at_offset(unexpected_bytes.len()) {
            Some(b'\n') | Some(b'\r') | Some(b'\t') | Some(b' ') | None => break,
            Some(byte) => unexpected_bytes.push(byte),
        }
    }

    input.give_up(format!(
        "expected {}, found {:?}",
        expected,
        String::from_utf8_lossy(&unexpected_bytes)
    ))
}

#[cold]
#[inline(never)]
pub fn exceeds_var_count(
    input: &mut LineReader,
    what: &str,
    value: impl Display,
    limit: isize,
    hard_limit: bool,
) -> ParseError {
    input.give_up_at(
        input.reader.mark(),
        format!(
            "{} {} exceeds the {} variable count of {}",
            what,
            value,
            if hard_limit { "supported" } else { "specified" },
            limit
        ),
    )
}

#[inline]
pub fn var_count<L: Dimacs>(input: &mut LineReader) -> Parsed<usize, ParseError> {
    input.reader.set_mark();
    uint(input)
        .map_err(|count| exceeds_var_count(input, "variable count", count, L::MAX_DIMACS, true))
        .and_also(|&mut count| {
            if count > L::MAX_DIMACS as usize {
                Err(exceeds_var_count(
                    input,
                    "variable count",
                    count,
                    L::MAX_DIMACS,
                    true,
                ))
            } else {
                Ok(())
            }
        })
}

#[inline]
pub fn uint_count<T>(input: &mut LineReader, what: &str) -> Parsed<T, ParseError>
where
    T: Zero + FromPrimitive + OverflowingAdd + OverflowingMul + Bounded + Display,
{
    input.reader.set_mark();
    uint(input).map_err(|count| {
        input.give_up(format!(
            "{} {} exceeds the supported maximum of {}",
            what,
            count,
            T::max_value(),
        ))
    })
}

#[inline]
pub fn non_terminating_linebreaks(input: &mut LineReader) -> Result<bool, ParseError> {
    let linebreak = newline(input).matches()?;
    if linebreak {
        while comment(input).or_parse(|| newline(input)).matches()? {}
    }
    Ok(linebreak)
}

#[inline]
pub fn clause_lits<L: Dimacs>(
    input: &mut LineReader,
    lits: &mut Vec<L>,
    limit: isize,
    hard_limit: bool,
) -> Parsed<(), ParseError> {
    input.reader.set_mark();
    int(input)
        .map_err(|lit| exceeds_var_count(input, "literal", lit, limit, hard_limit))
        .and_then(|mut lit| {
            lits.clear();
            while lit != 0 {
                if (-limit..=limit).contains(&lit) {
                    lits.push(L::from_dimacs(lit));
                } else {
                    return Err(exceeds_var_count(input, "literal", lit, limit, hard_limit));
                }

                input.reader.set_mark();
                if let Some(next_lit) = int(input)
                    .map_err(|lit| exceeds_var_count(input, "literal", lit, limit, hard_limit))
                    .optional()?
                {
                    lit = next_lit;
                } else if non_terminating_linebreaks(input)? {
                    input.reader.set_mark();
                    lit = int(input)
                        .map_err(|lit| exceeds_var_count(input, "literal", lit, limit, hard_limit))
                        .or_give_up(|| unexpected(input, "literal, terminating zero or comment"))?;
                } else {
                    return Err(unexpected(input, "literal or terminating zero"));
                }
            }

            Ok(())
        })
}
