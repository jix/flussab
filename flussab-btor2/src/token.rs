use core::str;
use std::num::NonZeroU64;

use bstr::BStr;
use flussab::{
    text::{self, LineReader},
    DeferredReader,
    Parsed::{self, Fallthrough, Res},
};

use crate::{
    btor2::{AssignmentKind, BinaryOp, NodeId, SingleValueOutputKind, TernaryOp, UnaryOp},
    error::ParseError,
};

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

#[inline]
pub fn skip_whitespace(input: &mut LineReader) {
    let mut offset = 0;
    loop {
        let next = input.reader.request_byte_at_offset(offset);
        match next {
            Some(b' ') => (),
            Some(b'\n') => input.line_at_offset(offset + 1),
            _ => break,
        }
        offset += 1;
    }
    input.reader.advance(offset);
}

/// Parses a non-negative integer.
#[inline]
pub fn uint(input: &mut LineReader) -> Parsed<u64, String> {
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
#[inline(never)]
pub fn exceeds_count(input: &mut LineReader, what: &str, value: &str) -> ParseError {
    input.give_up_at(
        input.reader.mark(),
        format!(
            "{} {} exceeds the supported node id limit of {}",
            what,
            value,
            u64::MAX,
        ),
    )
}

#[inline]
pub fn positive_int(input: &mut LineReader, what: &str) -> Parsed<NonZeroU64, ParseError> {
    if matches!(input.reader.request_byte(), Some(b'0')) {
        return Fallthrough;
    }
    uint(input)
        .map_err(|value| exceeds_count(input, what, &value))
        .map(|width| NonZeroU64::new(width).unwrap())
}

#[inline]
pub fn nonnegative_int(input: &mut LineReader, what: &str) -> Parsed<u64, ParseError> {
    uint(input).map_err(|value| exceeds_count(input, what, &value))
}

#[inline]
pub fn node_id(input: &mut LineReader) -> Parsed<NodeId, ParseError> {
    positive_int(input, "node id").map(NodeId)
}

#[inline]
pub fn sort_id(input: &mut LineReader) -> Parsed<NodeId, ParseError> {
    positive_int(input, "sort id").map(NodeId)
}

#[inline]
pub fn required_positive_int(input: &mut LineReader, what: &str) -> Result<NonZeroU64, ParseError> {
    positive_int(input, what).or_give_up(|| unexpected(input, what))
}

#[inline]
pub fn required_nonnegative_int(input: &mut LineReader, what: &str) -> Result<u64, ParseError> {
    nonnegative_int(input, what).or_give_up(|| unexpected(input, what))
}

#[inline]
pub fn required_node_id(input: &mut LineReader) -> Result<NodeId, ParseError> {
    node_id(input).or_give_up(|| unexpected(input, "node id"))
}

#[inline]
pub fn required_sort_id(input: &mut LineReader) -> Result<NodeId, ParseError> {
    sort_id(input).or_give_up(|| unexpected(input, "sort id"))
}

/// Parses a single space.
#[inline]
pub fn comment_start(input: &mut LineReader) -> Parsed<(), ParseError> {
    if matches!(input.reader.request_byte(), Some(b';')) {
        input.reader.advance(1);
        Res(Ok(()))
    } else {
        Fallthrough
    }
}

pub fn comment_body<'a>(input: &'a mut LineReader) -> &'a BStr {
    let mut offset = 0;

    while !matches!(
        input.reader.request_byte_at_offset(offset),
        Some(b'\n') | None
    ) {
        offset += 1;
    }

    input.reader.advance_with_buf(offset).into()
}

#[inline]
pub fn symbol_name<'a>(input: &'a mut LineReader) -> Parsed<&'a BStr, ParseError> {
    let mut offset = 0;

    while !matches!(
        input.reader.request_byte_at_offset(offset),
        Some(b'\n' | b' ') | None
    ) {
        offset += 1;
    }

    if offset == 0 {
        Fallthrough
    } else {
        Res(Ok(input.reader.advance_with_buf(offset).into()))
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

fn ascii_lowercase_u64(reader: &mut DeferredReader, offset: usize) -> (u64, usize) {
    if reader.buf_len() < offset + 8 {
        return ascii_lowercase_u64_cold(reader, offset);
    }
    let word = unsafe { u64::from_le_bytes(*(reader.buf_ptr().add(offset) as *const [u8; 8])) };

    const REPEAET: u64 = 0x0101010101010101;

    let high_mismatch = word ^ (REPEAET * 0x60);

    let low_bits = word & (REPEAET * 0x1f);

    let too_small = (low_bits ^ (REPEAET * 0x1f)) + REPEAET;

    let too_large = low_bits + (REPEAET * 0x05);

    let matches = (high_mismatch | too_small | too_large) & (REPEAET * 0xe0);

    // This is 8 times the amount of matching characters
    let shift = matches.trailing_zeros() & !7;

    if shift == 64 {
        return (word, 8);
    }

    let mask = !((!0u64) << shift);

    (word & mask, (shift / 8) as usize)
}

fn ascii_lowercase_u64_cold(reader: &mut DeferredReader, offset: usize) -> (u64, usize) {
    let mut reading = true;
    let mut len = 0;
    let word = u64::from_le_bytes(std::array::from_fn(|i| {
        if reading {
            match reader.request_byte_at_offset(offset + i) {
                Some(c @ b'a'..=b'z') => {
                    len = i + 1;
                    c
                }
                _ => {
                    reading = false;
                    0
                }
            }
        } else {
            0
        }
    }));
    (word, len)
}

fn ascii_lowercase<'a>(reader: &'a mut DeferredReader, mut offset: usize) -> &'a str {
    let start = offset;
    loop {
        let advance = ascii_lowercase_u64(reader, offset).1;
        offset += advance;
        if advance < 8 {
            break;
        }
    }

    unsafe { str::from_utf8_unchecked(&reader.buf()[start..offset]) }
}

fn hex_string<'a>(reader: &'a mut DeferredReader, mut offset: usize) -> &'a str {
    let start = offset;
    while matches!(
        reader.request_byte_at_offset(offset),
        Some(b'0'..=b'9' | b'a'..=b'f' | b'A'..=b'F')
    ) {
        offset += 1;
    }

    unsafe { str::from_utf8_unchecked(&reader.buf()[start..offset]) }
}

fn decimal_string<'a>(reader: &'a mut DeferredReader, mut offset: usize) -> &'a str {
    let start = offset;
    if matches!(reader.request_byte_at_offset(offset), Some(b'-')) {
        offset += 1;
    }
    while matches!(reader.request_byte_at_offset(offset), Some(b'0'..=b'9')) {
        offset += 1;
    }

    unsafe { str::from_utf8_unchecked(&reader.buf()[start..offset]) }
}

fn binary_string<'a>(reader: &'a mut DeferredReader, mut offset: usize) -> &'a str {
    let start = offset;
    while matches!(reader.request_byte_at_offset(offset), Some(b'0' | b'1')) {
        offset += 1;
    }

    unsafe { str::from_utf8_unchecked(&reader.buf()[start..offset]) }
}

pub fn required_hex_constant<'a>(input: &'a mut LineReader) -> Result<&'a str, ParseError> {
    let matched = hex_string(input.reader(), 0).len();
    if matched == 0 {
        Err(unexpected(input, "hex constant"))
    } else {
        let matched = unsafe { str::from_utf8_unchecked(input.reader.advance_with_buf(matched)) };

        Ok(matched)
    }
}

pub fn required_decimal_constant<'a>(input: &'a mut LineReader) -> Result<&'a str, ParseError> {
    let matched = decimal_string(input.reader(), 0).len();
    if matched == 0 {
        Err(unexpected(input, "decimal constant"))
    } else {
        let matched = unsafe { str::from_utf8_unchecked(input.reader.advance_with_buf(matched)) };

        Ok(matched)
    }
}

pub fn required_binary_constant<'a>(input: &'a mut LineReader) -> Result<&'a str, ParseError> {
    let matched = binary_string(input.reader(), 0).len();
    if matched == 0 {
        Err(unexpected(input, "binary constant"))
    } else {
        let matched = unsafe { str::from_utf8_unchecked(input.reader.advance_with_buf(matched)) };

        Ok(matched)
    }
}

pub enum NodeToken {
    Sort,
    Assignment(AssignmentKind),
    Output(SingleValueOutputKind),
    Justice,
    Value(NodeValueToken),
}

pub enum NodeValueToken {
    Const,
    Constd,
    Consth,
    Ones,
    One,
    Zero,
    Input,
    State,
    ExtOp(NodeValueExtOpToken),
    Slice,
    UnaryOp(NodeValueUnaryOpToken),
    BinaryOp(BinaryOp),
    TernaryOp(TernaryOp),
}

pub enum NodeValueExtOpToken {
    Uext,
    Sext,
}

impl NodeValueExtOpToken {
    pub fn unary_op(self, pad: u64) -> UnaryOp {
        match self {
            NodeValueExtOpToken::Uext => UnaryOp::Uext(pad),
            NodeValueExtOpToken::Sext => UnaryOp::Sext(pad),
        }
    }
}
pub enum NodeValueUnaryOpToken {
    Not,
    Inc,
    Dec,
    Neg,
    Redand,
    Redor,
    Redxor,
}

impl NodeValueUnaryOpToken {
    pub fn unary_op(self) -> UnaryOp {
        match self {
            NodeValueUnaryOpToken::Not => UnaryOp::Not,
            NodeValueUnaryOpToken::Inc => UnaryOp::Inc,
            NodeValueUnaryOpToken::Dec => UnaryOp::Dec,
            NodeValueUnaryOpToken::Neg => UnaryOp::Neg,
            NodeValueUnaryOpToken::Redand => UnaryOp::Redand,
            NodeValueUnaryOpToken::Redor => UnaryOp::Redor,
            NodeValueUnaryOpToken::Redxor => UnaryOp::Redxor,
        }
    }
}

pub fn node_token(input: &mut LineReader) -> Parsed<NodeToken, ParseError> {
    let matched = ascii_lowercase(input.reader(), 0);
    let token = match matched {
        "sort" => NodeToken::Sort,
        "init" => NodeToken::Assignment(AssignmentKind::Init),
        "next" => NodeToken::Assignment(AssignmentKind::Next),
        "bad" => NodeToken::Output(SingleValueOutputKind::Bad),
        "constraint" => NodeToken::Output(SingleValueOutputKind::Constraint),
        "fair" => NodeToken::Output(SingleValueOutputKind::Fair),
        "output" => NodeToken::Output(SingleValueOutputKind::Output),
        "justice" => NodeToken::Justice,
        "const" => NodeToken::Value(NodeValueToken::Const),
        "constd" => NodeToken::Value(NodeValueToken::Constd),
        "consth" => NodeToken::Value(NodeValueToken::Consth),
        "ones" => NodeToken::Value(NodeValueToken::Ones),
        "one" => NodeToken::Value(NodeValueToken::One),
        "zero" => NodeToken::Value(NodeValueToken::Zero),
        "input" => NodeToken::Value(NodeValueToken::Input),
        "state" => NodeToken::Value(NodeValueToken::State),
        "uext" => NodeToken::Value(NodeValueToken::ExtOp(NodeValueExtOpToken::Uext)),
        "sext" => NodeToken::Value(NodeValueToken::ExtOp(NodeValueExtOpToken::Sext)),
        "slice" => NodeToken::Value(NodeValueToken::Slice),
        "not" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Not)),
        "inc" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Inc)),
        "dec" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Dec)),
        "neg" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Neg)),
        "redand" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Redand)),
        "redor" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Redor)),
        "redxor" => NodeToken::Value(NodeValueToken::UnaryOp(NodeValueUnaryOpToken::Redxor)),
        "iff" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Iff)),
        "implies" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Implies)),
        "eq" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Eq)),
        "neq" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Neq)),
        "ugt" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Ugt)),
        "sgt" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sgt)),
        "ugte" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Ugte)),
        "sgte" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sgte)),
        "ult" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Ult)),
        "slt" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Slt)),
        "ulte" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Ulte)),
        "slte" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Slte)),
        "and" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::And)),
        "nand" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Nand)),
        "nor" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Nor)),
        "or" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Or)),
        "xnor" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Xnor)),
        "xor" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Xor)),
        "rol" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Rol)),
        "ror" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Ror)),
        "sll" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sll)),
        "sra" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sra)),
        "srl" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Srl)),
        "add" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Add)),
        "mul" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Mul)),
        "udiv" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Udiv)),
        "sdiv" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sdiv)),
        "smod" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Smod)),
        "urem" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Urem)),
        "srem" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Srem)),
        "sub" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sub)),
        "uaddo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Uaddo)),
        "saddo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Saddo)),
        "sdivo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Sdivo)),
        "umulo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Umulo)),
        "smulo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Smulo)),
        "usubo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Usubo)),
        "ssubo" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Ssubo)),
        "concat" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Concat)),
        "read" => NodeToken::Value(NodeValueToken::BinaryOp(BinaryOp::Read)),
        "ite" => NodeToken::Value(NodeValueToken::TernaryOp(TernaryOp::Ite)),
        "write" => NodeToken::Value(NodeValueToken::TernaryOp(TernaryOp::Write)),
        _ => return Fallthrough,
    };
    let advance = matched.len();
    input.reader.advance(advance);

    Res(Ok(token))
}

pub enum SortToken {
    Bitvec,
    Array,
}

pub fn sort_token(input: &mut LineReader) -> Parsed<SortToken, ParseError> {
    let matched = ascii_lowercase(input.reader(), 0);
    let token = match matched {
        "bitvec" => SortToken::Bitvec,
        "array" => SortToken::Array,
        _ => return Fallthrough,
    };
    let advance = matched.len();
    input.reader.advance(advance);

    Res(Ok(token))
}
