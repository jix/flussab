//! Parsing and writing of the DIMACS CNF file format.
use std::io::{BufReader, Read, Write};

use flussab::{text::LineReader, write, DeferredReader, DeferredWriter};

use crate::{error::ParseError, token, Dimacs};

/// Header data of a DIMACS CNF file.
///
/// Contains the number of variables and clauses. This parser considers a count of `0` as
/// unspecified and will not check that count during parsing, even in strict mode.
#[derive(Copy, Clone, Debug)]
pub struct Header {
    /// Upper bound on the number of variables present in the formula.
    ///
    /// Ignored during parsing when `0`.
    pub var_count: usize,
    /// Number of clauses present in the formula.
    ///
    /// Ignored during parsing when `0`.
    pub clause_count: usize,
}

/// Configuration for the DIMACS CNF parser.
#[derive(Clone, Default, Debug)]
#[non_exhaustive]
pub struct Config {
    /// When set, the variable and clause count of the DIMACS CNF header are ignored
    /// during parsing. (Default: `false`)
    pub ignore_header: bool,
}

impl Config {
    #[inline]
    /// Sets the [`ignore_header`][Self#structfield.ignore_header] field.
    pub fn ignore_header(mut self, value: bool) -> Self {
        self.ignore_header = value;
        self
    }
}

/// Parser for the DIMACS CNF file format.
pub struct Parser<'a, L> {
    reader: LineReader<'a>,
    clause_count: usize,
    clause_limit: usize,
    clause_limit_active: bool,
    lit_limit: isize,
    lit_limit_is_hard: bool,
    lit_buf: Vec<L>,
    header: Option<Header>,
}

impl<'a, L> Parser<'a, L>
where
    L: Dimacs,
{
    /// Creates a parser reading from a [`BufReader`].
    pub fn from_buf_reader(
        buf_reader: BufReader<impl Read + 'a>,
        config: Config,
    ) -> Result<Self, ParseError> {
        Self::new(
            LineReader::new(DeferredReader::from_buf_reader(buf_reader)),
            config,
        )
    }

    /// Creates a parser reading from a [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    pub fn from_read(read: impl Read + 'a, config: Config) -> Result<Self, ParseError> {
        Self::new(LineReader::new(DeferredReader::from_read(read)), config)
    }

    /// Creates a parser reading from a boxed [`Read`] instance.
    ///
    /// If the [`Read`] instance is a [`BufReader`], it is better to use
    /// [`from_buf_reader`][Self::from_buf_reader] to avoid unnecessary double buffering of the
    /// data.
    #[inline(never)]
    pub fn from_boxed_dyn_read(
        read: Box<dyn Read + 'a>,
        config: Config,
    ) -> Result<Self, ParseError> {
        Self::new(
            LineReader::new(DeferredReader::from_boxed_dyn_read(read)),
            config,
        )
    }

    /// Creates a parser reading from a [`LineReader`].
    pub fn new(reader: LineReader<'a>, config: Config) -> Result<Self, ParseError> {
        let mut new = Self {
            reader,
            clause_count: 0,
            clause_limit: 0,
            clause_limit_active: false,
            lit_limit: L::MAX_DIMACS,
            lit_limit_is_hard: true,
            lit_buf: vec![],
            header: None,
        };

        if let Some(header) = new.parse_header()? {
            if !config.ignore_header {
                if header.var_count != 0 {
                    new.lit_limit = header.var_count as isize;
                    new.lit_limit_is_hard = false;
                }
                if header.clause_count != 0 {
                    new.clause_limit = header.clause_count;
                    new.clause_limit_active = true;
                }
            }
            new.header = Some(header);
        }

        Ok(new)
    }

    fn parse_header(&mut self) -> Result<Option<Header>, ParseError> {
        let reader = &mut self.reader;

        token::skip_whitespace(reader);
        while token::comment(reader).matches()? || token::newline(reader).matches()? {}

        token::word(reader, b"p")
            .and_then(|_| {
                token::word(reader, b"cnf").or_give_up(|| token::unexpected(reader, "\"cnf\""))?;

                let var_count = token::var_count::<L>(reader)
                    .or_give_up(|| token::unexpected(reader, "variable count"))?;

                let clause_count = token::uint_count(reader, "clause count")
                    .or_give_up(|| token::unexpected(reader, "clause count"))?;

                token::interactive_end_of_line(reader)
                    .or_give_up(|| token::unexpected(reader, "end of line"))?;

                Ok(Header {
                    var_count,
                    clause_count,
                })
            })
            .optional()
    }

    /// Returns the DIMACS CNF header if it was present.
    pub fn header(&self) -> Option<Header> {
        self.header
    }

    /// Parses and returns the next clause.
    ///
    /// Returns `Ok(None)` if the end of file was successfully reached.
    pub fn next_clause(&mut self) -> Result<Option<&[L]>, ParseError> {
        let input = &mut self.reader;
        self.lit_buf.clear();

        token::skip_whitespace(input);
        loop {
            if self.clause_count != self.clause_limit || !self.clause_limit_active {
                let clause = token::clause_lits(
                    input,
                    &mut self.lit_buf,
                    self.lit_limit,
                    self.lit_limit_is_hard,
                )
                .and_also(|_| {
                    token::interactive_end_of_line(input)
                        .or_give_up(|| token::unexpected(input, "end of line"))
                });
                if clause.matches()? {
                    self.clause_count += 1;
                    break Ok(Some(&self.lit_buf));
                }
            }

            if token::comment(input).matches()? {
                continue;
            }

            if token::newline(input).matches()? {
                continue;
            }

            if (!self.clause_limit_active || self.clause_count >= self.clause_limit)
                && token::eof(input).matches()?
            {
                break Ok(None);
            }

            break Err(self.unexpected_statement());
        }
    }

    #[cold]
    #[inline(never)]
    fn unexpected_statement(&mut self) -> ParseError {
        let mut expected = vec![];

        if self.clause_count == 0 && self.header.is_none() {
            expected.push("header");
        }
        if !self.clause_limit_active || self.clause_count < self.clause_limit {
            expected.push("clause");
        }
        expected.push("comment");

        if !self.clause_limit_active || self.clause_count >= self.clause_limit {
            expected.push("end of file");
        }

        let last = expected.pop().unwrap();
        let mut expected = expected.join(", ");
        expected.push_str(" or ");
        expected.push_str(last);

        token::unexpected(&mut self.reader, &expected)
    }
}

/// Writes a DIMACS CNF header.
pub fn write_header(writer: &mut DeferredWriter, header: Header) {
    let _ = writeln!(writer, "p cnf {} {}", header.var_count, header.clause_count);
}

/// Writes a clause.
pub fn write_clause<L: Dimacs>(writer: &mut DeferredWriter, clause_lits: &[L]) {
    for lit in clause_lits {
        write::text::ascii_digits(writer, lit.dimacs());
        writer.write_all_defer_err(b" ");
    }
    writer.write_all_defer_err(b"0\n")
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use super::*;

    type Result<T> = std::result::Result<T, ParseError>;

    #[test]
    fn empty() -> Result<()> {
        let mut parser = Parser::<i32>::from_read("".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn headerless() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("1 2 -3 0\n4 5 0\n-6 0\n0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, Some(&[4, 5][..]));
        assert_eq!(parser.next_clause()?, Some(&[-6][..]));
        assert_eq!(parser.next_clause()?, Some(&[][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn eof_clause() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("1 2 -3 0\n4 5 0\n-6 0".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, Some(&[4, 5][..]));
        assert_eq!(parser.next_clause()?, Some(&[-6][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn empty_lines() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "\n1 2 -3 0\n\n4 5 0\n\n-6 0\n\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, Some(&[4, 5][..]));
        assert_eq!(parser.next_clause()?, Some(&[-6][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn split_clauses() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("1 2\n-3 0\n4 5\n0\n-6\n0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, Some(&[4, 5][..]));
        assert_eq!(parser.next_clause()?, Some(&[-6][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn split_clauses_with_comments() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "1 2\nc 0\n-3 0\nc 0\n4 5\nc 0\n0\nc 0\n-6\nc 0\n0\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, Some(&[4, 5][..]));
        assert_eq!(parser.next_clause()?, Some(&[-6][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn incomplete_header() {
        let parser = Parser::<i32>::from_read("p cnf\n1 2 -3 0\n".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));

        let parser = Parser::<i32>::from_read("p cnf 0\n1 2 -3 0\n".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn empty_header() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 0 0\n1 2 -3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn empty_lines_before_header() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("\n\np cnf 0 0\n1 2 -3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn var_only_header() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 0\n1 2 -3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn full_header() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 1\n1 2 -3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn early_comment() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("c 9 0\np cnf 3 1\n1 2 -3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn mid_comment() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 1\nc 9 0\n1 2 -3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn late_comment() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 1\n1 2 -3 0\nc 9 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn crlf_newlines() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "p cnf 3 1\r\n1 2 -3 0\r\nc 0\r\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn extra_misc_whitespace() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            " p\tcnf  3 1\t\n\t1\t 2\t-3\t0\r\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn leading_zeros_and_negative_zero() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "p cnf 00000000000000000000004 002\n00001 02 -03 00\n 004 -00\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, 2, -3][..]));
        assert_eq!(parser.next_clause()?, Some(&[4][..]));
        assert_eq!(parser.next_clause()?, None);
        Ok(())
    }

    #[test]
    fn max_var_count() -> Result<()> {
        let input = format!("p cnf {} 0\n{0} -{0} 0\n", i32::MAX);
        let mut parser = Parser::<i32>::from_read(input.as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[i32::MAX, -i32::MAX][..]));
        Ok(())
    }

    #[test]
    fn err_exceeding_max_var_count() {
        let input = format!("p cnf {} 0\n", (i32::MAX as u32) + 1);
        let parser = Parser::<i32>::from_read(input.as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_negative_var_count() {
        let parser = Parser::<i32>::from_read("p cnf -1 0".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_exceeding_max_clause_count() {
        let input = format!("p cnf 1 {}\n", (u64::MAX as u128) + 1);
        let parser = Parser::<i32>::from_read(input.as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_wrong_header() {
        let parser = Parser::<i32>::from_read("p notcnf\n".as_bytes(), Config::default());
        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_wrong_header_line2() {
        let parser = Parser::<i32>::from_read("\np\n".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_missing_clauses() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 2\n1 -2 3 0\n".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_matches!(parser.next_clause(), Err(..));
        Ok(())
    }

    #[test]
    fn err_extra_clauses() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "p cnf 3 2\n1 -2 3 0\n2 0\n3 0\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_eq!(parser.next_clause()?, Some(&[2][..]));
        assert_matches!(parser.next_clause(), Err(..));
        Ok(())
    }

    #[test]
    fn err_pos_lit_out_of_range() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 2\n1 -2 3 0\n2 4 0".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_matches!(parser.next_clause(), Err(..));
        Ok(())
    }

    #[test]
    fn pos_lit_out_of_range_ignored_header() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "p cnf 3 2\n1 -2 3 0\n2 4 0".as_bytes(),
            Config::default().ignore_header(true),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_eq!(parser.next_clause()?, Some(&[2, 4][..]));
        Ok(())
    }

    #[test]
    fn err_neg_lit_out_of_range() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 2\n1 -2 3 0\n2 -4 0".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_matches!(parser.next_clause(), Err(..));
        Ok(())
    }

    #[test]
    fn err_unterminated_clause() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 3 2\n1 -2 3 0\n2 -3".as_bytes(), Config::default())?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_matches!(parser.next_clause(), Err(..));
        Ok(())
    }

    #[test]
    fn err_dangling_literal() -> Result<()> {
        let mut parser = Parser::<i32>::from_read(
            "p cnf 3 2\n1 -2 3 0\n2 -3 0 1 0\n".as_bytes(),
            Config::default(),
        )?;

        assert_eq!(parser.next_clause()?, Some(&[1, -2, 3][..]));
        assert_matches!(parser.next_clause(), Err(..));
        Ok(())
    }

    #[test]
    fn err_unexpected_token_var_count() {
        let parser = Parser::<i32>::from_read("p cnf error 2\n".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_unexpected_token_clause_count() {
        let parser = Parser::<i32>::from_read("p cnf 2 error\n".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_extra_header_field() {
        let parser = Parser::<i32>::from_read("p cnf 2 1 2\n".as_bytes(), Config::default());

        assert_matches!(parser.err(), Some(..));
    }

    #[test]
    fn err_unexpected_token_clause() -> Result<()> {
        let mut parser =
            Parser::<i32>::from_read("p cnf 2 1\n 1 2 err 0\n".as_bytes(), Config::default())?;

        assert_matches!(parser.next_clause(), Err(..));

        Ok(())
    }

    #[test]
    fn roundtrip() -> Result<()> {
        let input = &r"
p cnf 5 12
-1 -2 0
-1 -3 0
-1 -4 0
-1 -5 0
-2 -3 0
-2 -4 0
-2 -5 0
-3 -4 0
-3 -5 0
-4 -5 0
2 5 3 4 1 0
4 2 3 1 5 0
"[1..];

        let mut output = vec![];
        let mut parser = Parser::<i32>::from_read(input.as_bytes(), Config::default())?;

        {
            let mut writer = DeferredWriter::from_write(&mut output);

            write_header(&mut writer, parser.header().unwrap());

            while let Some(clause) = parser.next_clause()? {
                write_clause(&mut writer, clause);
            }

            writer.flush()?;
        }

        assert_eq!(input.as_bytes(), output);

        Ok(())
    }
}
