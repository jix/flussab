//! Parsing of SAT solver logs.
//!
//! Most SAT solvers produce an output conforming to the [conventions used in the yearly SAT
//! competition][output-format], which includes whether the input formula is satisfiable ("solution
//! line") and, in case it is satisfiable, optionally a variable assignment ("value lines") under
//! which the input formula is satisfied. This module can parse such logs.
//!
//! [output-format]:http://www.satcompetition.org/2009/format-solvers2009.html

use flussab::text::LineReader;

use crate::{error::ParseError, token, Dimacs};

/// Data contained in a SAT solver log.
#[derive(PartialEq, Eq, Debug)]
pub struct SolverLog<L> {
    /// Whether the input formula is satisfiable (`Some(true)`), unsatisfiable (`Some(false)`) or
    /// unkonwn `None`.
    ///
    /// This does not distinguish between the case of no solution line in the output and the case of
    /// a `"s UNKNOWN"` solution line.
    pub satisfiable: Option<bool>,
    /// Satisfying assignment. All literals from all value lines in order.
    pub assignment: Vec<L>,
}

/// Configuration for the SAT solver log parser.
#[derive(Clone, Default, Debug)]
#[non_exhaustive]
pub struct Config {
    /// When set, any line that does not start with `"s "` or `"v "` is ignored, even if it does not
    /// start with `"c "`. In particular this includes lines containing just a single `"c"` or empty
    /// lines, which some solvers output even though the specification does not allow those.
    /// (Default: `false`)
    pub ignore_unknown_lines: bool,
}

impl Config {
    #[inline]
    /// Sets the [`ignore_unknown_lines`][Self#structfield.ignore_unknown_lines] field.
    pub fn ignore_unknown_lines(mut self, value: bool) -> Self {
        self.ignore_unknown_lines = value;
        self
    }
}

/// Parses a SAT solver log.
pub fn parse_log<L>(input: &mut LineReader, config: Config) -> Result<SolverLog<L>, ParseError>
where
    L: Dimacs,
{
    let mut satisfiable = None;
    let mut assignment = vec![];
    let mut assignment_started = false;
    let mut assignment_finished = false;

    loop {
        while token::interactive_strict_comment(input).matches()? {}

        if !assignment_finished && token::fixed(input, b"v ").matches()? {
            token::skip_whitespace(input);

            assignment_started = true;
            while let Some(lit) = {
                input.reader.set_mark();
                token::int::<isize>(input)
                    .map_err(|lit| {
                        token::exceeds_var_count(input, "literal", lit, L::MAX_DIMACS, true)
                    })
                    .optional()?
            } {
                if lit == 0 {
                    assignment_finished = true;
                    break;
                }

                if (-L::MAX_DIMACS..=L::MAX_DIMACS).contains(&lit) {
                    assignment.push(L::from_dimacs(lit));
                } else {
                    return Err(token::exceeds_var_count(
                        input,
                        "literal",
                        lit,
                        L::MAX_DIMACS,
                        true,
                    ));
                }
            }

            token::interactive_end_of_line(input)
                .or_give_up(|| token::unexpected(input, "end of line"))?;
        } else if satisfiable.is_none() && token::fixed(input, b"s ").matches()? {
            satisfiable = Some(
                token::fixed(input, b"SATISFIABLE")
                    .map(|_| Some(true))
                    .or_parse(|| token::fixed(input, b"UNSATISFIABLE").map(|_| Some(false)))
                    .or_parse(|| token::fixed(input, b"UNKNOWN").map(|_| None))
                    .and_also(|_| {
                        token::interactive_end_of_line(input)
                            .or_give_up(|| token::unexpected(input, "end of line"))
                    })
                    .or_give_up(|| {
                        token::unexpected(input, "SATISFIABLE, UNSATISFIABLE or UNKNOWN")
                    })?,
            );
        } else if token::eof(input).matches()? {
            if assignment_started && !assignment_finished {
                return Err(token::unexpected(input, "comment or value line"));
            }
            break;
        } else if config.ignore_unknown_lines && token::interactive_skip_line(input).matches()? {
        } else {
            let mut expected = vec!["comment line (\"c \")"];

            if !assignment_finished {
                expected.push("value line (\"v \")");
            }

            if satisfiable.is_none() {
                expected.push("solution line (\"s \")");
            }

            if assignment_finished || !assignment_started {
                expected.push("end of file");
            }

            let last = expected.pop().unwrap();
            let mut expected = expected.join(", ");
            expected.push_str(" or ");
            expected.push_str(last);

            return Err(token::unexpected(input, &expected));
        }
    }

    Ok(SolverLog {
        satisfiable: satisfiable.flatten(),
        assignment,
    })
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use flussab::DeferredReader;

    use super::*;

    type Result<T> = std::result::Result<T, ParseError>;

    macro_rules! parse {
        ($input:expr) => {
            parse!($input, Config::default())
        };
        ($input:expr, $config:expr) => {
            parse_log::<i32>(
                &mut DeferredReader::from_read(($input).as_bytes()).into(),
                $config,
            )
        };
    }

    #[test]
    fn empty() -> Result<()> {
        let parsed = parse!("")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: None,
                assignment: vec![]
            }
        );
        Ok(())
    }

    #[test]
    fn unsat() -> Result<()> {
        let parsed = parse!("c foo\ns UNSATISFIABLE\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(false),
                assignment: vec![]
            }
        );
        Ok(())
    }

    #[test]
    fn sat() -> Result<()> {
        let parsed = parse!("c foo\ns SATISFIABLE\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(true),
                assignment: vec![]
            }
        );
        Ok(())
    }

    #[test]
    fn unknown() -> Result<()> {
        let parsed = parse!("c foo\ns UNKNOWN\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: None,
                assignment: vec![]
            }
        );
        Ok(())
    }

    #[test]
    fn sat_assignment_after() -> Result<()> {
        let parsed = parse!("c foo\ns SATISFIABLE\nv 1 -2 3 0\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(true),
                assignment: vec![1, -2, 3]
            }
        );
        Ok(())
    }

    #[test]
    fn sat_assignment_before() -> Result<()> {
        let parsed = parse!("c foo\nv 1 -2 3 0\ns SATISFIABLE\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(true),
                assignment: vec![1, -2, 3]
            }
        );
        Ok(())
    }

    #[test]
    fn sat_split_assignment() -> Result<()> {
        let parsed = parse!("c foo\nv 1 -2\nv \ns SATISFIABLE\nv 3 0\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(true),
                assignment: vec![1, -2, 3]
            }
        );
        Ok(())
    }

    #[test]
    fn err_empty_line() -> Result<()> {
        let parsed = parse!("c foo\ns SATISFIABLE\nv 1 -2 3 0\nc bar\n\n");
        assert_matches!(parsed, Err(..));
        Ok(())
    }

    #[test]
    fn err_comment_spaces() -> Result<()> {
        let parsed = parse!("c\ns SATISFIABLE\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!(" c foo\ns SATISFIABLE\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\ns SATISFIABLE\nv 1 -2 3 0\n c bar\n");
        assert_matches!(parsed, Err(..));
        Ok(())
    }

    #[test]
    fn err_values_spaces() -> Result<()> {
        let parsed = parse!("c foo\ns SATISFIABLE\nv1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\ns SATISFIABLE\n v 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\ns SATISFIABLE\nv\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        Ok(())
    }

    #[test]
    fn values_extra_space() -> Result<()> {
        let parsed = parse!("c foo\ns SATISFIABLE\nv \t 1  -2   3   0\t\nc bar\n")?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(true),
                assignment: vec![1, -2, 3]
            }
        );
        Ok(())
    }

    #[test]
    fn err_solution_spaces() -> Result<()> {
        let parsed = parse!("c foo\nsSATISFIABLE\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\ns  SATISFIABLE\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\ns SATISFIABLE \nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\n s SATISFIABLE\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        Ok(())
    }

    #[test]
    fn err_multiple_solution() -> Result<()> {
        let parsed = parse!("c foo\ns UNKNOWN\ns SATISFIABLE\nv 1 -2 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        Ok(())
    }

    #[test]
    fn err_values_terminator() -> Result<()> {
        let parsed = parse!("c foo\ns SATISFIABLE\nv 1 -2 3\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\nv 1 -2 0\ns SATISFIABLE\nv 3 0\nc bar\n");
        assert_matches!(parsed, Err(..));
        let parsed = parse!("c foo\nv 1 -2 0\ns SATISFIABLE\nv \nc bar\n");
        assert_matches!(parsed, Err(..));
        Ok(())
    }

    #[test]
    fn err_exceeding_max_var_count() {
        let input = format!("s SATISFIABLE\nv {} 0\n", (i32::MAX as u32) + 1);
        let parsed = parse!(input);
        assert_matches!(parsed, Err(..));
    }

    #[test]
    fn sat_ignore_unknown() -> Result<()> {
        let parsed = parse!(
            "c foo\n\nc\nv 1 -2\nv\ns SATISFIABLE\nv 3 0\nc bar\nhello, world!",
            Config::default().ignore_unknown_lines(true)
        )?;
        assert_eq!(
            parsed,
            SolverLog {
                satisfiable: Some(true),
                assignment: vec![1, -2, 3]
            }
        );
        Ok(())
    }
}
