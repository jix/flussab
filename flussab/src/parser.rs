/// Return type for recursive descent parser that can fall through to another parser on unexpected
/// input.
///
/// If you have a parser that only fails irrecoverably, by returning a [`Result`], you can use
/// [`into`][Into::into] to convert it to a [`Parsed`] value. Conversely, [`Parsed`] values can be
/// converted into plain `Result` using [`or_give_up`][Parsed::or_give_up], making the `Parsed`
/// value fall through to an unrecoverable error, or using [`optional`][Parsed::optional] making the
/// parsed value optional.
///
/// Note that some of the methods of `Parsed` already have equivalent methods of the [`Result`] type
/// in `std`. Some more methods are also implemented for `Result` by this crate, using the
/// [`ResultExt`] trait. Finally some of the methods are only intended for parser that can fall
/// through, and as such, they are not implemented for `Result`.
#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Parsed<T, E> {
    /// A successfully parsed value or an irrecoverable error.
    ///
    /// When `Res(Ok(v))` is returned, the value was parsed successfully and the input stream should
    /// be advanced past the parsed input.
    ///
    /// When `Res(Err(err))` is returned, either the initial input matched, but continued in an
    /// unexpected way or some other kind of runtime error occured. In this case the input stream
    /// points at the position of the unexpected input or the input causing an error.
    Res(Result<T, E>),
    /// The input does not match the expected input of the parser.
    ///
    /// This leaves the input stream unchanged, so that another parser can be tried.
    Fallthrough,
}

pub use Parsed::*;

/// Return type for recursive descent parsers that fail irrecoverably on unexpected input.
///
/// This is just an alias for the [`std::result::Result`] type, added to hold this documentation.
/// See [`ResultExt`] for additional methods on `Result` values provided by this crate.
///
/// This is used for parsers that are guaranteed to never return the [`Fallthrough`] value. See
/// [`Parsed`] on how to convert between `Result` and `Parsed`.
pub type Result<T, E> = std::result::Result<T, E>;

impl<T, E> Parsed<T, E> {
    /// Equivalent to `map_err(From::from)`.
    pub fn err_into<E2: From<E>>(self) -> Parsed<T, E2> {
        self.map_err(From::from)
    }

    /// Makes a parser fail irrecoverably when the current input did not match the expected input of
    /// the returning parser.
    ///
    /// This makes the expected input of the parser returning this [`Parsed`] value a mandatory part
    /// of the input.
    #[inline]
    pub fn or_give_up(self, err: impl FnOnce() -> E) -> Result<T, E> {
        match self {
            Res(result) => result,
            Fallthrough => Err(err()),
        }
    }

    /// Returns `None` when the current input did not match the expected input of the returning
    /// parser.
    ///
    /// This makes the expected input of the parser returning this [`Parsed`] value optional.
    #[inline]
    pub fn optional(self) -> Result<Option<T>, E> {
        match self {
            Res(Ok(value)) => Ok(Some(value)),
            Res(Err(err)) => Err(err),
            Fallthrough => Ok(None),
        }
    }

    /// Returns whether the current input did match the expected input of the returning parser.
    ///
    /// This makes the expected input of the parser returning this [`Parsed`] value optional.
    #[inline]
    pub fn matches(self) -> Result<bool, E> {
        match self {
            Res(Ok(_)) => Ok(true),
            Res(Err(err)) => Err(err),
            Fallthrough => Ok(false),
        }
    }

    /// Tries a different parser when the current input did not match the expected input of the
    /// returning parser.
    ///
    /// This adds another parser to the expected choices for the current input.
    ///
    /// This is intentionally not called `or_else` as calling
    /// [`Result::or_else`][std::result::Result::or_else] on an irrecoverably failing parser cannot
    /// be used to implement choice, as such a parser can leave the input stream in a modified state
    /// when it returns an [`Err`] value.
    #[inline]
    pub fn or_parse(self, parse: impl FnOnce() -> Parsed<T, E>) -> Parsed<T, E> {
        match self {
            Fallthrough => parse(),
            v => v,
        }
    }

    /// Variant of `or_parse` for when the alternative parser is guaranteed to not fall through.
    #[inline]
    pub fn or_always_parse(self, parse: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
        match self {
            Fallthrough => parse(),
            Res(res) => res,
        }
    }

    /// Locks in the choice when the input matches the expected input of the returning parser.
    ///
    /// When the returning parser succeeds, but the passed parser fails, further alternatives will
    /// not be tried.
    #[inline]
    pub fn and_then<U>(self, parse: impl FnOnce(T) -> Result<U, E>) -> Parsed<U, E> {
        match self {
            Res(Ok(value)) => Res(parse(value)),
            Res(Err(err)) => Res(Err(err)),
            Fallthrough => Fallthrough,
        }
    }

    /// Variant of [`and_then`][Self::and_then] which always returns the (possibly modified)
    /// original value on success.
    ///
    /// This is useful to parse delimiters which do not contain any parsed information.
    #[inline]
    pub fn and_also(mut self, parse: impl FnOnce(&mut T) -> Result<(), E>) -> Parsed<T, E> {
        if let Res(Ok(value)) = &mut self {
            let res = parse(value);
            if let Err(err) = res {
                return Res(Err(err));
            }
        }
        self
    }

    /// Variant of [`and_also`][Self::and_then] where the action cannot fail.
    ///
    /// This is useful to modify a value or to perform side-effects on successful parses.
    #[inline]
    pub fn and_do(mut self, action: impl FnOnce(&mut T)) -> Parsed<T, E> {
        if let Res(Ok(value)) = &mut self {
            action(value);
        }
        self
    }

    /// Replaces a successfully parsed value with the value returned when applying the function `f`
    /// to it.
    ///
    /// If the parser was not successful, the error or fallthrough is returned unchanged.
    #[inline]
    pub fn map<T2>(self, f: impl FnOnce(T) -> T2) -> Parsed<T2, E> {
        match self {
            Res(Ok(value)) => Res(Ok(f(value))),
            Res(Err(err)) => Res(Err(err)),
            Fallthrough => Fallthrough,
        }
    }

    /// Replaces an occured error with the error returned when applying the function `f` to it.
    #[inline]
    pub fn map_err<E2>(self, f: impl FnOnce(E) -> E2) -> Parsed<T, E2> {
        match self {
            Res(Ok(value)) => Res(Ok(value)),
            Res(Err(err)) => Res(Err(f(err))),
            Fallthrough => Fallthrough,
        }
    }
}

impl<T, E> From<Result<T, E>> for Parsed<T, E> {
    #[inline]
    fn from(res: Result<T, E>) -> Self {
        Res(res)
    }
}

/// This trait implements additional methods for [`Result`] values, which are useful for parsing.
pub trait ResultExt<T, E> {
    /// Equivalent to `map_err(From::from)`.
    fn err_into<E2: From<E>>(self) -> Result<T, E2>;

    /// Variant of [`and_then`][std::result::Result::and_then] which always returns the (possibly
    /// modified) original value on success.
    ///
    /// This is useful to parse delimiters which do not contain any parsed information.
    fn and_also(self, f: impl FnOnce(&mut T) -> Result<(), E>) -> Self;

    /// Variant of [`and_also`][ResultExt::and_also] where the action cannot fail.
    ///
    /// This is useful to modify a value or to perform side-effects on successful parses.
    fn and_do(self, f: impl FnOnce(&mut T)) -> Self;
}

impl<T, E> ResultExt<T, E> for Result<T, E> {
    #[inline]
    fn err_into<E2: From<E>>(self) -> Result<T, E2> {
        self.map_err(From::from)
    }

    #[inline]
    fn and_also(mut self, f: impl FnOnce(&mut T) -> Result<(), E>) -> Result<T, E> {
        if let Ok(value) = &mut self {
            f(value)?;
        }
        self
    }

    #[inline]
    fn and_do(mut self, action: impl FnOnce(&mut T)) -> Result<T, E> {
        if let Ok(value) = &mut self {
            action(value);
        }
        self
    }
}

#[cfg(test)]
mod tests {
    use std::{
        iter::{Copied, Peekable},
        slice,
        str::FromStr,
    };

    use super::*;

    struct Stream<'a, 'b> {
        tokens: Peekable<Copied<slice::Iter<'b, &'a str>>>,
    }

    impl<'a, 'b> Stream<'a, 'b> {
        fn new(tokens: Peekable<Copied<slice::Iter<'b, &'a str>>>) -> Self {
            Self { tokens }
        }

        fn current(&mut self) -> Option<&'a str> {
            self.tokens.peek().copied()
        }
    }

    type Error<'a> = (Option<&'a str>, Vec<&'static str>);

    type Parsed<'a, T> = super::Parsed<T, Error<'a>>;
    type Result<'a, T> = super::Result<T, Error<'a>>;

    fn keyword<'a>(input: &mut Stream, keyword: &'static str) -> Parsed<'a, ()> {
        if input.current() == Some(keyword) {
            input.tokens.next();
            Res(Ok(()))
        } else {
            Fallthrough
        }
    }

    fn from_str<'a, T: FromStr>(input: &mut Stream) -> Parsed<'a, T> {
        if let Some(&str) = input.tokens.peek() {
            if let Ok(value) = T::from_str(str) {
                input.tokens.next();
                return Res(Ok(value));
            }
        }
        Fallthrough
    }

    fn isize_from_str_radix<'a>(input: &mut Stream, radix: u32) -> Parsed<'a, isize> {
        if let Some(&str) = input.tokens.peek() {
            if let Ok(value) = isize::from_str_radix(str, radix) {
                input.tokens.next();
                return Res(Ok(value));
            }
        }
        Fallthrough
    }

    fn end<'a>(input: &mut Stream) -> Parsed<'a, ()> {
        if input.tokens.peek().is_none() {
            Res(Ok(()))
        } else {
            Fallthrough
        }
    }

    fn entry<'a>(input: &mut Stream<'a, '_>) -> Parsed<'a, Option<isize>> {
        keyword(input, "value")
            .and_then(|_| {
                from_str(input)
                    .map(Some)
                    .or_parse(|| keyword(input, "none").map(|_| None))
                    .or_give_up(|| (input.current(), vec!["integer", "none"]))
            })
            .or_parse(|| {
                keyword(input, "radix").and_then(|_| {
                    let radix =
                        from_str::<u32>(input).or_give_up(|| (input.current(), vec!["radix"]))?;
                    isize_from_str_radix(input, radix)
                        .map(Some)
                        .or_give_up(|| (input.current(), vec!["radix-integer"]))
                })
            })
    }

    fn one_or_more<'a, T>(mut parse: impl FnMut() -> Parsed<'a, T>) -> Parsed<'a, Vec<T>> {
        parse().and_then(|initial| {
            let mut result = vec![initial];
            while let Some(value) = parse().optional()? {
                result.push(value);
            }
            Ok(result)
        })
    }

    fn parse_everything<'a>(input: &mut Stream<'a, '_>) -> Parsed<'a, Vec<Option<isize>>> {
        (keyword(input, "begin")).and_then(|_| {
            one_or_more(|| entry(input))
                .or_give_up(|| (input.current(), vec!["entry"]))
                .and_also(|_| {
                    keyword(input, "end").or_give_up(|| (input.current(), vec!["end", "entry"]))?;
                    end(input).or_give_up(|| (input.current(), vec!["end of file"]))?;
                    Ok(())
                })
        })
    }

    fn parse_words(input: &str) -> Result<Vec<Option<isize>>> {
        let tokens: Vec<_> = input.split_ascii_whitespace().collect();
        let mut input = Stream::new(tokens.iter().copied().peekable());
        let input = &mut input;
        parse_everything(input).or_give_up(|| (input.current(), vec!["begin"]))
    }

    #[test]
    fn parsing() {
        assert_eq!(parse_words(""), Err((None, vec!["begin"])));
        assert_eq!(parse_words("wrong"), Err((Some("wrong"), vec!["begin"])));
        assert_eq!(parse_words("begin"), Err((None, vec!["entry"])));
        assert_eq!(
            parse_words("begin wrong"),
            Err((Some("wrong"), vec!["entry"]))
        );
        assert_eq!(
            parse_words("begin value"),
            Err((None, vec!["integer", "none"]))
        );
        assert_eq!(parse_words("begin radix"), Err((None, vec!["radix"])));
        assert_eq!(
            parse_words("begin value none"),
            Err((None, vec!["end", "entry"]))
        );
        assert_eq!(parse_words("begin value none end"), Ok(vec![None]));
        assert_eq!(
            parse_words("begin value none value 3 end"),
            Ok(vec![None, Some(3)])
        );
        assert_eq!(
            parse_words("begin value -3 value none end"),
            Ok(vec![Some(-3), None])
        );
        assert_eq!(
            parse_words("begin radix 3 value none end"),
            Err((Some("value"), vec!["radix-integer"]))
        );
        assert_eq!(
            parse_words("begin value -3 radix 3 1212 value none end"),
            Ok(vec![Some(-3), Some(50), None])
        );
    }
}
