use std::io;

pub use flussab::text::SyntaxError;
use thiserror::Error;

/// Either a [`SyntaxError`] or an [`io::Error`].
///
/// This is used via [`ParseError`], which wraps this in a [`Box`].
#[derive(Error, Debug)]
pub enum InnerParseError {
    /// A syntax error containing a message and a source location.
    #[error(transparent)]
    SyntaxError(SyntaxError),
    /// An IO error.
    #[error("IO error during parsing: {}", .0)]
    IoError(#[source] io::Error),
}

/// Boxed version of [`InnerParseError`].
pub type ParseError = Box<InnerParseError>;

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> Self {
        Box::new(InnerParseError::IoError(err))
    }
}

impl From<SyntaxError> for ParseError {
    fn from(err: SyntaxError) -> Self {
        Box::new(InnerParseError::SyntaxError(err))
    }
}

#[derive(Error, Debug)]
pub enum Error<L: crate::lit::Lit> {
    /// An error during parsing.
    #[error(transparent)]
    ParseError(#[from] ParseError),
    /// An error while processing the parsed data.
    #[error(transparent)]
    AigStructureError(#[from] crate::aig::AigStructureError<L>),
}

impl<L: crate::lit::Lit> From<io::Error> for Error<L> {
    fn from(err: io::Error) -> Self {
        Self::from(ParseError::from(err))
    }
}

impl<L: crate::lit::Lit> From<SyntaxError> for Error<L> {
    fn from(err: SyntaxError) -> Self {
        Self::from(ParseError::from(err))
    }
}
