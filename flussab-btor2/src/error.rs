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
