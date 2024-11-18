//! Parsing and writing of the BTOR2 file format.
#![warn(missing_docs)]

pub mod btor2;

mod error;
mod parser;
mod token;

pub use parser::{Config, Parser};

pub use error::{InnerParseError, ParseError};

#[cfg(test)]
mod tests;
