//! This crate contains parsers and writers for the DIMACS CNF file format.
//!
//! In the future it will also contain parser for extensions of this format and other closely
//! related file formats.
//!
//! See the [`cnf`] module for the DIMACS CNF file format.

#![warn(missing_docs)]
pub mod cnf;
mod dimacs_trait;
mod error;
mod token;

pub use dimacs_trait::Dimacs;
pub use error::{InnerParseError, ParseError};
