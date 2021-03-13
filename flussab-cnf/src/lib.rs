//! This crate contains parsers and writers for the DIMACS CNF file format and for some variants or
//! extensions of this format.
//!
//! In the future it will also contain parser for other closely related file formats.

#![warn(missing_docs)]
pub mod cnf;
mod dimacs_trait;
mod error;
mod token;
pub mod wcnf;

pub use dimacs_trait::Dimacs;
pub use error::{InnerParseError, ParseError};
