//! This crate contains parsers and writers for the DIMACS CNF file format and for some variants or
//! extensions of this format.
//!
//! In the future it will also contain parser for other closely related file formats.

#![warn(missing_docs)]
mod dimacs_trait;
mod error;
mod token;

pub mod cnf;
pub mod gcnf;
pub mod wcnf;

pub mod sat_solver_log;

pub use dimacs_trait::Dimacs;
pub use error::{InnerParseError, ParseError};
