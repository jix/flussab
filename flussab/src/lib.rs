//! The Flussab crate is a collection of utlities for writing parsers.
//!
//! Currently Flussab aims to provide just enough to write parsers with a certain combination of
//! constraints for which Flussab's author did not find a suitable existing solution. It is not
//! intended as a replacement for any such existing solution targeting a different set of
//! constraints.
//!
//! The target use-case are efficient, continuously streaming, interactive, error reporting,
//! non-backtracking, recursive-descent parsers for text-based, binary and mixed formats:
//!
//! * _Efficient_: The straight forward implementation of a parser should be fast, and the library
//!   should not get in the way when performing more complex optimizations that are possible in
//!   hand-rolled parsers (e.g. using data parallelism to scan multiple input bytes at once).
//!
//!   The efficiency is realized by a) using an input stream with a mutable cursor instead of
//!   threading the current position through return values and b) (when using [`DeferredReader`])
//!   deferring IO error checks. This together greatly simplifies the control and data flow of the
//!   parsers, which makes it easier for the compiler to optimize the code and often results in
//!   overall faster code.
//!
//!   Note that deferred error checking does not mean that IO errors are ignored or handled
//!   imprecisely. Instead when an IO error occurs the parsing logic will exhaust all current
//!   choices which forces a parse error. At the point where a parse error is generated, outside of
//!   the hot path, we can then check whether an IO error that occured caused this.
//!
//! * _Continuously streaming_: It should be possible to parse and process data which does not fit
//!   into memory, i.e. it is possible to read more data as it is parsed and processed, without
//!   buffering everything read so far. In particular it is not sufficient to require restarting the
//!   parser when the input was incomplete nor to require external code that splits a potentially
//!   infinite input stream into individual chunks.
//!
//! * _Interactive_: The parser should be usable in a REPL with multi-line input without negatively
//!   affecting efficiency when parsing data in bulk. This means it must be able to handle input
//!   data reads that return less bytes than requested, so that line-buffering is usable.
//!
//! * _Error reporting_: When parsing fails, a parser must be able to generate useful error
//!   messages. The goal here isn't to replicate the excellent error reporting of e.g. `rustc`, but
//!   to offer enough information that e.g. a user that generated a gigabyte sized input file using
//!   a bunch of `println!`s can quickly find the typo they made.
//!
//! * _Non-backtracking_ (a.k.a. _predictive_): Continuous streaming already requires some limits to
//!   back tracking. Avoiding backtracking completely also avoids many ways to accidentally making a
//!   recursive-descent parser become very slow for some inputs. This does mean that some form of
//!   look-ahead is required to parse most formats. This can be realized either by using the
//!   provided [`DeferredReader`] which has dynamic lookahead, and/or by using a tokenizer for
//!   formats where no look-ahead is required after tokenization.
//!
//!   (Note that Flussab doesn't stop you from handling backtracking yourself, it just does not
//!   provide any help for that.)
//!
//! * _Recursive-descent_: Parsers are written as simple Rust functions that take a mutable
//!   reference to the input stream and either return the parsed value after consuming some input or
//!   alternatively indicate failure. This also enables writing parser-combinators as higher level
//!   functions, although currently Flussab provides only a minimal set of combinators.
//!
//!   The provided infrastructure for combining parsers is entirely agnostic of how the input stream
//!   is handled, as long as it itself keeps track of the input position. This crate provides
//!   [`DeferredReader`] as one choice, but a [`Peekable`][std::iter::Peekable] iterator of tokens
//!   would work as well.
//!
//! ## Using Flussab
//!
//! Parsers are written very much like manual recursive descent parsers. If our format has a
//! _something_ we would have a function like this:
//! ```rust
//! # use flussab::*;
//! # type Something = ();
//! # type ParseError = ();
//! fn something(input: &mut DeferredReader) -> Parsed<Something, ParseError> {
//! #   let input_as_expected = false;
//!     // Check whether `input` contains a _something_ at the current position
//!     if !input_as_expected {
//!         return Fallthrough;
//!     }
//!     // Read _something_ from `input` and advance the input position.
//! #   let something = ();
//! #   let parsing_failed_after_advancing_the_input = false;
//! #   let some_error = ();
//!     if parsing_failed_after_advancing_the_input {
//!         return Res(Err(some_error));
//!     }
//!
//!     Res(Ok(something))
//! }
//! ```
//!
//! Here [`Parsed`] and [`DeferredReader`] are provided by this crate and are good entry points for
//! the documentation. A `Parsed` value is just a wrapper for a [`Result`] which adds
//! [`Fallthrough`] as a third option besides `Ok` and `Err` that indicates that the input does not
//! match and that the parser did not consume any input.
//!
//! Instead of manually returning `Fallthrough` or `Res(Err(..))`, often the provided methods of
//! [`Parsed`] and [`Result`] are used to combine smaller parsers into larger ones.

#![warn(missing_docs)]
mod deferred_reader;
mod deferred_writer;
mod parser;
pub mod text;
pub mod write;

pub use deferred_reader::DeferredReader;
pub use deferred_writer::DeferredWriter;
pub use parser::{Parsed, Result, ResultExt};

pub use Parsed::*;
