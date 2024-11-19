# Changelog

## Recent changes

* `flussab-aiger` 0.1.1 fixes a bug when writing latch initialization
* New flussab-btor2 crate for BTOR2 files

## flussab 0.3.1 (2023-11-05)

* `flussab-cnf` is at version 0.3.1
* `flussab-aiger` is at version 0.1.0
* Add `advance_with_buf` to `DeferredReader`
* Changed some `#[inline]` and `#[cold]` attributes
* New flussab-aiger crate for AIGER files (binary and ASCII)

## flussab 0.3.0 (2022-03-06)

* Rename `ByteReader`/`ByteWriter` to `DeferredReader`/`DeferredWriter`
* Require `DeferredWriter` instead of any `Write` impl for faster writing
* Use `Config` structs instead of individual arguments for parser configs
* Add parser for the SAT competition output format
