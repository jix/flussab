# Changelog

## flussab 0.3.0 (2022-03-06)

* Rename `ByteReader`/`ByteWriter` to `DeferredReader`/`DeferredWriter`
* Require `DeferredWriter` instead of any `Write` impl for faster writing
* Use `Config` structs instead of individual arguments for parser configs
* Add parser for the SAT competition output format
