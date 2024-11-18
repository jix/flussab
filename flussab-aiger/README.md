# Flussab AIGER

[![github][github-badge]][github]
[![crates.io][crate-badge]][crate]
[![docs.rs][docs-badge]][docs]

Parsing and writing of the [AIGER file format][aiger] for combinational and
sequential boolean circuits represented as And-Inverter-Graphs (AIGs),
implemented using [`flussab`][flussab]. The goal of this library is to provide
a very efficient streaming parser for the AIGER file format. In addition to a
streaming parser, which allows parsing the AIG directly into application
specific data structures, this library also provides simple data structures to
represent the full contents of an AIGER file together with utility functions
for reading writing that are implemented on top of the streaming API. Finally
since the binary AIGER file format places more restrictions on the numbering of
literals, this library provides functions fo renumbering AIGs to allow
conversion from ASCII AIGER to binary AIGER.

**Note:** This is currently a preview release. It's mostly feature complete,
but it's lacking documentation and I might still restructure the API a bit
while prepareing this for a first proper release.

[aiger]:https://fmv.jku.at/aiger/
[flussab]:https://crates.io/crates/flussab

## License

This software is available under the Zero-Clause BSD license, see
[LICENSE](LICENSE) for full licensing information.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this software by you shall be licensed as defined in
[LICENSE](LICENSE).

[github]:https://github.com/jix/flussab
[crate]:https://crates.io/crates/flussab-aiger
[docs]:https://docs.rs/flussab-aiger/*/flussab_aiger

[github-badge]: https://img.shields.io/badge/github-jix/flussab-blueviolet?style=flat-square
[crate-badge]: https://img.shields.io/crates/v/flussab-aiger?style=flat-square
[docs-badge]: https://img.shields.io/badge/docs.rs-flussab_aiger-informational?style=flat-square
