# Flussab

[![github][github-badge]][github]
[![crates.io][crate-badge]][crate]
[![docs.rs][docs-badge]][docs]

The Flussab crate is a collection of utlities for writing parsers.

Currently Flussab aims to provide just enough to write parsers with a certain
combination of constraints for which Flussab's author did not find a suitable
existing solution. It is not intended as a replacement for any such existing
solution targeting a different set of constraints.

The target use-case are efficient, continuously streaming, interactive, error
reporting, non-backtracking, recursive-descent parsers for text-based, binary
and mixed formats. See the [documentation][docs], for details on what
trade-offs this implies.

Parsers (and writers) for the following file formats are currently implemented
using Flussab:

* [`flussab-cnf`](https://crates.io/crates/flussab-cnf) for the DIMACS CNF file
  format and variants.

## License

This software is available under the Zero-Clause BSD license, see
[LICENSE](LICENSE) for full licensing information.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this software by you shall be licensed as defined in
[LICENSE](LICENSE).

[github]:https://github.com/jix/flussab
[crate]:https://crates.io/crates/flussab
[docs]:https://docs.rs/flussab/*/flussab

[github-badge]: https://img.shields.io/badge/github-jix/flussab-blueviolet?style=flat-square
[crate-badge]: https://img.shields.io/crates/v/flussab?style=flat-square
[docs-badge]: https://img.shields.io/badge/docs.rs-flussab-informational?style=flat-square
