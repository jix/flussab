# Flussab CNF

[![github][github-badge]][github]
[![crates.io][crate-badge]][crate]
[![docs.rs][docs-badge]][docs]

Parsing and writing of the [DIMACS CNF file format][dimacs-cnf], implemented
using [`flussab`][flussab]. The goal of this library is to provide a very
efficient streaming parser for the DIMACS CNF file format while being easy to
extend to the numerous variants, extensions and related file formats. Currently
plain CNF, WCNF and GCNF are supported.

## Performance

I don't have any extensive benchmarks, but parsing _and_ writing a 2.1 GiB CNF
file (from and to a `tmpfs`) takes 9 seconds on my machine, while `coreutils`'
`wc -w` takes 9.5 seconds to perform a much easier task.

Now it might seem silly to focus an _parsing_ performance for a file format
used in SAT solving, given that the runtime of SAT solvers almost always
dominates any time spent parsing. Nevertheless, often enough I find myself in
the situation of parsing many and/or large CNF files for some processing task
which is much faster than solving itself.

[dimacs-cnf]:http://www.satcompetition.org/2009/format-benchmarks2009.html
[flussab]:https://crates.io/crates/flussab

## License

This software is available under the Zero-Clause BSD license, see
[LICENSE](LICENSE) for full licensing information.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this software by you shall be licensed as defined in
[LICENSE](LICENSE).

[github]:https://github.com/jix/flussab
[crate]:https://crates.io/crates/flussab-cnf
[docs]:https://docs.rs/flussab-cnf/*/flussab_cnf

[github-badge]: https://img.shields.io/badge/github-jix/flussab-blueviolet?style=flat-square
[crate-badge]: https://img.shields.io/crates/v/flussab-cnf?style=flat-square
[docs-badge]: https://img.shields.io/badge/docs.rs-flussab_cnf-informational?style=flat-square
