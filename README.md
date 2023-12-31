## LSL

[![Build Status][build-badge]][build]

## Todo

* check error source location for -> flat failure
* define-contract should check to make sure is actually contract...
* annotate should use lift to generate contract definition for reuse
* improve perf with tight boundary using weak hash table cache
* have #false in the structs and only error on projection
* better name for arrows (can use the names from stx of doms and cods instead of runtime)
* dynamic check that domain is flat contract (not higher order)
* error if symbolic not present
* name for or error reporting
* restrict names of struct to lowercase kebab case
* counterexample generation for unions doesn't work properly
* struct should do as many flat contracts as possible early
* recursive check inside "productive" constructors
* recursive must be over flat stuff
* mutual recursion
* errors (invalid inputs) for check-*
* lifted is probably quite slow, can do more at compile time?
* calling static struct info leads to bad error message (rosette doesn't support `#:name` though)
* generalize List to non-flat elements

[build-badge]: https://github.com/logiccomp/lsl/actions/workflows/build.yml/badge.svg
[build]: https://github.com/logiccomp/lsl/actions/workflows/build.yml?query=workflow%3Abuild
