## LSL

[![Build Status][build-badge]][build]

## Todo

* restore error locs
* define-contract should check to make sure is actually contract...
* annotate should use lift to generate contract definition for reuse
* improve perf with tight boundary using weak hash table cache
* have #false in the structs and only error on projection
* better name for arrows (can use the names from stx of doms and cods instead of runtime)
* split and DRY `function-dependency-graph`
* dry `function-contract`
* free variable expansion should not be discarded
* do we need all the `#:binding-class` declarations?
* enforce ~alt constraint on flat
* dynamic check that domain is flat contract (not higher order)
* error if symbolic not present
* should remove multiple values from function since ISL doesn't have them?
* contract-exercise & contract-verify should have output
* `define-annotated` should be in syntax-spec and should attach contract to persistent symbol table
* restrict disjuncts of `one-of` to flat
* or contract generation and symbolic, etc
* name for or error reporting
* restrict names of struct to lowercase kebab case
* define-contract needs "function" form for abstraction in dependency case
* countexample generation for unions doesn't work properly
* TODO struct should do as many flat contracts as possibly can early
* recursive check inside "productive" constructors
* performance is atrocious...
* recursive must be over flat stuff
* shorthand for recursive (no explicit syntax)
* parameterized define-contract
* no else for cond
* add contract-verify and contract-exercise to rackunit results with extension hooks
* mutual recursion
* errors (invalid inputs) for check-*
* recursive should have name too (or not?)
* lifted is probably quite slow, can do more at compile time?
* calling static struct info leads to bad error message (rosette doesn't support `#:name` though)

```
(: x (Trace (Flat (check increasing?))))
(declare-trace x)

(: time (-> (And Integer (Recording x))))
```

[build-badge]: https://github.com/logiccomp/lsl/actions/workflows/build.yml/badge.svg
[build]: https://github.com/logiccomp/lsl/actions/workflows/build.yml?query=workflow%3Abuild
