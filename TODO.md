* check binding specs (esp. for `contract` nonterminal)
* cyclic
* is `with-reference-compilers` working (?)
* annotate should use lift to generate contract definition for reuse
* record src loc for flat contract and use in error reporting
* improve perf with tight boundary using weak hash table cache
* add back rosette verification stuff
* add back random generation
* big bang style custom contracts
* using lax for now, indy later
* don't use `datum-literals` (use literal set?)
* have #false in the structs and only error on projection
* `contract-generate` should be a macro that takes syntax in
* better name for arrows
* split and DRY `function-dependency-graph`
* dry `function-contract`
* interact for function because dom is dependent
* free variable expansion should not be discarded
* do we need all the `#:binding-class` declarations?
* enforce ~alt constraint on flat
* should lift `with-reference-compilers` ?
* dynamic check that domain is flat contract (not higher order)
* error if symbolic not present
