* check binding specs (esp. for `contract` nonterminal)
* cyclic
* annotate should use lift to generate contract definition for reuse
* record src loc for flat contract and use in error reporting
* improve perf with tight boundary using weak hash table cache
* don't use `datum-literals` (use literal set?)
* have #false in the structs and only error on projection
* better name for arrows (can use the names from stx of doms and cods instead of runtime)
* split and DRY `function-dependency-graph`
* dry `function-contract`
* interact for function because dom is dependent
* free variable expansion should not be discarded
* do we need all the `#:binding-class` declarations?
* enforce ~alt constraint on flat
* should lift `with-reference-compilers` ?
* dynamic check that domain is flat contract (not higher order)
* error if symbolic not present
* should remove multiple values from function since ISL doesn't have them?
* contract-exercise & contract-verify should have output
* `define-annotated` should be in syntax-spec and should attach contract to persistent symbol table
