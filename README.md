## LSL todo

* define-contract should check to make sure is actually contract...
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
* restrict disjuncts of `one-of` to flat
* or contract generation and symbolic, etc
* name for or error reporting
* restrict names of struct to lowercase kebab case
* define-contract needs "function" form for abstraction in dependency case
* countexample generation for unions doesn't work properly
* TODO struct should do as many flat contracts as possibly can early
* recursive check inside "productive" constructors
* performance is atrocious...
* why no #%top-interaction?
* recursive must be over flat stuff
* shorthand for recursive (no explicit syntax)
* parameterized define-contract
* bind #%top for unbound identifiers
* no else for cond
