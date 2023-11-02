## `syntax.rkt`

* check binding specs (esp. for `contract` nonterminal)
* cyclic
* is `with-reference-compilers` working (?)
* annotate should use lift to generate contract definition for reuse
* record src loc for flat contract and use in error reporting

## `runtime.rkt`

* improve perf with tight boundary using weak hash table cache
* add back rosette verification stuff
* add back random generation
* big bang style custom contracts
* using lax for now, indy later
* don't use `datum-literals`
* have #false in the structs and only error on projection
* `contract-generate` should be a macro that takes syntax in
* better name for arrows
