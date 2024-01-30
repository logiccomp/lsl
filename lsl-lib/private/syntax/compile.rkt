#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     syntax/stx
                     syntax/kerncase
                     syntax/private/boundmap
                     syntax/parse
                     syntax/parse/class/struct-id
                     mischief/dict
                     mischief/sort)
         racket/class
         racket/promise
         "../contract/immediate.rkt"
         "../contract/function.rkt"
         "../contract/oneof.rkt"
         "../contract/allof.rkt"
         "../contract/struct.rkt"
         "../contract/list.rkt"
         "../contract/recursive.rkt"
         "../contract/parametric.rkt"
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax compile-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function dependency

(begin-for-syntax
  (struct exn:fail:cyclic exn:fail (srclocs)
    #:property prop:exn:srclocs
    (λ (self) (exn:fail:cyclic-srclocs self)))

  (define (sort-indices stx ids fvs)
    (define n (length (syntax-e ids)))
    (define index-hash
      (for/hash ([id (in-syntax ids)]
                 [k (in-naturals)])
        (values (syntax-e id) k)))
    (define dep-hash
      (for/hash ([id (in-syntax ids)]
                 [fv (in-syntax fvs)])
        (define deps
          (for/list ([var (in-syntax fv)])
            (hash-ref index-hash (syntax-e var) #f)))
        (values (hash-ref index-hash (syntax-e id))
                (filter values deps))))
    (define neighbors
      (dict->procedure #:failure (λ _ '()) dep-hash))
    (define (cycle _)
      (raise (exn:fail:cyclic "cannot have cyclic dependency"
                              (current-continuation-marks)
                              (list (syntax-srcloc stx)))))
    (topological-sort (range n) neighbors #:cycle cycle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(begin-for-syntax
  (define/hygienic (compile-contract stx)
    #:expression
    (define/syntax-parse quoted-stx #`#'#,stx)
    (syntax-parse stx
      #:literal-sets (contract-literal immediate-literal function-literal)
      [(Immediate (check chk)
                  (generate gen)
                  (shrink shk)
                  (symbolic sym))
       #'(new immediate-contract%
              [syntax quoted-stx]
              [checker chk]
              [generator gen]
              [shrinker shk]
              [symbol sym])]
      [(Function (arguments [x fvs a] ...)
                 (result r)
                 (raises e:struct-id ...))
       #:with (k ...) (sort-indices #'stx #'(x ...) #'(fvs ...))
       #:with r* #'(λ* (x ...) (compile-contract r))
       (define/syntax-parse (a^ ...) (stx-map compile-contract #'(a ...)))
       (define/syntax-parse r^ (compile-contract #'r))
       #'(new function-contract%
              [syntax quoted-stx]
              [domain-order (list (#%datum . k) ...)]
              [domains (list (λ* (x ...) a^) ...)]
              [codomain (λ* (x ...) r^)]
              [exceptions (list e.predicate-id ...)])]
      [(OneOf e ...)
       (define/syntax-parse (e^ ...) (stx-map compile-contract #'(e ...)))
       #'(new oneof-contract%
              [syntax quoted-stx]
              [disjuncts (list e^ ...)])]
      [(AllOf e ...)
       (define/syntax-parse (e^ ...) (stx-map compile-contract #'(e ...)))
       #'(new allof-contract%
              [syntax quoted-stx]
              [conjuncts (list e^ ...)])]
      [(Struct s:struct-id e ...)
       (define/syntax-parse (e^ ...) (stx-map compile-contract #'(e ...)))
       #'(new struct-contract%
              [syntax quoted-stx]
              [constructor s.constructor-id]
              [predicate s.predicate-id]
              [accessors (list s.accessor-id ...)]
              [mutators (list s.mutator-id ...)]
              [contracts (list e^ ...)])]
      [(List e)
       (define/syntax-parse e^ (compile-contract #'e))
       #'(new list-contract%
              [syntax quoted-stx]
              [fixed? #f]
              [contracts (list e^)])]
      [(Tuple e ...)
       (define/syntax-parse (e^ ...) (stx-map compile-contract #'(e ...)))
       #'(new list-contract%
              [syntax quoted-stx]
              [fixed? #t]
              [contracts (list e^ ...)])]
      [(Recursive x e)
       (define/syntax-parse e^ (compile-contract #'e))
       #'(letrec ([x (new recursive-contract%
                          [syntax quoted-stx]
                          [promise (delay e^)])])
           x)]
      [(All (x ...) e)
       (define/syntax-parse e^ (compile-contract #'e))
       #'(new parametric-contract%
              [syntax quoted-stx]
              [polarity #t]
              [names '(x ...)]
              [make-body (λ (x ...) e^)])]
      [(Exists (x ...) e)
       (define/syntax-parse e^ (compile-contract #'e))
       #'(new parametric-contract%
              [syntax quoted-stx]
              [polarity #f]
              [names '(x ...)]
              [make-body (λ (x ...) e^)])]
      [(Seal x)
       #'(new seal-contract%
              [syntax quoted-stx]
              [info x])]
      [x:id #'x])))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))
