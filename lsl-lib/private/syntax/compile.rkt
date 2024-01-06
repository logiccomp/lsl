#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     syntax/parse
                     syntax/parse/class/struct-id
                     mischief/dict
                     mischief/sort
                     "unbound-vars.rkt")
         racket/class
         "../contract/flat.rkt"
         "../contract/function.rkt"
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
    (topological-sort (range n) neighbors #:cycle cycle))

  (define/hygienic-metafunction (expand-racket form-stx)
    #:expression
    (define/syntax-parse (_ stx) form-stx)
    (local-expand #'stx 'expression null))

  (define/hygienic-metafunction (unbound-racket form-stx)
    #:expression
    (syntax-parse form-stx
      #:literals (#%plain-lambda)
      [(_ (#%plain-lambda (x ...) body))
       (datum->syntax #f (unbound-vars #'body))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(begin-for-syntax
  (define/hygienic-metafunction (compile-contract form-stx)
    #:expression
    (define/syntax-parse (_ stx) form-stx)
    (syntax-parse #'stx
      #:literal-sets (contract-literal flat-literal)
      [(Flat (domain dom)
             (check chk)
             (generate gen)
             (shrink shk))
       #`(new flat-contract%
              [syntax #'stx]
              [domain dom]
              [checker chk]
              [generator gen]
              [shrinker shk])]
      [(Function (arguments [x a] ...)
                 (result r)
                 (raises e:struct-id ...))
       #:with (a* ...) #'((expand-racket (λ* (x ...) (compile-contract a))) ...)
       #:with (y ...) #'((unbound-racket a*) ...)
       #:with (k ...) (sort-indices #'stx #'(x ...) #'(y ...))
       #:with r* #'(λ* (x ...) (compile-contract r))
       #'(new function-contract%
              [syntax #'stx]
              [domain-order (list (#%datum . k) ...)]
              [domains (list a* ...)]
              [codomain r*]
              [exceptions (list e.predicate-id ...)])])))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))
