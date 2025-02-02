#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     syntax/stx
                     syntax/parse
                     syntax/parse/class/struct-id
                     mischief/dict
                     mischief/sort)
         (only-in automata/machine
                  machine?)
         syntax/location
         racket/class
         racket/promise
         "../contract/common.rkt"
         "../contract/immediate.rkt"
         "../contract/function.rkt"
         "../contract/oneof.rkt"
         "../contract/allof.rkt"
         "../contract/struct.rkt"
         "../contract/list.rkt"
         "../contract/recursive.rkt"
         "../contract/parametric.rkt"
         "../util.rkt"
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax compile-contract
                     attach-contract))

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
                  (feature feat))
       #'(if (procedure? chk)
             (new immediate-contract%
              [syntax quoted-stx]
              [checker chk]
              [generator gen]
              [shrinker shk]
              [feature feat])
             (raise
              (exn:fail:invalid-signature-contract
               (format "Invalid contract: ~a" chk)
               (current-continuation-marks)
               (list (syntax-property quoted-stx 'unexpanded)))))]
      [(Function (arguments [x fvs a] ...)
                 (result r)
                 (raises e:struct-id ...))
       #:with (k ...) (sort-indices #'stx #'(x ...) #'(fvs ...))
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
      [(Struct s:struct-id (e ...))
       (define/syntax-parse (e^ ...) (stx-map compile-contract #'(e ...)))
       #'(new struct-contract%
              [syntax quoted-stx]
              [constructor s.constructor-id]
              [predicate s.predicate-id]
              [accessors (list s.accessor-id ...)]
              [mutators (~? (list s.mutator-id ...) #f)]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; `attach-contract`

(begin-for-syntax
  (define (attach-contract name ctc val #:compiler [compiler compile-contract])
    #`(let* ([name '#,name]
             [path (quote-module-name)]
             [pos (positive-blame name path)]
             [neg (negative-blame name path)]
             [ctc #,(compiler ctc)]
             [val #,val])
        (attach-contract-fn name pos neg ctc val))))

(define (attach-contract-fn name pos neg ctc val)
  (if (current-disable-contract)
      val
      ((send ctc protect val pos)
       (maybe-wrap name val)
       neg)))

;; TODO: Could be made robust.
(define (maybe-wrap name val)
  (if (and (procedure? val) (not (machine? val)))
      (procedure-reduce-arity
       (λ args
         (define logs (current-logs))
         (when logs
           (current-logs (hash-update logs name add1 0)))
         (apply val args))
       (procedure-arity val))
      val))
