#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     syntax/kerncase
                     syntax/private/boundmap
                     syntax/parse
                     syntax/parse/class/struct-id
                     mischief/dict
                     mischief/sort)
         racket/class
         racket/promise
         "../contract/flat.rkt"
         "../contract/function.rkt"
         "../contract/oneof.rkt"
         "../contract/allof.rkt"
         "../contract/struct.rkt"
         "../contract/list.rkt"
         "../contract/recursive.rkt"
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax compile-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unbound variables (modified from mflatt's code)

(begin-for-syntax
  (define (merge t)
    (define m (make-module-identifier-mapping))
    (reverse
     (let loop ([t t] [a null])
       (cond
         [(null? t) a]
         [(identifier? t)
          (if (module-identifier-mapping-get m t (lambda () #f))
              a
              (begin
                (module-identifier-mapping-put! m t #t)
                (cons t a)))]
         [(pair? t) (loop (cdr t) (loop (car t) a))]
         [else (error "internal error")]))))

  (define (formals->ids f)
    (let loop ([f f])
      (cond
        [(identifier? f) (list f)]
        [(pair? f) (cons (car f)
                         (loop (cdr f)))]
        [(null? f) null]
        [(syntax? f) (loop (syntax-e f))])))

  (define (unbound-vars e)
    (define code-insp
      (variable-reference->module-declaration-inspector
       (#%variable-reference)))
    (define (submodule-error e)
      (error 'free-vars "submodules not supported: ~a" e))
    (define bindings (make-bound-identifier-mapping))
    (merge
     (let free-vars ([e e])
       (kernel-syntax-case
        (syntax-disarm e code-insp) #f
        [id
         (identifier? #'id)
         (let ([b (identifier-binding #'id)])
           (if (and (not b)
                    (not (bound-identifier-mapping-get bindings #'id (lambda () #f))))
               (list #'id)
               null))]
        [(#%top . id) (list #'id)]
        [(quote q) null]
        [(quote-syntax . _) null]
        [(#%plain-lambda formals expr ...)
         (let ([ids (formals->ids #'formals)])
           (for ([id (in-list ids)])
             (bound-identifier-mapping-put! bindings id #t))
           (begin0
               (map free-vars (syntax->list #'(expr ...)))
             (for ([id (in-list ids)])
               (bound-identifier-mapping-put! bindings id #f))))]
        [(case-lambda [formals expr ...] ...)
         (map free-vars (syntax->list
                         #'((#%plain-lambda formals expr ...) ...)))]
        [(let-values ([(id ...) rhs] ...) expr ...)
         (cons (free-vars #'(#%plain-lambda (id ... ...) expr ...))
               (map free-vars (syntax->list #'(rhs ...))))]
        [(letrec-values ([(id ...) rhs] ...) expr ...)
         (free-vars #'(#%plain-lambda (id ... ...) rhs ... expr ...))]
        [(letrec-syntaxes+values stx-bindings ([(id ...) rhs] ...) expr ...)
         (free-vars #'(#%plain-lambda (id ... ...) rhs ... expr ...))]
        [(kw expr ...)
         (ormap (lambda (k) (free-identifier=? k #'kw))
                (list #'if #'begin #'begin0 #'set! #'#%plain-app #'#%expression
                      #'#%variable-reference #'with-continuation-mark))
         (map free-vars (syntax->list #'(expr ...)))]
        [(module . _)
         (submodule-error e)]
        [(module* . _)
         (submodule-error e)]
        [(kw . _)
         (error 'free-vars "unknown core form: ~a" (syntax->datum #'kw))])))))

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
    (local-expand/capture-lifts #'stx 'expression null))

  (define/hygienic-metafunction (unbound-racket form-stx)
    #:expression
    (syntax-parse form-stx
      #:literals (begin #%plain-lambda)
      [(_ (begin _ ... (#%plain-lambda (x ...) body)))
       (datum->syntax #f (unbound-vars #'body))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile

(begin-for-syntax
  (define/hygienic-metafunction (compile-contract form-stx)
    #:expression
    (define/syntax-parse (_ stx) form-stx)
    (syntax-parse #'stx
      #:literal-sets (contract-literal flat-literal function-literal)
      [(Flat (check chk)
             (generate gen)
             (shrink shk)
             (symbolic sym))
       #'(new flat-contract%
              [syntax #'stx]
              [checker chk]
              [generator gen]
              [shrinker shk]
              [symbol sym])]
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
              [domains (list (let () a*) ...)]
              [codomain r*]
              [exceptions (list e.predicate-id ...)])]
      [(OneOf e ...)
       #'(new oneof-contract%
              [syntax #'stx]
              [disjuncts (list (compile-contract e) ...)])]
      [(AllOf e ...)
       #'(new allof-contract%
              [syntax #'stx]
              [conjuncts (list (compile-contract e) ...)])]
      [(Struct s:struct-id e ...)
       #'(new struct-contract%
              [syntax #'stx]
              [constructor s.constructor-id]
              [predicate s.predicate-id]
              [accessors (list s.accessor-id ...)]
              [mutators (list s.mutator-id ...)]
              [contracts (list (compile-contract e) ...)])]
      [(List e)
       #'(new list-contract%
              [syntax #'stx]
              [fixed? #f]
              [contracts (list (compile-contract e))])]
      [(Tuple e ...)
       #'(new list-contract%
              [syntax #'stx]
              [fixed? #t]
              [contracts (list (compile-contract e) ...)])]
      [(Recursive x e)
       #'(letrec ([x (new recursive-contract%
                          [syntax #'stx]
                          [promise (delay (compile-contract e))])])
           x)]
      [x:id #'x])))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))
