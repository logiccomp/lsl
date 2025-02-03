#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     ee-lib
                     syntax/stx
                     syntax/id-set
                     syntax/parse
                     syntax/parse/class/struct-id
                     "free-vars.rkt")
         syntax/parse
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax expand-contract
                     contract-macro
                     contract-macro-proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(begin-for-syntax
  (define (contract-syntax-error s stx)
    (raise-syntax-error #f "contract cannot be used in here" stx))

  (struct parametric-var (id)
    #:property prop:procedure contract-syntax-error)
  (struct recursive-var (datum id)
    #:property prop:procedure contract-syntax-error)
  (struct contract-macro (proc)
    #:property prop:procedure contract-syntax-error)

  ;; TODO: Shouldn't need to do this...
  (define def-ctx (make-parameter #f))

  (define (expand-racket stx)
    (syntax-parse (local-expand/capture-lifts stx 'expression null (def-ctx))
      #:literals (kernel-literals)
      [(begin (define-values (id ...) e) ... body)
       #:with (^e ...) (stx-map expand-racket #'(e ...))
       #'(begin (define-values (id ...) ^e) ... body)]))

  (define/hygienic (expand-contract stx)
    #:expression
    (define new-stx
      (syntax-parse stx
        #:literal-sets (contract-literal immediate-literal function-literal)
        ;; TODO: all constitutent components should be contracted with `expr/c`
        [(Immediate ~! (~alt (~optional (check chk:expr))
                             (~optional (generate gen:expr))
                             (~optional (shrink shk:expr))
                             (feature feat-name:string feat:expr)) ...)
         (define/syntax-parse chk^ (expand-racket #'(~? chk (λ _ #t))))
         (define/syntax-parse gen^ (expand-racket #'(~? gen #f)))
         (define/syntax-parse shk^ (expand-racket #'(~? shk #f)))
         (define/syntax-parse feat^ (expand-racket #'(list (list (#%datum . feat-name) feat) ...)))
         #'(Immediate (check (let-values () chk^))
                      (generate (let-values () gen^))
                      (shrink (let-values () shk^))
                      (feature (let-values () feat^)))]
        [(Function ~! (~alt (~once (arguments [x:id a:expr] ...))
                            (~once (result r:expr))
                            (~optional (raises e:struct-id ...))) ...)
         #:fail-when
         (check-duplicate-identifier
          (filter non-wildcard? (syntax-e #'(x ...))))
         "duplicate identifier"
         (with-scope sc
           (define/syntax-parse (x^ ...)
             (for/list ([x (in-syntax #'(x ...))])
               (define x* (if (non-wildcard? x) x (generate-temporary x)))
               (bind! (add-scope x* sc) (racket-var))))
           (define/syntax-parse ([a^ fvs^] ...)
             (for/list ([a (in-syntax #'(a ...))])
               (define a^ (expand-contract (add-scope a sc)))
               (list a^ (free-id-set->list (fvs a^)))))
           (define/syntax-parse r^
             (expand-contract (add-scope #'r sc)))
           #'(Function (arguments [x^ fvs^ a^] ...)
                       (result r^)
                       (raises (~? (~@ e ...)))))]
        [(OneOf ~! e:expr ...)
         (define/syntax-parse (e^ ...)
           (stx-map expand-contract #'(e ...)))
         #'(OneOf e^ ...)]
        [(AllOf ~! e:expr ...)
         (define/syntax-parse (e^ ...)
           (stx-map expand-contract #'(e ...)))
         #'(AllOf e^ ...)]
        [(Struct ~! s:struct-id (e:expr ...))
         (define/syntax-parse (e^ ...)
           (stx-map expand-contract #'(e ...)))
         #'(Struct s (e^ ...))]
        [(List ~! e:expr)
         (define/syntax-parse e^ (expand-contract #'e))
         #'(List e^)]
        [(Tuple ~! e:expr ...)
         (define/syntax-parse (e^ ...)
           (stx-map expand-contract #'(e ...)))
         #'(Tuple e^ ...)]
        [(Recursive ~! (~or head:id (head:id param:expr ...)) e:expr)
         (with-scope sc
           (define/syntax-parse x^
             (bind! (add-scope #'head sc)
                    (recursive-var
                     (syntax->datum #'(~? (head param ...) head))
                     (add-scope #'head sc))))
           (define/syntax-parse e^
             (expand-contract (add-scope #'e sc)))
           (if (free-id-set-member? (fvs #'e^) #'x^)
               #'(Recursive x^ e^)
               #'e^))]
        ;; TODO: Fix this!
        [((~and (~or All Exists) name) ~! (x:id ...) e:expr)
         (with-scope sc
           (define new-def-ctx
             (syntax-local-make-definition-context (def-ctx)))
           (define/syntax-parse (x^ ...)
             (for/list ([x (in-syntax #'(x ...))])
               (define y (add-scope x sc))
               (define self
                 (car
                  (syntax-local-bind-syntaxes
                   (list y) #f new-def-ctx)))
               (bind! y (parametric-var self))
               (syntax-local-identifier-as-binding self)))
           (define/syntax-parse e^
             (parameterize ([def-ctx new-def-ctx])
               (expand-contract (add-scope #'e sc))))
           #'(name (x^ ...) e^))]
        ;; TODO: do this better
        [(~datum ...)
         (expand-contract
          #`(Immediate
             (check
              (λ _
                (raise-syntax-error
                 '…
                 "expected a finished contract, but found a template"
                 #'#,stx)))))]
        [(~or head:id (head:id e:expr ...))
         #:do [(define v (lookup #'head))]
         #:when (recursive-var? v)
         #:do [(define datum (recursive-var-datum v))]
         #:cut
         #:fail-when
         (and (not (equal? datum (syntax->datum stx))) stx)
         (format "must be exactly ~a" datum)
         (or (recursive-var-id v) #'head)]
        [x:id
         #:do [(define v (lookup #'x))]
         #:when (parametric-var? v)
         #:with y (parametric-var-id v)
         #'(Seal y)]
        [(~or head:id (head:id e:expr ...))
         #:do [(define v (lookup #'head))]
         #:when (contract-macro? v)
         #:do [(define proc (contract-macro-proc v))]
         (expand-contract (apply-as-transformer proc #'head 'expression stx))]
        [e:expr
         (expand-contract #'(Immediate (check (check-validity e #'e))))]))
    (syntax-replace-srcloc stx new-stx))

  (define (free-id-set-union* xs)
    (if (null? xs)
        (immutable-free-id-set null)
        (apply free-id-set-union xs)))

  (define (fvs stx)
    (syntax-parse stx
      #:literal-sets (contract-literal immediate-literal function-literal)
      [(Immediate (check chk:expr)
                  (generate gen:expr)
                  (shrink shk:expr)
                  (feature feat:expr))
       (free-id-set-union
        (immutable-free-id-set (free-vars #'chk))
        (immutable-free-id-set (free-vars #'gen))
        (immutable-free-id-set (free-vars #'shk))
        (immutable-free-id-set (free-vars #'feat)))]
      [(Function (arguments [x:id (_:id ...) a:expr] ...)
                 (result r:expr)
                 (raises e:struct-id ...))
       (free-id-set-subtract
        (free-id-set-union
         (free-id-set-union* (stx-map fvs #'(a ...)))
         (fvs #'r))
        (immutable-free-id-set (syntax-e #'(x ...))))]
      [(OneOf e:expr ...) (free-id-set-union* (stx-map fvs #'(e ...)))]
      [(AllOf e:expr ...) (free-id-set-union* (stx-map fvs #'(e ...)))]
      [(Struct s:struct-id (e:expr ...)) (free-id-set-union* (stx-map fvs #'(e ...)))]
      [(List e:expr) (fvs #'e)]
      [(Tuple e:expr ...) (free-id-set-union* (stx-map fvs #'(e ...)))]
      [(Recursive x:id e:expr)
       (free-id-set-remove (fvs #'e) #'x)]
      [((~and (~or All Exists) name) (x:id ...) e:expr)
       (free-id-set-subtract (fvs #'e) (immutable-free-id-set (syntax-e #'(x ...))))]
      [(Seal x) (immutable-free-id-set)]
      [x:id (immutable-free-id-set (list #'x))]
      [_ (immutable-free-id-set)]))

  (define (syntax-replace-srcloc loc-stx stx)
    (syntax-property
     (datum->syntax stx (syntax-e stx) loc-stx stx)
     'unexpanded loc-stx))

  (define (non-wildcard? x)
    (not (eq? (syntax-e x) '_))))

(define (check-validity e stx)
  (unless (procedure? e)
    (raise-syntax-error ': "invalid immediate contract (must be a predicate)" stx))
  e)
