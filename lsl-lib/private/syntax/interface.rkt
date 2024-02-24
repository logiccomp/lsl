#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     racket/syntax
                     syntax/id-table
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/parse/class/struct-id)
         (only-in automata/machine
                  machine?)
         racket/class
         syntax/location
         "grammar.rkt"
         "expand.rkt"
         "compile.rkt"
         "../contract/common.rkt"
         "../contract/parametric.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide define-protected
         declare-contract
         define-contract
         define-package
         contract-generate
         contract-shrink
         contract-symbolic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; phase-1 definitions

(begin-for-syntax
  (define-syntax-class define-header
    (pattern x:function-header
             #:with name #'x.name
             #:attr make-body
             (λ (body)
               #`(procedure-rename (λ x.params #,body) 'name)))
    (pattern x:id
             #:with name #'x
             #:attr make-body (λ (body) body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define-syntax define-protected
  (syntax-parser
    [(_ ?head:define-header ?body:expr)
     #:with ?new-body ((attribute ?head.make-body) #'?body)
     (define ctc (contract-table-ref #'?head.name))
     (if (not ctc)
        #'(define ?head.name ?new-body)
        #`(define ?head.name
            (let* ([name '?head.name]
                   [path (quote-module-name)]
                   [pos (positive-blame name path)]
                   [neg (negative-blame name path)]
                   [ctc #,(compile-contract
                           (expand-contract
                            (flip-intro-scope ctc)))]
                   [val ?new-body])
              ((send ctc protect val pos) (maybe-wrap name val) neg))))]))

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

(define-syntax declare-contract
  (syntax-parser
    [(_ name:id ctc:expr)
     (contract-table-set! #'name #'ctc)
     #'(void)]))

(define-syntax define-contract
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(define-syntax name
         (contract-macro
          (syntax-parser
            [_:id (syntax/loc #'ctc (Recursive name ctc))])))]
    [(_ (name:id param:id ...) ctc:expr)
     #'(define-syntax name
         (contract-macro
          (syntax-parser
            [(_:id param ...)
             (syntax/loc #'ctc (Recursive (name param ...) ctc))])))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; define-package

(begin-for-syntax
  (define (id->string x)
    (symbol->string (syntax-e x)))

  (define/hygienic (package-struct-defns stx name pkg-name)
    #:expression
    (syntax-parse stx
      #:literal-sets (contract-literal)
      [(Exists (x) (Struct s:struct-id e ...))
       ;; HACK: Name mangling necessary because Rosette structs don't
       ;; actually set the `field-info` part of the `struct-info`.
       #:with (?pkg-fld ...)
       (let ([n (add1 (string-length (id->string #'s)))])
         (for/list ([acc (attribute s.accessor-id)])
           (define fld (substring (id->string acc) n))
           (format-id name "~a-~a" name fld)))
       #`(begin
           (define ?pkg-fld (s.accessor-id #,pkg-name)) ...)]
      [_ #'(void)]))

  ;; HACK: Exfiltrate the info using `set!`
  (define/hygienic (compile-package-contract stx name info)
    #:expression
    (syntax-parse stx
      #:literal-sets (contract-literal)
      [(Exists (x) e)
       (define/syntax-parse e^ (compile-contract #'e))
       #`(new parametric-contract%
              [syntax #'#,stx]
              [polarity #f]
              [names '(x)]
              [seal-name '#,name]
              [make-body (λ (x) (set! #,info x) e^)])]
      [_
       #:fail-when
       (or (syntax-property stx 'unexpanded) stx)
       "not an existential contract"
       #'_])))

;; TODO: Nice to DRY up with `define-protected`
(define-syntax define-package
  (syntax-parser
    [(_ ?name:id ?body:expr)
     #:do [(define ctc (contract-table-ref #'?name))]
     #:fail-when (and (not ctc) #'?name) "unknown contract"
     #:do [(define expanded-ctc
             (expand-contract
              (flip-intro-scope ctc)))]
     #:with ?Name (kebab->camel #'?name)
     #:with ?info (generate-temporary)
     #:with ?pkg (generate-temporary)
     #`(begin
         (define ?info #f)
         (define ?pkg
           (let* ([name '?name]
                  [path (quote-module-name)]
                  [pos (positive-blame name path)]
                  [neg (negative-blame name path)]
                  [ctc #,(compile-package-contract expanded-ctc #'?Name #'?info)]
                  [val ?body])
             ((send ctc protect val pos) val neg)))
         (define-contract ?Name (seal-info-pred? ?info))
         #,(package-struct-defns expanded-ctc #'?name #'?pkg))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract operations

(define FUEL 50)

(define-syntax contract-generate
  (syntax-parser
    [(_ ctc:expr (~optional fuel:expr))
     #`(perform-or-error
        'contract-generate
        (λ ()
          (send #,(compile-contract (expand-contract #'ctc))
                generate
                (~? fuel FUEL))))]))

(define-syntax contract-shrink
  (syntax-parser
    [(_ ctc:expr val:expr (~optional fuel:expr))
     #`(perform-or-error
        'contract-shrink
        (λ ()
          (send #,(compile-contract (expand-contract #'ctc))
                shrink
                (~? fuel FUEL)
                val)))]))

(define-syntax contract-symbolic
  (syntax-parser
    [(_ ctc:expr)
     #`(perform-or-error
        'contract-symbolic
        (λ ()
          (send #,(compile-contract (expand-contract #'ctc))
                symbolic)))]))

(define (perform-or-error name thk)
  (define result (thk))
  (if (none? result)
      (raise-user-error name "failed")
      result))
