#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax ee-lib
                     racket/base
                     syntax/id-table
                     syntax/parse
                     syntax/parse/lib/function-header)
         racket/class
         syntax/location
         "grammar.rkt"
         "expand.rkt"
         "compile.rkt"
         "../contract/common.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide define-protected
         declare-contract
         define-contract
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

(define (maybe-wrap name val)
  (if (procedure? val)
      (λ args
        (define logs (current-logs))
        (when logs
          (current-logs (hash-update logs name add1 0)))
        (apply val args))
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
