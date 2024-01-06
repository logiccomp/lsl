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
         "../contract/common.rkt"
         "grammar.rkt"
         "expand.rkt"
         "compile.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide $define
         $:
         $define-contract)

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

(define-syntax $define
  (syntax-parser
    [(_ ?head:define-header ?body:expr)
     #:with ?new-body ((attribute ?head.make-body) #'?body)
     (define ctc (free-id-table-ref contract-table #'?head.name #f))
     (if (not ctc)
        #'(define ?head.name ?new-body)
        #`(define ?head.name
            (let* ([name '?head.name]
                   [path (quote-module-name)]
                   [pos (positive-blame name path)]
                   [neg (negative-blame name path)]
                   [ctc (compile-contract
                         (expand-contract
                          #,(flip-intro-scope ctc)))]
                   [val ?new-body])
              ((send ctc protect val pos) val neg))))]))

(define-syntax $:
  (syntax-parser
    [(_ name:id ctc:expr)
     (free-id-table-set! contract-table #'name #'ctc)
     #'(void)]))

;; TODO: restore recursive
(define-syntax $define-contract
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(define-contract-syntax name
         (syntax-parser
           [_:id
            (syntax/loc #'ctc
              ctc #;(Recursive name ctc))]))]
    [(_ (name:id param:id ...) ctc:expr)
     #'(define-contract-syntax name
         (syntax-parser
           [(_:id param ...)
            (syntax/loc #'ctc
              ctc #;(Recursive (name param ...) ctc))]))]))
