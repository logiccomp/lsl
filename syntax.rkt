#lang racket/base

;;
;; provide
;;

(provide (for-space contract-space (all-defined-out))
         (for-syntax contract-macro)
         define-annotated
         annotate
         define-alias
         contract-generate
         ->)

;;
;; require
;;

(require (for-syntax racket/base
                     racket/function
                     racket/sequence
                     syntax/id-table
                     syntax/parse
                     syntax/parse/lib/function-header)
         syntax-spec
         syntax/location
         "runtime/contract.rkt"
         "runtime/flat.rkt"
         "runtime/function.rkt")

;;
;; `define-annotated`
;;

(begin-for-syntax
  (define contract-table (make-bound-id-table))

  (define-syntax-class define-header
    (pattern x:function-header
             #:with name #'x.name
             #:attr make-body (λ (body) #`(λ x.params #,body)))
    (pattern x:id
             #:with name #'x
             #:attr make-body (λ (body) body))))

(define-syntax define-annotated
  (syntax-parser
    [(_ ?head:define-header ?body:expr)
     #:do [(define name-stx (syntax-local-introduce #'?head.name))]
     #:attr ?ctc (bound-id-table-ref contract-table name-stx (const #f))
     #:with ?new-body ((attribute ?head.make-body) #'?body)
     (if (attribute ?ctc)
         #'(define ?head.name
             (let ([pos (positive-blame-struct (quote-module-name))]
                   [neg (negative-blame-struct (quote-module-name))])
               (((contract-struct-protect (compile ?ctc)) ?new-body pos) neg)))
         #'(define ?head.name ?new-body))]))

;;
;; contract grammar
;;

(syntax-spec
 (binding-class contract-var-class)
 (extension-class contract-macro #:binding-space contract-space)

 (host-interface/definitions
  (annotate name:id ctc:contract)
  #:do [(define name-stx (syntax-local-introduce #'name))
        (bound-id-table-set! contract-table name-stx #'ctc)]
  #'(void))

 (host-interface/expression
  (contract-generate ctc:contract)
  #'(contract-generate-function (compile ctc)))

 (nonterminal contract
   #:allow-extension contract-macro
   #:binding-space contract-space
   (function arg:argument-clause res:result-clause)
   #:binding {(recursive arg) {(recursive res)}}
   e:racket-expr)

 (nonterminal/two-pass argument-clause
   #:binding-space contract-space
   (arguments [x:contract-var ctc:contract] ...)
   #:binding [(re-export x) ctc])

 (nonterminal/two-pass result-clause
   #:binding-space contract-space
   (results [x:contract-var ctc:contract] ...)
   #:binding [(re-export x) ctc])

 (nonterminal/two-pass contract-var
   #:binding-space contract-space
   _
   x:contract-var-class
   #:binding (export x)))

;;
;; `define-alias`
;;

(define-syntax define-alias
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(define-syntax name
         (contract-macro
          (syntax-parser
            [_ #'ctc])))]))

;;
;; `compile`
;;

(define-syntax compile
  (syntax-parser
    #:datum-literals (function arguments results #%host-expression)
    [(_ (function (arguments [x a] ...) (results [y r] ...)))
     #'(function-contract
        (list (λ* (x ...) (compile a)) ...)
        (list (λ* (x ... y ...) (compile r)) ...))]
    [(_ (~and e (#%host-expression _)))
     #'(with-reference-compilers ([contract-var immutable-reference-compiler])
         (value->flat-contract e))]))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))

;;
;; macros
;;

(define-syntax ->
  (contract-macro
   (syntax-parser
     [(_ x ... y)
      #'(function (arguments [_ x] ...) (results [_ y]))])))
