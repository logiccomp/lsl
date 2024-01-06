#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     ee-lib
                     syntax/id-table
                     syntax/parse
                     syntax/parse/class/struct-id)
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax contract-table
                     expand-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(begin-for-syntax
  (define contract-table (make-free-id-table))

  (define/hygienic-metafunction (expand-contract form-stx)
    #:expression
    (define/syntax-parse (_ stx) form-stx)
    (syntax-replace-srcloc #'stx
      (syntax-parse #'stx
        #:literal-sets (contract-literal flat-literal function-literal)
        [(Flat ~! (~alt (~optional (domain dom:expr))
                        (~optional (check chk:expr))
                        (~optional (generate gen:expr))
                        (~optional (shrink shk:expr))) ...)
         #'(Flat (domain (~? dom #f))
                 (check (~? chk #f))
                 (generate (~? gen #f))
                 (shrink (~? shk #f)))]
        [(Function ~! (~alt (~once (arguments [x:id a:expr] ...))
                            (~once (result r:expr))
                            (~optional (raises e:struct-id ...)
                                       #;#:defaults #;([(e 1) null]))) ...)
         #:fail-when
         (check-duplicate-identifier
          (filter non-wildcard? (syntax-e #'(x ...))))
         "duplicate identifier"
         #'(Function (arguments [x (expand-contract a)] ...)
                     (result (expand-contract r))
                     (raises (~? (~@ e ...))))]
        [(~or head:id (head:id e:expr ...))
         #:when (contract-syntax-rep? (lookup #'head))
         #:do [(define (get stx) (contract-syntax-transform (lookup #'head) stx))]
         #:with res (apply-as-transformer get #'head 'expression #'stx)
         #'(expand-contract res)])))

  (define (syntax-replace-srcloc loc-stx stx)
    (syntax-property
     (datum->syntax stx (syntax-e stx) loc-stx stx)
     'unexpanded loc-stx))

  (define (non-wildcard? x)
    (not (eq? (syntax-e x) '_))))
