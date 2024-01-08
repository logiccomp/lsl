#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     ee-lib
                     syntax/id-table
                     syntax/id-set
                     syntax/parse
                     syntax/parse/class/struct-id)
         "grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (for-syntax expand-contract))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(begin-for-syntax
  (define rec-table (make-parameter (make-immutable-free-id-table)))
  (define used-vars (make-parameter (immutable-free-id-set)))

  (define/hygienic-metafunction (expand-contract form-stx)
    #:expression
    (define/syntax-parse (_ stx) form-stx)
    (syntax-replace-srcloc #'stx
      (syntax-parse #'stx
        #:literal-sets (contract-literal flat-literal function-literal)
        [(Flat ~! (~alt (~optional (check chk:expr))
                        (~optional (generate gen:expr))
                        (~optional (shrink shk:expr))
                        (~optional (symbolic sym:expr))) ...)
         #'(Flat (check (~? chk #f))
                 (generate (~? gen #f))
                 (shrink (~? shk #f))
                 (symbolic (~? sym #f)))]
        [(Function ~! (~alt (~once (arguments [x:id a:expr] ...))
                            (~once (result r:expr))
                            (~optional (raises e:struct-id ...))) ...)
         #:fail-when
         (check-duplicate-identifier
          (filter non-wildcard? (syntax-e #'(x ...))))
         "duplicate identifier"
         #'(Function (arguments [x (expand-contract a)] ...)
                     (result (expand-contract r))
                     (raises (~? (~@ e ...))))]
        [(OneOf ~! e:expr ...)
         #'(OneOf (expand-contract e) ...)]
        [(AllOf ~! e:expr ...)
         #'(AllOf (expand-contract e) ...)]
        [(Struct ~! s:struct-id e:expr ...)
         #'(Struct s (expand-contract e) ...)]
        [(List ~! e:expr)
         #'(List (expand-contract e))]
        [(Tuple ~! e:expr ...)
         #'(Tuple (expand-contract e) ...)]
        [(Recursive ~! (~and (~or head:id (head:id a:expr ...)) self:expr) e:expr)
         (parameterize ([rec-table (free-id-table-set (rec-table) #'head #'self)]
                        [used-vars (used-vars)])
           (define/syntax-parse ^e #'(expand-contract e))
           (if (free-id-set-member? (used-vars) #'head)
               #'(Recursive head ^e)
               #'^e))]
        [(~or head:id (head:id e:expr ...))
         #:do [(define self (free-id-table-ref (rec-table) #'head #f))]
         #:when self
         (unless (equal? (syntax->datum self) (syntax->datum #'stx))
           (define err-str (format "recursive call must be exactly ~a" (syntax->datum self)))
           (raise-syntax-error #f err-str #'stx))
         (used-vars (free-id-set-add (used-vars) #'head))
         #'head]
        [(~or head:id (head:id e:expr ...))
         #:when (contract-syntax-rep? (lookup #'head))
         #:do [(define (get stx) (contract-syntax-transform (lookup #'head) stx))]
         #:with res (apply-as-transformer get #'head 'expression #'stx)
         #'(expand-contract res)]
        [e:expr #'(expand-contract (Flat (check e)))])))

  (define (syntax-replace-srcloc loc-stx stx)
    (syntax-property
     (datum->syntax stx (syntax-e stx) loc-stx stx)
     'unexpanded loc-stx))

  (define (non-wildcard? x)
    (not (eq? (syntax-e x) '_))))
