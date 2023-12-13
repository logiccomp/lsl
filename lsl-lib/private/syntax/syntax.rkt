#lang racket/base

;;
;; provide
;;

(provide
 ;; contract literals
 Flat
 OneOf
 Struct
 Function

 ;; flat literals
 domain
 check
 generate
 symbolic

 ;; others
 $define
 annotate
 define-contract
 contract-generate
 define-contract-syntax)

;;
;; require
;;

(require (for-syntax ee-lib
                     ee-lib/persistent-id-table
                     racket/base
                     racket/function
                     racket/match
                     racket/list
                     racket/sequence
                     racket/syntax-srcloc
                     syntax/id-table
                     syntax/id-set
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/parse/class/struct-id
                     mischief/dict
                     mischief/sort
                     "unbound-vars.rkt")
         ee-lib/define
         syntax/location
         "../runtime/contract.rkt"
         "../runtime/oneof.rkt"
         "../runtime/flat.rkt"
         "../runtime/function.rkt"
         "../runtime/recursive.rkt"
         "../runtime/struct.rkt")

;;
;; syntax
;;

(define-literal-forms contract-literals
  "contract constructor must occur within a contract"
  (Name Flat OneOf Struct Recursive Function))

(define-literal-forms flat-literals
  "literal clause must occur within Flat"
  (domain check generate symbolic))

(define-extensible-syntax contract-syntax)

(begin-for-syntax
  (define contract-table (make-free-id-table)))

;;
;; expander
;;

(begin-for-syntax
  (define rec-vars (make-parameter (immutable-free-id-set)))
  (define rec-table (make-parameter (make-immutable-free-id-table)))
  (define used-vars (make-parameter (immutable-free-id-set)))

  (define/hygienic-metafunction (expand-contract this-stx)
    #:expression
    (define/syntax-parse (_ stx) this-stx)
    (define/syntax-parse qstx #`#'stx)
    (syntax-parse #'stx
      #:literal-sets (contract-literals flat-literals)
      [(Name name:id e:expr)
       #'(Name name (expand-contract e))]
      [(Flat (~alt (~optional (domain d:expr))
                   (~optional (check c:expr))
                   (~optional (generate g:expr))
                   (~optional (symbolic s:expr))) ...)
       #'(Flat qstx
               (~? (expand-contract d) #f)
               (~? c #f)
               (~? g #f)
               (~? s #f))]
      [(Function [x:id a:expr] ... r:expr)
       #'(Function qstx ([x (expand-contract a)] ...) (expand-contract r))]
      [(OneOf e:expr ...)
       #'(OneOf qstx (expand-contract e) ...)]
      [(Struct s:struct-id e:expr ...)
       #'(Struct qstx s (expand-contract e) ...)]
      [(Recursive x:id e:expr)
       (parameterize ([rec-vars (free-id-set-add (rec-vars) #'x)]
                      [used-vars (used-vars)])
         (define/syntax-parse ^e #'(expand-contract e))
         (if (free-id-set-empty? (used-vars))
             #'^e
             #'(Recursive qstx x ^e)))]
      [(Recursive (x:id y:id ...) e:expr)
       (parameterize ([rec-table (free-id-table-set (rec-table) #'x (syntax-e #'(y ...)))]
                      [used-vars (used-vars)])
         (define/syntax-parse ^e #'(expand-contract e))
         (if (free-id-set-empty? (used-vars))
             #'^e
             #'(Recursive qstx x ^e)))]
      [head:id
       #:when (free-id-set-member? (rec-vars) #'head)
       #:do [(used-vars (free-id-set-add (used-vars) #'head))]
       #'head]
      [(head:id var:id ...)
       #:do [(define vars (syntax-e #'(var ...)))]
       #:do [(define args (free-id-table-ref (rec-table) #'head #f))]
       #:when args
       #:do [(used-vars (free-id-set-add (used-vars) #'head))]
       (unless (and (= (length args) (length vars))
                    (andmap free-identifier=? args vars))
         (define err-str (format "recursive call must be exactly ~a" (map syntax-e (cons #'head args))))
         (raise-syntax-error #f err-str #'stx))
       #'head]
      [(~or head:id (head:id e:expr ...))
       #:when (contract-syntax-rep? (lookup #'head))
       #:do [(define (get stx) (contract-syntax-transform (lookup #'head) stx))]
       #:with result (apply-as-transformer get #'head 'expression #'stx)
       #'(expand-contract result)]
      [e:expr
       #'(expand-contract (Flat (check e)))]))

  (struct exn:fail:cyclic exn:fail (srclocs)
    #:property prop:exn:srclocs
    (λ (self) (exn:fail:cyclic-srclocs self)))

  ;; TODO: no syntax-e?
  (define (sort-indices stx ids fvs)
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
      (dict->procedure #:failure (const '()) dep-hash))
    (define (cycle _)
      (raise (exn:fail:cyclic "cannot have cyclic dependency"
                              (current-continuation-marks)
                              (list (syntax-srcloc stx)))))
    (topological-sort (range (hash-count index-hash)) neighbors
                      #:cycle cycle)))

;;
;; compiler
;;

(begin-for-syntax
  (define-syntax-class ctc
    (pattern #f
             #:with compiled #'#f)
    (pattern e:expr
             #:with compiled #'(compile-contract e)))

  (define/hygienic-metafunction (compile-contract this-stx)
    #:expression
    (define/syntax-parse (_ stx) this-stx)
    (syntax-parse #'stx
      #:literal-sets (contract-literals flat-literals)
      [(Name x e:ctc)
       #'(name-contract 'x e.compiled)]
      [(Flat q d:ctc c g s)
       #'(flat-contract q d.compiled c g s)]
      [(Function q ([x a:ctc] ...) r:ctc)
       #:with (a* ...) #'((expand-racket (λ* (x ...) a.compiled)) ...)
       #:with (y ...) #'((unbound-racket a*) ...)
       #:with (k ...) (sort-indices #'stx #'(x ...) #'(y ...))
       #:with r* #'(λ* (x ...) r.compiled)
       #'(function-contract q (list (#%datum . k) ...) (list a* ...) r*)]
      [(OneOf q e:ctc ...)
       #'(oneof-contract q e.compiled ...)]
      [(Struct q s:struct-id e:ctc ...)
       #'(struct-contract q s.constructor-id s.predicate-id e.compiled ...)]
      [(Recursive q x e:ctc)
       #'(recursive-contract q (λ (x) e.compiled))]
      [x:id #'x]))

  (define/hygienic-metafunction (expand-racket this-stx)
    #:expression
    (define/syntax-parse (_ stx) this-stx)
    (local-expand #'stx 'expression null))

  (define/hygienic-metafunction (unbound-racket this-stx)
    #:expression
    (syntax-parse this-stx
      #:literals (#%plain-lambda)
      [(_ (#%plain-lambda (x ...) body))
       (datum->syntax #f (unbound-vars #'body))])))

;;
;; interface macros
;;

(begin-for-syntax
  (define-syntax-class define-header
    (pattern x:function-header
             #:with name #'x.name
             #:attr make-body (λ (body) #`(λ x.params #,body)))
    (pattern x:id
             #:with name #'x
             #:attr make-body (λ (body) body))))

(define-syntax expand+compile-contract
  (syntax-parser
    [(_ ctc)
     #:with ctc* #`(expand-contract ctc)
     #'(compile-contract ctc*)]))

(define-syntax $define
  (syntax-parser
    [(_ ?head:define-header ?body:expr)
     #:with ?new-body ((attribute ?head.make-body) #'?body)
     (match (free-id-table-ref contract-table #'?head.name #f)
       [(? not) #'(define ?head.name ?new-body)]
       [ctc     #`(define ?head.name
                    (let ([pos (positive-blame-struct '?head.name (quote-module-name))]
                          [neg (negative-blame-struct '?head.name (quote-module-name))]
                          [compiled (expand+compile-contract #,(flip-intro-scope ctc))])
                      ((((contract-struct-protect compiled) compiled) ?new-body pos) neg)))])]))

(define-syntax annotate
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(begin-for-syntax
         (free-id-table-set! contract-table #'name #'ctc))]))

(define-syntax define-contract
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(define-contract-syntax name
         (syntax-parser
           [_:id #'(Name name (Recursive name ctc))]))]
    [(_ (name:id param:id ...) ctc:expr)
     #'(define-contract-syntax name
         (syntax-parser
           [(_:id param ...)
            #'(Name name (Recursive (name param ...) ctc))]))]))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))

(define-syntax contract-generate
  (syntax-parser
    [(_ ctc:expr)
     #'(contract-generate-function
        (expand+compile-contract ctc))]))


(define-contract Integer
  (Flat (check integer?)))

(define-contract-syntax ->
  (syntax-parser
    [(_ d:expr ... c:expr)
     #'(Function [_ d] ... c)]))
