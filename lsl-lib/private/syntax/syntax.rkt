#lang racket/base

;;
;; provide
;;

(provide
 (for-syntax contract-table)

 ;; contract literals
 Flat
 List
 OneOf
 And
 Struct
 Function

 ;; flat literals
 domain
 check
 generate
 shrink
 symbolic

 ;; others
 $define
 annotate
 define-contract
 contract-generate
 contract-symbolic
 contract-predicate
 contract-shrink
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
                     "loc.rkt"
                     "unbound-vars.rkt")
         ee-lib/define
         syntax/location
         "../runtime/contract.rkt"
         "../runtime/oneof.rkt"
         "../runtime/and.rkt"
         "../runtime/flat.rkt"
         "../runtime/function.rkt"
         "../runtime/recursive.rkt"
         "../runtime/struct.rkt")

;;
;; syntax
;;

(define-literal-forms contract-literal
  "contract constructor must occur within a contract"
  (Flat List OneOf And Struct Recursive Function))

(define-literal-forms flat-literal
  "literal clause must occur within Flat"
  (domain check generate shrink symbolic))

(define-extensible-syntax contract-syntax)

(begin-for-syntax
  (define contract-table (make-free-id-table)))

;;
;; expander
;;

(begin-for-syntax
  (define rec-table (make-parameter (make-immutable-free-id-table)))
  (define used-vars (make-parameter (immutable-free-id-set)))

  (define (non-wildcard? x)
    (not (eq? (syntax-e x) '_)))

  (define/hygienic-metafunction (expand-contract this-stx)
    #:expression
    (define/syntax-parse (_ stx) this-stx)
    (define/syntax-parse qstx
      (or (syntax-property #'stx 'original) #'stx))
    (syntax-parse #'stx
      #:literal-sets (contract-literal flat-literal)
      [(Flat ~! (~alt (~optional (domain d:expr))
                      (~optional (check c:expr))
                      (~optional (generate g:expr))
                      (~optional (shrink h:expr))
                      (~optional (symbolic s:expr))) ...)
       #'(Flat #'qstx
               (~? (expand-contract d) #f)
               (~? c #f)
               (~? g #f)
               (~? h #f)
               (~? s #f))]
      [(List ~! (~optional n:expr #:defaults ([n #'#f])) e:expr)
       #'(List #'qstx n (expand-contract e))]
      [(Function ~! [x:id a:expr] ... r:expr)
       #:fail-when
       (check-duplicate-identifier
        (filter non-wildcard? (syntax-e #'(x ...))))
       "duplicate identifier"
       #'(Function #'qstx ([x (expand-contract a)] ...) (expand-contract r))]
      [(OneOf ~! e:expr ...+)
       #'(OneOf #'qstx (expand-contract e) ...)]
      [(And ~! e:expr ...+)
       #'(And #'qstx (expand-contract e) ...)]
      [(Struct ~! s:struct-id e:expr ...)
       #'(Struct #'qstx s (expand-contract e) ...)]
      [(Recursive ~! (~and (~or head:id (head:id a:expr ...)) self:expr) e:expr)
       (parameterize ([rec-table (free-id-table-set (rec-table) #'head #'self)]
                      [used-vars (used-vars)])
         (define/syntax-parse ^e #'(expand-contract e))
         (if (free-id-set-empty? (used-vars))
             #'^e
             #'(Recursive #'qstx head ^e)))]
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
       #:with result (apply-as-transformer get #'head 'expression #'stx)
       #'(expand-contract result)]
      [e:expr
       #'(expand-contract (Flat (check e)))]))

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
      (dict->procedure #:failure (const '()) dep-hash))
    (define (cycle _)
      (raise (exn:fail:cyclic "cannot have cyclic dependency"
                              (current-continuation-marks)
                              (list (syntax-srcloc stx)))))
    (topological-sort (range n) neighbors #:cycle cycle)))

;;
;; compiler
;;

(begin-for-syntax
  (define-syntax-class ctc
    #:attributes (compiled compiled.c)
    (pattern #f
             #:with compiled #'#f
             #:declare compiled (expr/c #'any/c))
    (pattern e:expr
             #:with compiled #'(compile-contract e)
             #:declare compiled (expr/c #'flat-contract-struct?)))

  (define/hygienic-metafunction (compile-contract this-stx)
    #:expression
    (define/syntax-parse (_ stx) this-stx)
    (syntax-parse #'stx
      #:literal-sets (contract-literal flat-literal)
      [(Flat q d:ctc c g h s)
       #'(flat-contract q d.compiled c g h s)]
      [(List q n:expr e:ctc)
       #'(list-contract q n e.compiled)]
      [(Function q ([x a:ctc] ...) r:ctc)
       #:with (a* ...) #'((expand-racket (λ* (x ...) a.compiled)) ...)
       #:with (y ...) #'((unbound-racket a*) ...)
       #:with (k ...) (sort-indices #'stx #'(x ...) #'(y ...))
       #:with r* #'(λ* (x ...) r.compiled)
       #'(function-contract q (list (#%datum . k) ...) (list a* ...) r*)]
      [(OneOf q e:ctc ...)
       #'(oneof-contract q e.compiled.c ...)]
      [(And q e:ctc ...)
       #'(and-contract q e.compiled.c ...)]
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
             #:attr make-body (λ (body) #`(procedure-rename (λ x.params #,body) 'name)))
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
     (free-id-table-set! contract-table #'name #'ctc)
     #'(void)]))

(define-syntax define-contract
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(define-contract-syntax name
         (λ (stx)
           (syntax-parse stx
             [_:id
              (define stx* (syntax-property stx 'original stx))
              (syntax/whole-loc stx* (Recursive name ctc))])))]
    [(_ (name:id param:id ...) ctc:expr)
     #'(define-contract-syntax name
         (λ (stx)
           (syntax-parse stx
             [(_:id param ...)
              (define stx* (syntax-property stx 'original stx))
              (syntax/whole-loc stx* (Recursive (name param ...) ctc))])))]))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))

(define-syntax contract-generate
  (syntax-parser
    [(_ ctc:expr (~optional fuel:expr))
     #'(contract-generate-function/error
        (expand+compile-contract ctc)
        (~? fuel))]))

(define (contract-generate-function/error ctc [fuel 5])
  (define result (contract-generate-function ctc fuel))
  (if (contract-generate-failure? result)
      (generate-error (contract-struct-syntax ctc))
      result))

(define-syntax contract-symbolic
  (syntax-parser
    [(_ ctc:expr)
     #'(contract-symbolic-function
        (expand+compile-contract ctc))]))

(define-syntax contract-predicate
  (syntax-parser
    [(_ ctc:expr)
     #'(flat-contract-struct-predicate
        (expand+compile-contract ctc))]))

(define-syntax contract-shrink
  (syntax-parser
    [(_ ctc:expr val:expr)
     #'(contract-shrink-function
        (expand+compile-contract ctc)
        val)]))
