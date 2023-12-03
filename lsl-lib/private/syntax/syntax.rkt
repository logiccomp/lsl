#lang racket/base

;;
;; provide
;;

(provide (for-space contract-space (all-defined-out))
         (for-syntax contract-macro)
         define-annotated
         annotate
         define-contract
         contract-generate
         ->)

;;
;; require
;;

(require (for-syntax (only-in ee-lib flip-intro-scope)
                     racket/base
                     racket/function
                     racket/list
                     racket/match
                     racket/set
                     racket/sequence
                     syntax/id-table
                     syntax/id-set
                     syntax/parse
                     syntax/parse/lib/function-header
                     syntax/parse/class/struct-id
                     syntax/transformer
                     mischief/dict
                     mischief/sort)
         syntax-spec
         syntax/location
         "../runtime/contract.rkt"
         "../runtime/or.rkt"
         "../runtime/flat.rkt"
         "../runtime/function.rkt"
         "../runtime/recursive.rkt"
         "../runtime/struct.rkt")

;;
;; `define-annotated`
;;

(begin-for-syntax
  (define ctc-sc (make-syntax-introducer))

  (define-syntax-class define-header
    (pattern x:function-header
             #:with name #'x.name
             #:attr make-body (λ (body) #`(λ x.params #,body)))
    (pattern x:id
             #:with name #'x
             #:attr make-body (λ (body) body))))

(define-syntax annotate
  (syntax-parser
    [(_ name:id ctc:expr)
     #`(: #,(ctc-sc #'name) ctc)]))

(define-syntax define-annotated
  (syntax-parser
    [(_ ?head:define-header ?body:expr)
     #:do [(define ctc (syntax-local-value (ctc-sc #'?head.name) (const #f)))]
     #:with ?new-body ((attribute ?head.make-body) #'?body)
     (if ctc
         #`(define ?head.name
             (let ([pos (positive-blame-struct '?head.name (quote-module-name))]
                   [neg (negative-blame-struct '?head.name (quote-module-name))]
                   [compiled (compile* #,(flip-intro-scope ctc))])
               (((contract-struct-protect compiled) ?new-body pos) neg)))
         #'(define ?head.name ?new-body))]))

;;
;; contract grammar
;;

(syntax-spec
 (binding-class contract-var)
 (extension-class contract-macro #:binding-space contract-space)

 (host-interface/definitions
  (: name:id ctc:contract)
  #`(define-syntax name (flip-intro-scope #'ctc)))

 (host-interface/expression
  (contract-generate ctc:contract)
  #'(contract-generate-function (compile ctc)))

 (nonterminal contract
   #:allow-extension contract-macro
   #:binding-space contract-space
   name:contract-var
   (Flat opt:flat-clause ...)
   (OneOf ctc:contract ...)
   (Struct name:id ctc:contract ...)
   (Recursive name:contract-var ctc:contract)
   #:binding {(bind name) ctc}
   (Function arg:function-clause ... res-ctc:contract)
   #:binding {(recursive arg) res-ctc})

 (nonterminal/two-pass function-clause
   [(~datum _) arg-ctc:contract]
   [arg:racket-var arg-ctc:contract]
   #:binding (export arg))

 (nonterminal flat-clause
   #:binding-space contract-space
   (domain dom-ctc:contract)
   (check check-expr:racket-expr)
   (generate gen-expr:racket-expr)
   (symbolic sym-expr:racket-expr)))

;;
;; `define-alias`
;;

(define-syntax define-contract
  (syntax-parser
    [(_ name:id ctc:expr)
     #'(define-syntax name
         (contract-macro
          (syntax-parser
            [_ #'ctc])))]))

;;
;; `compile`
;;

(define-syntax compile*
  (syntax-parser
    [(_ ctc)
     #'(with-reference-compilers
         ([contract-var-class immutable-reference-compiler])
         (compile ctc))]))

(begin-for-syntax
  (define get-name
    (syntax-parser
      [(_ ctc)
       (match (syntax-property #'ctc 'origin)
         [(list _ ... orig-stx)
          #:when (identifier? orig-stx)
          orig-stx]
         [_ #f])]
      [_ #f])))

(define-syntax (compile stx)
  (define name (get-name stx))
  (syntax-parse stx
    #:datum-literals (Flat domain check generate symbolic Function OneOf Struct Recursive)
    [(_ (Flat (~alt (~optional (domain dom-ctc))
                    (~optional (check check-expr))
                    (~optional (generate gen-expr))
                    (~optional (symbolic sym-expr))) ...))
     #`(flat-contract
        #'#,name
        (~? (compile dom-ctc) #false)
        (~? check-expr #false)
        (~? gen-expr #false)
        (~? sym-expr #false))]
    [(_ (Function [x a] ... r))
     #:with (k ...) (function-dependencies (syntax->list #'([x a] ...)))
     #`(function-contract
          '#,name
          (list (#%datum . k) ...)
          (list (λ* (x ...) (compile a)) ...)
          (λ* (x ...) (compile r)))]
    [(_ (OneOf ctc ...))
     #`(or-contract '#,name (list (compile ctc) ...))]
    [(_ (Struct sname:struct-id ctc ...))
     #`(struct-contract '#,name
                        sname.constructor-id
                        sname.predicate-id
                        (list (compile ctc) ...))]
    [(_ (Recursive x:id ctc))
     #'(letrec ([x (compile ctc)]) x)]
    [(_ name:id)
     #'(recursive-contract (λ () name))]))

(define-syntax λ*
  (syntax-parser
    [(_ (x:id ...) e:expr)
     #:with (x* ...)
     (for/list ([x (in-syntax #'(x ...))])
       (if (eq? (syntax-e x) '_) (gensym) x))
     #'(λ (x* ...) e)]))

;;
;; free variables
;;

(begin-for-syntax
  (define MT (immutable-bound-id-set))
  (define fv
    (syntax-parser
      #:datum-literals (Flat domain check generate symbolic Function OneOf Struct Recursive)
      [(Flat (~alt (~optional (domain dom-ctc))
                   (~optional (check check-expr))
                   (~optional (generate gen-expr))
                   (~optional (symbolic sym-expr))) ...)
       (bound-id-set-union
        (if (attribute dom-ctc) (fv #'dom-ctc) MT)
        (if (attribute check-expr) (free-variables #'check-expr) MT)
        (if (attribute gen-expr) (free-variables #'gen-expr) MT)
        (if (attribute sym-expr) (free-variables #'sym-expr) MT))]
      [(Function [x a] ... r)
       (for/fold ([acc MT])
                 ([e (in-sequences (in-syntax #'(a ... r)))])
         (bound-id-set-union acc (fv e)))]
      [(Struct _ ctc ...)
       (for/fold ([acc MT])
                 ([e (in-sequences (in-syntax #'(ctc ...)))])
         (bound-id-set-union acc (fv e)))]
      [(OneOf ctc ...)
       (for/fold ([acc MT])
                 ([e (in-sequences (in-syntax #'(ctc ...)))])
         (bound-id-set-union acc (fv e)))]
      [(Recursive _ ctc) (fv #'ctc)]
      [_:id MT])))

;;
;; macros
;;

(define-syntax ->
  (contract-macro
   (syntax-parser
     [(_ x ... y)
      #'(Function [_ x] ... y)])))

;;
;; dependency
;;

(begin-for-syntax
  (define (function-dependencies stxs)
    (define id-index-table
      (for/fold ([acc (make-immutable-bound-id-table)])
                ([stx (in-list stxs)]
                 [k (in-naturals)])
        (syntax-parse stx
          [(name:id body:expr)
           (bound-id-table-set acc #'name k)])))
    (define dep-hash
      (for/hash ([stx (in-list stxs)])
        (syntax-parse stx
          [(name:id body:expr)
           (define deps
             (for/list ([var (in-bound-id-set (fv #'body))])
               (bound-id-table-ref id-index-table var #false)))
           (values #'name (filter values deps))])))
    (define neighbors (dict->procedure #:failure (const empty) dep-hash))
    (topological-sort (range (length stxs)) neighbors))

  (define current-free-variables (make-parameter #f))

  (define record-var-compiler
    (make-variable-like-transformer
     (λ (id)
       (current-free-variables
        (bound-id-set-add (current-free-variables) id))
       #''_)))

  (define (free-variables stx)
    (define fv-set (immutable-bound-id-set))
    (define stx*
      #`(with-reference-compilers ([contract-var-class record-var-compiler])
          #,stx))
    (parameterize ([current-free-variables fv-set])
      (local-expand stx* 'expression null))
    fv-set))
