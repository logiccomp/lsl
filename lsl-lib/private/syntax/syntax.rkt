#lang racket/base

;;
;; provide
;;

(provide (for-space contract-space
                    flat
                    domain
                    check
                    generate
                    symbolic
                    function
                    one-of
                    Struct)
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
             (let ([pos (positive-blame-struct (quote-module-name))]
                   [neg (negative-blame-struct (quote-module-name))])
               (((contract-struct-protect (compile #,(flip-intro-scope ctc))) ?new-body pos) neg)))
         #'(define ?head.name ?new-body))]))

;;
;; contract grammar
;;

(syntax-spec
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
   (flat opt:flat-clause ...)
   (one-of ctc:contract ...)
   (Struct name:id ctc:contract ...)
   (function [arg:racket-var arg-ctc:contract] ... res-ctc:contract)
   #:binding {(bind arg) arg-ctc res-ctc})

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
            [_ (syntax-property #'ctc 'inferred-name (syntax->datum #'name))])))]))

;;
;; `compile`
;;

(define-syntax (compile stx)
  (syntax-parse stx
    #:datum-literals (flat domain check generate symbolic function one-of Struct)
    [(_ (~and (flat (~alt (~optional (domain dom-ctc))
                          (~optional (check check-expr))
                          (~optional (generate gen-expr))
                          (~optional (symbolic sym-expr))) ...)
              flat-stx))
     #:with name (syntax-property #'flat-stx 'inferred-name)
     #'(flat-contract
        'name
        (~? (compile dom-ctc) #false)
        (~? check-expr #false)
        (~? gen-expr #false)
        (~? sym-expr #false))]
    [(_ (~and (function [x a] ... r) fun-stx))
     #:with (k ...) (function-dependencies (syntax->list #'([x a] ...)))
     #:with name (syntax-property #'fun-stx 'inferred-name)
     #'(with-reference-compilers ([contract-var-class immutable-reference-compiler])
         (function-contract
          'name
          (list (#%datum . k) ...)
          (list (λ* (x ...) (compile a)) ...)
          (λ* (x ...) (compile r))))]
    [(_ (~and (one-of ctc ...) or-stx))
     #:with name (syntax-property #'or-stx 'inferred-name)
     #'(or-contract 'name (list (compile ctc) ...))]
    [(_ (~and (Struct sname:struct-id ctc ...) sstx))
     #:with name (syntax-property #'fun-stx 'inferred-name)
     #'(struct-contract 'name sname.constructor-id (list (compile ctc) ...))]))

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
      #:datum-literals (flat domain check generate symbolic function one-of)
      [(flat (~alt (~optional (domain dom-ctc))
                   (~optional (check check-expr))
                   (~optional (generate gen-expr))
                   (~optional (symbolic sym-expr))) ...)
       (bound-id-set-union
        (if (attribute dom-ctc) (fv #'dom-ctc) MT)
        (if (attribute check-expr) (free-variables #'check-expr) MT)
        (if (attribute gen-expr) (free-variables #'gen-expr) MT)
        (if (attribute sym-expr) (free-variables #'sym-expr) MT))]
      [(function [x a] ... r)
       (for/fold ([acc MT])
                 ([e (in-sequences (in-syntax #'(a ... r)))])
         (bound-id-set-union acc (fv e)))]
      [(Struct _ ctc ...)
       (for/fold ([acc MT])
                 ([e (in-sequences (in-syntax #'(ctc ...)))])
         (bound-id-set-union acc (fv e)))]
      [(one-of ctc ...)
       (for/fold ([acc MT])
                 ([e (in-sequences (in-syntax #'(ctc ...)))])
         (bound-id-set-union acc (fv e)))])))

;;
;; macros
;;

(define-syntax ->
  (contract-macro
   (syntax-parser
     [(_ x ... y)
      #'(function [_ x] ... y)])))

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
