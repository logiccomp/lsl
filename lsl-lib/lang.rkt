#lang racket/base

;;
;; provide
;;

(require (for-syntax racket/base
                     racket/string)
         racket/provide)

(begin-for-syntax
  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

(provide (filtered-out
          (strip "$")
          (combine-out $require
                       ;; from contract.rkt`
                       ;$define
                       $define-struct
                       $lambda
                       $λ
                       $begin
                       $letrec
                       $let
                       $let*
                       $cond
                       $if
                       $and
                       $or
                       $quote
                       $#%datum

                       $empty))
          (filtered-out
          (strip "^")
          (combine-out ^true
                       ^false

                       ^-
                       ^<
                       ^<=
                       ^>
                       ^>=
                       ^abs
                       ^add1
                       ^ceiling
                       ^even?
                       ^exact->inexact
                       ^floor
                       ^inexact->exact
                       ^integer?
                       ^max
                       ^min
                       ^modulo
                       ^negative?
                       ^number?
                       ^odd?
                       ^pi
                       ^positive?
                       ^quotient
                       ^real?
                       ^remainder
                       ^sgn
                       ^sub1
                       ^zero?

                       ^boolean?
                       ^not

                       ^append
                       ^assoc
                       ^assq
                       ^car
                       ^cdr
                       ^cons
                       ^cons?
                       ^eighth
                       ^empty?
                       ^fifth
                       ^first
                       ^fourth
                       ^length
                       ^list
                       ^list-ref
                       ^list?
                       ^member
                       ^memq
                       ^null
                       ^null?
                       ^remove
                       ^rest
                       ^reverse
                       ^second
                       ^seventh
                       ^sixth
                       ^third

                       ^eq?
                       ^equal?
                       ^identity

                       ^*
                       ^+
                       ^/
                       ^=

                       ^andmap
                       ^apply
                       ^argmax
                       ^argmin
                       ^compose
                       ^filter
                       ^foldl
                       ^foldr
                       ^for-each
                       ^map
                       ^memf
                       ^ormap
                       ^procedure?
                       ^sort))
         #%app
         #%module-begin
         #%top-interaction

         (for-space contract-space
                    flat
                    domain
                    check
                    generate
                    symbolic
                    function
                    one-of
                    Struct)
         (rename-out
          [define-annotated define]
          [annotate :])
         Integer
         Boolean
         Real
         ->
         define-contract
         contract-generate
         contract-exercise
         contract-verify)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse
                     threading)
         (prefix-in ^ rosette/safe)
         rosette/solver/smt/z3
         syntax/parse/define
         "private/syntax/syntax.rkt"
         "private/runtime/contract.rkt"
         "private/runtime/flat.rkt"
         "private/runtime/function.rkt")

(^current-solver (z3 #:path (find-executable-path "z3")))

;;
;; syntax
;;

(define-syntax-parse-rule ($require (~or* mod:string mod:id) ...)
  (^require mod ...))

(define-syntax $define
  (syntax-parser
    [(_ (name:id param:id ...) body:expr)
     #'(^define (name param ...) body)]
    [(_ name:id rhs:expr)
     #'(^define name rhs)]))

(begin-for-syntax
  (define (struct-name->contract-name stx)
    (~> stx
        syntax-e
        symbol->string
        (string-replace "-" " ")
        string-titlecase
        (string-replace " " "")
        string->symbol
        (datum->syntax stx _)))

  (define (struct-contract-macro sname)
    (contract-macro
     (syntax-parser
       [(_ ctc ...)
        #`(Struct #,sname ctc ...)]))))

(define-syntax $define-struct
  (syntax-parser
    [(_ name:id (field:id ...))
     #:with Name (struct-name->contract-name #'name)
     #'(begin
         (define-syntax Name (struct-contract-macro #'name))
         (^struct name (field ...) #:transparent))]))

(define-syntax-parse-rule ($lambda (param:id ...) body:expr)
  (^lambda (param ...) body))

(define-syntax-parse-rule ($λ (param:id ...) body:expr)
  (^λ (param ...) body))

(define-syntax-parse-rule ($begin body:expr ...+)
  (^begin body ...))

(define-syntax-parse-rule ($letrec ([var:id rhs:expr] ...) body:expr)
  (^letrec ([var rhs] ...) body))

(define-syntax-parse-rule ($let ([var:id rhs:expr] ...) body:expr)
  (^let ([var rhs] ...) body))

(define-syntax-parse-rule ($let* ([var:id rhs:expr] ...) body:expr)
  (^let* ([var rhs] ...) body))

(define-syntax-parse-rule ($cond [guard:expr arm:expr] ...+)
  (^cond [guard arm] ...))

(define-syntax-parse-rule ($if guard:expr then:expr else:expr)
  (^if guard then else))

(define-syntax-parse-rule ($and arg0:expr arg:expr ...+)
  (^and arg0 arg ...))

(define-syntax-parse-rule ($or arg0:expr arg:expr ...+)
  (^or arg0 arg ...))

(define-syntax-parse-rule ($quote body:expr)
  (^quote body))

(define-syntax $#%datum
  (syntax-parser
    [(_ . (~or e:number e:boolean e:string e:character))
     #'(^#%datum . e)]))

;;
;; built-in contracts
;;

(define ((predicate->symbolic predicate))
  (^define-symbolic* x predicate) x)

(define-contract Integer
  (flat (check ^integer?)
        (symbolic (predicate->symbolic ^integer?))
        (generate (λ () (random -100 100)))))

(define-contract Boolean
  (flat (check ^boolean?)
        (symbolic (predicate->symbolic ^boolean?))
        (generate (λ () (< (random) 1/2)))))

(define-contract Real
  (flat (check ^real?)
        (symbolic (predicate->symbolic ^real?))
        (generate (λ () (- (* 200 (random)) 100)))))

;;
;; TODO: testing
;;

#|

    (pattern (check-expect arg0:exp arg1:exp))
    (pattern (check-random arg0:exp arg1:exp))
    (pattern (check-within arg0:exp arg1:exp arg2:exp))
    (pattern (check-member-of arg0:exp arg:exp ...))
    (pattern (check-range arg0:exp arg1:exp arg2:exp))
    (pattern (check-satisfied arg0:exp arg1:exp))
    (pattern (check-error arg0:exp arg1:exp))
    (pattern (check-error arg:exp)))

|#

;;
;; standard library
;;

(define $empty ^null)

;;
;; reader
;;

(module reader syntax/module-reader
  lsl)
