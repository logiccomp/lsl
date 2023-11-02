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
                       $local
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

         (for-space contract-space (all-defined-out))
         (rename-out
          [define-annotated define]
          [annotate :])
         ->
         define-alias
         contract
         contract-generate
         contract-interact)

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse)
         (prefix-in ^ rosette/safe)
         syntax/parse/define
         "syntax.rkt"
         "runtime/contract.rkt"
         "runtime/flat.rkt"
         "runtime/function.rkt")

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

(define-syntax-parse-rule ($define-struct name:id (field:id ...))
  (^struct name (field ...) #:prefab))

(define-syntax-parse-rule ($lambda (param:id ...) body:expr)
  (^lambda (param ...) body))

(define-syntax-parse-rule ($λ (param:id ...) body:expr)
  (^λ (param ...) body))

(define-syntax-parse-rule ($local [def:expr ...] body:expr)
  (^local [def ...] body))

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
