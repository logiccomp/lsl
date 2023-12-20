#lang racket/base

;;
;; provide
;;

(require (for-syntax racket/base
                     racket/string
                     racket/syntax
                     racket/list)
         racket/provide
         racket/list
         racket/local
         (prefix-in @ racket/contract)
         (except-in rackunit check)
         rackunit/text-ui
         "private/syntax/lifting.rkt")

(begin-for-syntax
  (define ((strip pre) str)
    (and (string-prefix? str pre)
         (substring str (string-length pre)))))

(provide (filtered-out
          (strip "$")
          (combine-out $require
                       $define
                       $define-struct
                       $lambda
                       $λ
                       $begin
                       $letrec
                       $let
                       $let*
                       $cond
                       $else
                       $if
                       $and
                       $or
                       $quote
                       $#%datum
                       $...
                       $build-list

                       $check-expect
                       $check-contract
                       $verify-contract
                       $check-within
                       $check-member-of
                       $check-range
                       $check-satisfied
                       $check-error
                       $run-tests

                       $empty
                       $#%module-begin))
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
          #%top-interaction

          Flat
          Function
          OneOf
          And
          Struct

          domain
          check
          generate
          symbolic

          (all-from-out "library.rkt")
          (rename-out [annotate :])
          define-contract
          contract-generate
          contract-symbolic
          contract-predicate
          local

          (@contract-out (rename $boolean=? boolean=? (@-> boolean? boolean? boolean?)))


          (@contract-out (rename equal? string=? (@-> string? string? boolean?)))
          (lift-out
           sqrt
           expt

           random

           string-length
           format
           ;implode
           ;int->string
           list->string
           make-string
           ;replicate
           string
           ;string->int
           string->list
           string->number
           string->symbol
           ;string-alphabetic?
           ;string-contains-ci?
           string-contains?
           string-copy
           string-downcase
           ;string-ith
           #;string-length
           ;string-lower-case?
           ;string-numeric?
           string-ref
           string-upcase
           ;string-upper-case?
           ;string-whitespace?
           string?
           substring))

;;
;; require
;;

(require (for-syntax racket/base
                     syntax/parse
                     threading)
         (prefix-in ^ rosette/safe)
         (for-space contract-space "private/syntax/syntax.rkt")
         rosette/solver/smt/z3
         racket/string
         syntax/parse/define
         "library.rkt"
         "private/syntax/syntax.rkt"
         "private/runtime/contract.rkt"
         "private/runtime/flat.rkt"
         "private/runtime/function.rkt")

(^current-solver (z3 #:path (find-executable-path "z3")))

;;
;; syntax
;;

(define-syntax $...
  (syntax-parser
    [_
     #'(error "error: expected a finished expression, but found a template")]))

(define-syntax-parse-rule ($require (~or* mod:string mod:id) ...)
  (^require mod ...))

(define-syntax $#%module-begin
  (syntax-parser
    [(_ form:expr ...)
     #'(#%module-begin form ... ($run-tests))]))

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
    (syntax-parser
      [(_ ctc ...)
       #`(Struct #,sname ctc ...)])))

(define-syntax $define-struct
  (syntax-parser
    [(_ name:id (field:id ...))
     #:with Name (struct-name->contract-name #'name)
     #:with ctor (format-id #'name "make-~a" #'name)
     #'(begin
         (define-contract-syntax Name (struct-contract-macro #'name))
         (^struct name (field ...)
                  #:transparent
                  #:constructor-name ctor))]))

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

(define-syntax ($else stx)
  (raise-syntax-error 'else "else must be used in a cond"))

(define-syntax $cond
  (syntax-parser
    #:literals ($else)
    [(_ [(~and (~not $else) guard:expr) arm:expr] ...+ [$else final-arm:expr])
     #'(^cond [guard arm] ... [^else final-arm])]
    [(_ [(~and (~not $else) guard:expr) arm:expr] ...+)
     #:with final-arm #'(error "cond: all question results were false")
     #'(^cond [guard arm] ... [^else final-arm])]))

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
;; math
;;

(define (^sqr x)
  (^* x x))

;; strings

(define (explode s)
  (drop-right (drop (string-split s "") 1) 1))

(provide
 (lift-out explode))

;; lists

(define (member? x l)
  (not (equal? #f (member x l))))

(define ($build-list n f)
  (local ([define (bl-helper m)
            (cond [(^equal? m n) $empty]
                  [else (^cons (f m) (bl-helper (^add1 m)))])])
    (if (or (^not (^integer? n)) (^< n 0))
        ;; Hook this into contract system?
        (error "build-list: must be passed a non-negative integer")
        (bl-helper 0))))

(provide
 (lift-out member?
           member))

;;
;; testing
;;

(begin-for-syntax
  (define expect-forms null)

  (define (push-form! stx)
    (set! expect-forms (cons stx expect-forms))
    #'(void)))

(define-syntax $check-expect
  (syntax-parser
    [(_ actual expected)
     (push-form! #'(check-equal? actual expected))]))

(define-syntax $check-contract
  (syntax-parser
    [(_ val:expr (~optional n:number #:defaults ([n #'1])))
     (push-form! #'(check-contract (λ () (check-contract-function val n))))]))

(define-check (check-contract thk)
  (let ([result (with-handlers ([exn? exn-message]) (thk) #f)])
    (when result
      (fail-check result))))

(define-syntax $verify-contract
  (syntax-parser
    [(_ val:expr)
     (push-form! #'(verify-contract (λ () (verify-contract-function val))))]))

(define-check (verify-contract thk)
  (let ([result (with-handlers ([exn? exn-message]) (thk) #f)])
    (when result
      (fail-check result))))

(define-syntax $check-within
  (syntax-parser
    [(_ actual expected ϵ)
     (push-form! #'(check-within actual expected ϵ))]))

(define-syntax $check-member-of
  (syntax-parser
    [(_ actual expecteds ...)
     (push-form! #'(check-true ($member actual (list expecteds ...))))]))

(define-syntax $check-range
  (syntax-parser
    [(_ actual low high)
     (push-form! #'(check-true (<= low actual high)))]))

(define-syntax $check-satisfied
  (syntax-parser
    [(_ actual pred)
     (push-form! #'(check-pred pred actual))]))

(define-syntax $check-error
  (syntax-parser
    [(_ body:expr)
     (push-form! #'(check-exn always (λ () body)))]
    [(_ body:expr msg:expr)
     (push-form! #'(check-exn (matches? msg) (λ () body)))]))

(define-syntax $run-tests
  (syntax-parser
    [(_)
     #:with (form ...) expect-forms
     (if (empty? expect-forms)
         #'(void)
         #'(void
            (run-tests
             (test-suite
              "unit tests"
              (test-begin form) ...))))]))

(define (always _) #t)

(define ((matches? msg) e)
  (string-contains? (exn-message e) msg))

;;
;; standard library
;;

(define $empty ^null)

(define ($boolean=? b1 b2)
  (^equal? b1 b2))
