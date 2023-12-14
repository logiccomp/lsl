#lang racket/base

;;
;; provide
;;

(require (for-syntax racket/base
                     racket/string
                     racket/list)
         racket/provide
         (prefix-in @ racket/contract)
         (except-in rackunit check)
         rackunit/text-ui)

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
                       $if
                       $and
                       $or
                       $quote
                       $#%datum
                       $...
                       ;$explode

                       $check-expect
                       $check-within
                       $check-member-of
                       $check-range
                       $check-satisfied
                       $check-error

                       $empty
                       $#%module-begin
                       #;$#%top))
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
                       ^sqr
                       ^sqrt
                       ^expt

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
          Struct

          domain
          check
          generate
          symbolic

          Integer
          Boolean
          True
          Real
          Natural
          List
          ->

          (rename-out [annotate :])
          define-contract
          contract-generate
          contract-predicate
          check-contract
          verify-contract

          (@contract-out (rename $boolean=? boolean=? (@-> boolean? boolean? boolean?)))

          build-list

          (@contract-out (rename equal? string=? (@-> string? string? boolean?)))
          ;explode
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
          string-length
          ;string-lower-case?
          ;string-numeric?
          string-ref
          string-upcase
          ;string-upper-case?
          ;string-whitespace?
          string?
          substring)

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
     #'(begin
         (define-contract-syntax Name (struct-contract-macro #'name))
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

(define-contract Boolean
  (Flat (check ^boolean?)
        (symbolic (predicate->symbolic ^boolean?))
        (generate (λ () (< (random) 1/2)))))

(define-contract True
  (Flat (check (λ (x) (equal? x #t)))
        (symbolic #t)
        (generate (λ () #t))))

(define-contract Integer
  (Flat (check ^integer?)
        (symbolic (predicate->symbolic ^integer?))
        (generate (λ () (random -100 100)))))

(define-contract Real
  (Flat (check ^real?)
        (symbolic (predicate->symbolic ^real?))
        (generate (λ () (- (* 200 (random)) 100)))))

(define (natural? n)
  (and (integer? n) (or (positive? n) (zero? n))))

(define-contract Natural
  (Flat (check natural?)
        (symbolic (predicate->symbolic natural?))
        (generate (λ () (random 0 200)))))


(define-contract (List X)
  (Flat (check (λ (l) (and (list? l)
                           (andmap (contract-predicate X) l))))
        (generate (λ () (let ([n (random 0 100)])
                          (build-list n (λ (_) (contract-generate X))))))))

(define-contract-syntax ->
  (syntax-parser
    [(_ d:expr ... c:expr)
     #'(Function [_ d] ... c)]))

;;
;; math
;;

(define (^sqr x)
  (^* x x))

(define (^sqrt x)
  (^for/all ([x x #:exhaustive])
    (sqrt x)))

(define (^expt z w)
  (^for*/all ([z z #:exhaustive] [w w #:exhaustive])
    (expt z w)))

;; strings

#;(define ($explode s)
  (^for/all ([s s #:exhaustive])
    (drop-right (drop (string-split s "") 1) 1)))


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

(define-syntax $check-within
  (syntax-parser
    [(_ actual expected ϵ)
     (push-form! #'(check-within actual expected ϵ))]))

(define-syntax $check-member-of
  (syntax-parser
    [(_ actual expecteds ...)
     (push-form! #'(check-true (member actual (list expecteds ...))))]))

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

;;
;; reader
;;

(module reader syntax/module-reader
  #:language 'lsl
  #:info
  (λ (key default use-default)
    (case key
      [(drracket:opt-out-toolbar-buttons) #f]
      [else (use-default key default)])))
