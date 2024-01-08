#lang rosette/safe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse
                     syntax/id-table)
         (only-in racket/base
                  integer->char
                  random
                  build-string
                  string?
                  symbol?
                  string->symbol
                  build-list)
         (only-in racket/stream
                  stream
                  empty-stream)
         racket/class
         "../syntax/expand.rkt"
         "../syntax/compile.rkt"
         "../syntax/grammar.rkt"
         "../syntax/interface.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide Any
         Boolean
         Constant
         True
         False
         Integer
         Real
         Natural
         String
         Symbol
         Record
         ->)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic

(define ((predicate->symbolic predicate))
  (define-symbolic* x predicate) x)

(define-contract Any
  (Flat (check (λ _ #t))))

(define-contract Boolean
  (Flat (check boolean?)
        (generate (λ (fuel) (< (random) 1/2)))
        (symbolic (predicate->symbolic boolean?))))

(define-contract (Constant v)
  (Flat (check (λ (x) (equal? x v)))
        (generate (λ (fuel) v))
        (symbolic (λ _ v))))

(define-contract True (Constant #t))
(define-contract False (Constant #f))

(define-contract Integer
  (Flat (check integer?)
        (generate (λ (fuel) (random -100 100)))
        (shrink (λ (fuel val)
                  (if (zero? val)
                      (none)
                      (floor (/ val 2)))))
        (symbolic (predicate->symbolic integer?))))

(define-contract Real
  (Flat (check real?)
        (generate (λ (fuel) (- (* 200 (random)) 100)))
        (symbolic (predicate->symbolic real?))))

(define (natural? n)
  (and (integer? n)
       (or (positive? n) (zero? n))))

(define-contract Natural
  (Flat (check natural?)
        (generate (λ (fuel) (random 0 200)))
        (symbolic (λ ()
                    (define v (contract-symbolic Integer))
                    (if (positive? v) v (- v))))))

(define (random-alpha-char)
  (integer->char (random 33 127)))

(define-contract String
  (Flat (check lifted-string?)
        (generate (λ (fuel) (random-string)))))

(define (lifted-string? x)
  (for/all ([x x])
    (string? x)))

(define (random-string)
  (build-string (random 0 100) (λ (_) (random-alpha-char))))

(define-contract Symbol
  (Flat (check lifted-symbol?)
        (generate (λ (fuel) (string->symbol (random-string))))))

(define (lifted-symbol? x)
  (for/all ([x x])
    (symbol? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrow

(define-contract-syntax ->
  (λ (stx)
    (syntax-parse stx
      [(_ d:expr ... c:expr)
       #'(Function (arguments [_ d] ...)
                   (result c))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record

(define-contract-syntax Record
  (syntax-parser
    [(_ (~optional folder:expr) tr:id)
     #:do [(define ctc (contract-table-ref #'tr))]
     #:fail-unless ctc
     (format "unknown contract for ~a" (syntax-e #'tr))
     (quasisyntax/loc ctc
       (Flat
        (check
         (let ([c (compile-contract (expand-contract #,ctc))]
               [f (~? folder default-folder)])
           (λ (val)
             (define tr* (f tr val))
             (set! tr ((send c protect tr* #f) tr* #f)))))))]))

(define (default-folder tr val)
  (append tr (list val)))
