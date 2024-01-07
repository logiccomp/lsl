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
         "../syntax/expand.rkt"
         "../syntax/grammar.rkt"
         "../syntax/interface.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide Any
         Boolean
         Constant
         True
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
        (symbolic (predicate->symbolic boolean?))
        (generate (λ (fuel) (< (random) 1/2)))))

(define-contract (Constant v)
  (Flat (check (λ (x) (equal? x v)))
        (symbolic v)
        (generate (λ (fuel) v))))

(define-contract True (Constant #t))

(define-contract Integer
  (Flat (check integer?)
        (symbolic (predicate->symbolic integer?))
        (generate (λ (fuel) (random -100 100)))
        (shrink (λ (fuel val) (if (zero? val) 0 (floor (/ val 2)))))))

(define-contract Real
  (Flat (check real?)
        (symbolic (predicate->symbolic real?))
        (generate (λ (fuel) (- (* 200 (random)) 100)))))

(define (natural? n)
  (and (integer? n) (or (positive? n) (zero? n))))

(define-contract Natural
  (Flat (check natural?)
        (symbolic (predicate->symbolic natural?))
        (generate (λ (fuel) (random 0 200)))))

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
     #:do [(define ctc (free-id-table-ref contract-table #'tr #f))]
     #:fail-unless ctc
     (format "unknown contract for ~a" (syntax-e #'tr))
     (if (attribute folder)
         (quasisyntax/loc ctc
           (Flat
            (check
             (λ (val)
               (set! tr (folder tr val))
               ((contract->predicate #,ctc) tr)))))
         (quasisyntax/loc ctc
           (Flat
            (check
             (λ (val)
               (set! tr (append tr (list val)))
               ((contract->predicate #,ctc) tr))))))]))
