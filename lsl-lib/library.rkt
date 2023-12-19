#lang rosette/safe

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide Boolean
         Constant
         True
         Integer
         Real
         Natural
         String
         List
         Record
         ->)

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
                  build-list)
         "private/syntax/syntax.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic

(define ((predicate->symbolic predicate))
  (define-symbolic* x predicate) x)

(define-contract Boolean
  (Flat (check boolean?)
        (symbolic (predicate->symbolic boolean?))
        (generate (λ () (< (random) 1/2)))))

(define-contract (Constant v)
  (Flat (check (λ (x) (equal? x v)))
        (symbolic v) ;; Questionable decision...
        (generate (λ () v))))

(define-contract True (Constant #t))

(define-contract Integer
  (Flat (check integer?)
        (symbolic (predicate->symbolic integer?))
        (generate (λ () (random -100 100)))))

(define-contract Real
  (Flat (check real?)
        (symbolic (predicate->symbolic real?))
        (generate (λ () (- (* 200 (random)) 100)))))

(define (natural? n)
  (and (integer? n) (or (positive? n) (zero? n))))

(define-contract Natural
  (Flat (check natural?)
        (symbolic (predicate->symbolic natural?))
        (generate (λ () (random 0 200)))))

(define (random-alpha-char)
  (integer->char (random 33 127)))

(define-contract String
  (Flat (check lifted-string?)
        (generate (λ () (random-string)))))

(define (lifted-string? x)
  (for/all ([x x])
    (string? x)))

(define (random-string)
  (build-string (random 0 100) (λ (_) (random-alpha-char))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-contract (List X)
  (Flat (check (λ (l) (and (list? l)
                           (andmap (contract-predicate X) l))))
        (generate (λ () (let ([n (random 0 100)])
                          (build-list n (λ (_) (contract-generate X))))))))

(define-contract-syntax ->
  (λ (stx)
    (syntax-parse stx
      [(_ d:expr ... c:expr)
       (syntax-property #'(Function [_ d] ... c) 'original stx)])))

(define-contract-syntax Record
  (syntax-parser
    [(_ tr:id)
     #:do [(define ctc (free-id-table-ref contract-table #'tr #f))]
     #:fail-unless ctc
     (format "unknown contract for ~a" (syntax-e #'tr))
     #`(Flat
        (check
         (λ (val)
           (set! tr (append tr (list val)))
           ((contract-predicate #,ctc) tr))))]))
