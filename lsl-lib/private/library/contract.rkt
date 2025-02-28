#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         racket/random
         racket/list
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
         Maybe
         Integer
         Real
         Natural
         String
         Symbol
         NonemptyList
         BoundedList
         Record
         ->)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic

(define-contract Any
  (Immediate
   (check (λ _ #t))
   (generate
    (λ (fuel)
      ((random-ref
        (list (λ () (contract-generate Boolean fuel))
              (λ () (contract-generate Integer fuel))
              (λ () (contract-generate Real fuel))
              (λ () (contract-generate Natural fuel))
              (λ () (contract-generate String fuel))
              (λ () (contract-generate Symbol fuel)))))))))

(define-contract Boolean
  (Immediate (check boolean?)
             (generate (λ (fuel) (< (random) 1/2)))))

(define-contract (Constant v)
  (Immediate (check (λ (x) (equal? x v)))
             (generate (λ (fuel) v))))

(define-contract True (Constant #t))
(define-contract False (Constant #f))

(define-contract (Maybe T) (OneOf False T))

(define-contract Integer
  (Immediate (check integer?)
             (generate (λ (fuel) (if (zero? fuel) 0 (random (* -1 fuel) fuel))))
             (shrink (λ (fuel val)
                       (if (zero? val) 0 (floor (/ val 2)))))))

(define-contract Real
  (Immediate (check real?)
             (generate (λ (fuel) (- (* 2 fuel (random)) fuel)))))

(define (natural? n)
  (and (integer? n)
       (or (positive? n) (zero? n))))

(define-contract Natural
  (Immediate (check natural?)
             (generate (λ (fuel) (random 0 (add1 fuel))))))

(define (random-alpha-char)
  (integer->char (random 33 127)))

(define-contract String
  (Immediate (check string?)
             (generate (λ (fuel) (random-string fuel)))))

(define (random-string fuel)
  (build-string (random 0 (add1 fuel)) (λ (_) (random-alpha-char))))

(define-contract Symbol
  (Immediate (check symbol?)
             (generate (λ (fuel) (string->symbol (random-string fuel))))))

(define-contract (NonemptyList X)
  (AllOf (List X) cons?))

(define-syntax BoundedList
  (contract-macro
   (syntax-parser
     [(_ n-stx:integer t)
      #:do [(define n (syntax-e #'n-stx))]
      #:fail-when (< n 0)
      (format "~a must be non-negative" n)
      #:with (tup ...)
      (for/list ([k (in-range (add1 n))])
        #`(Tuple #,@(map (λ _ #'t) (range k))))
      (quasisyntax/loc this-syntax
        (OneOf tup ...))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; arrow

(define-syntax ->
  (contract-macro
   (syntax-parser
     [(_ d:expr ... c:expr)
      #'(Function (arguments [_ d] ...)
                  (result c))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; record

(define-syntax Record
  (contract-macro
   (syntax-parser
     [(_ (~optional folder:expr) tr:id)
      #:do [(define ctc (contract-table-ref #'tr))]
      #:fail-unless ctc
      (format "unknown contract for ~a" (syntax-e #'tr))
      (quasisyntax/loc ctc
        (Immediate
         (check
          (let ([c #,(compile-contract (expand-contract ctc))]
                [f (~? folder default-folder)])
            (λ (val)
              (define ct (current-traces))
              (unless (hash-has-key? ct 'tr)
                (define val* tr)
                (current-traces (hash-set ct 'tr (λ () (set! tr val*)))))
              (define tr* (f tr val))
              (set! tr ((send c protect tr* #f) tr* #f)))))))])))

(define (default-folder tr val)
  (append tr (list val)))
