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
         (only-in racket/random
                  random-ref)
         racket/class
         "../syntax/expand.rkt"
         "../syntax/compile.rkt"
         "../syntax/grammar.rkt"
         "../syntax/interface.rkt"
         "../util.rkt"
         "concurrency.rkt")

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
         SendPacket
         ReceivePacket
         ->)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; atomic

(define ((predicate->symbolic predicate))
  (define-symbolic* x predicate) x)

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
             (generate (λ (fuel) (< (random) 1/2)))
             (symbolic (predicate->symbolic boolean?))))

(define-contract (Constant v)
  (Immediate (check (λ (x) (equal? x v)))
             (generate (λ (fuel) v))
             (symbolic (λ _ v))))

(define-contract True (Constant #t))
(define-contract False (Constant #f))

(define-contract (Maybe T) (OneOf False T))

(define-contract Integer
  (Immediate (check integer?)
             (generate (λ (fuel) (if (zero? fuel)
                                     0
                                     (random (* -1 fuel) fuel))))
             (shrink (λ (fuel val)
                       (if (zero? val)
                           (none)
                           (floor (/ val 2)))))
             (symbolic (predicate->symbolic integer?))))

(define-contract Real
  (Immediate (check real?)
             (generate (λ (fuel) (- (* 2 fuel (random)) fuel)))
             (symbolic (predicate->symbolic real?))))

(define (natural? n)
  (and (integer? n)
       (or (positive? n) (zero? n))))

(define-contract Natural
  (Immediate (check natural?)
             (generate (λ (fuel) (random 0 (add1 fuel))))
             (symbolic (λ ()
                         (define v (contract-symbolic Integer))
                         (if (positive? v) v (- v))))))

(define (random-alpha-char)
  (integer->char (random 33 127)))

(define-contract String
  (Immediate (check lifted-string?)
             (generate (λ (fuel) (random-string fuel)))))

(define (lifted-string? x)
  (for/all ([x x])
    (string? x)))

(define (random-string fuel)
  (build-string (random 0 (add1 fuel)) (λ (_) (random-alpha-char))))

(define-contract Symbol
  (Immediate (check lifted-symbol?)
             (generate (λ (fuel) (string->symbol (random-string fuel))))))

(define (lifted-symbol? x)
  (for/all ([x x])
    (symbol? x)))

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
              (define tr* (f tr val))
              (set! tr ((send c protect tr* #f) tr* #f)))))))])))

(define (default-folder tr val)
  (append tr (list val)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; packets

(define-contract SendPacket
  (Immediate (check send-packet?)
             (generate (λ (fuel) (send-packet (contract-generate Natural fuel) (contract-generate Any fuel))))))

(define-contract ReceivePacket
  (Immediate (check receive-packet?)
             (generate (λ (fuel) (receive-packet (contract-generate Natural fuel) (contract-generate Any fuel))))))
