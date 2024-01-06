#lang rosette/safe

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
         "private/syntax/syntax.rkt")

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
        (symbolic v) ;; Questionable decision...
        (generate (λ (fuel) v))))

(define-contract True (Constant #t))

(define-contract Integer
  (Flat (check integer?)
        (symbolic (predicate->symbolic integer?))
        (generate (λ (fuel) (random -100 100)))
        (shrink (λ (val) (if (zero? val) 0 (floor (/ val 2)))))))

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

(define-contract-syntax ->
  (λ (stx)
    (syntax-parse stx
      [(_ d:expr ... c:expr)
       (syntax-property #'(Function (arguments [_ d] ...)
                                    (result c))
                        'original stx)])))

(define-contract-syntax Record
  (syntax-parser
    [(_ (~optional folder:expr) tr:id)
     #:do [(define ctc (free-id-table-ref contract-table #'tr #f))]
     #:fail-unless ctc
     (format "unknown contract for ~a" (syntax-e #'tr))
     (syntax-property
      (if (attribute folder)
          (quasisyntax/loc ctc
            (Flat
             (check
              (λ (val)
                (set! tr (folder tr val))
                ((contract-predicate #,ctc) tr)))))
          (quasisyntax/loc ctc
            (Flat
             (check
              (λ (val)
                (set! tr (append tr (list val)))
                ((contract-predicate #,ctc) tr))))))
      'original
      ctc)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; list

(define-contract (List N X)
  (Flat (check (list-check N X))
        (generate (list-generate N X))
        (shrink (list-shrink N X))))

(define ((list-check n x) l)
  (and (list? l)
       (implies maybe-n (= (length l) maybe-n))
       (andmap (flat-contract-struct-predicate ctc) l)))

(define ((list-generate n x) fuel)
  (define result
    (let ([n (or maybe-n (random 0 (* 10 fuel)))])
      (build-list n (λ (_) (contract-generate-function ctc fuel)))))
  (if (ormap contract-generate-failure? result)
      (contract-generate-failure)
      result))

(define ((list-shrink n x) val)
  (cond
    [maybe-n (list-shrink-length val)]
    [(empty? val) (list-shrink-elems val)]
    [else (if (< (random) 1/2)
              (list-shrink-length val)
              (list-shrink-elems val))]))

(define (list-shrink-length val)
  (define k (random (length val)))
  (append (take val k) (drop val (add1 k))))

(define (list-shrink-elems val)
  (map (curry contract-shrink-function ctc) val))
