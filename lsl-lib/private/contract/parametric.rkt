#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (only-in racket/base
                  for/list
                  in-list
                  write-string)
         racket/class
         racket/string
         racket/match
         "common.rkt"
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out seal-info)
         parametric-contract%
         seal-contract%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define seal-number -1)
(struct seal-info (ctor pred? get name polarity))

;; TODO: A bit odd to have seal-name be constant and not a list.
(define parametric-contract%
  (class contract%
    (init-field syntax polarity names make-body [seal-name #f])

    (super-new)

    (define/override (protect val pos)
      (define infos
        (for/list ([base-name (in-list names)])
          (set! seal-number (add1 seal-number))
          (define name
            (or seal-name (make-name base-name polarity seal-number)))
          (define actual-polarity
            (if (positive-blame? pos)
                polarity
                (not polarity)))
          (struct seal base-seal (val)
            #:methods gen:custom-write
            [(define write-proc (make-seal-writer name))])
          (seal-info seal seal? seal-val name actual-polarity)))
      (define ctc (apply make-body infos))
      (send ctc protect val pos))))

(define (make-name base-name polarity seal-number)
  (format "~a~a~a"
          (if polarity "∀" "∃")
          base-name
          (integer->subscript seal-number)))

(define ((make-seal-writer name) self port mode)
  (write-string name port))

(define seal-contract%
  (class contract%
    (init-field syntax info)

    (super-new)

    (define/override (protect val pos)
      (match-define (seal-info ctor pred? get name polarity) info)
      (define wrap?
        (or (and polarity (negative-blame? pos))
            (and (not polarity) (positive-blame? pos))))
      (cond
        [wrap? (passed-guard (λ (val neg) (ctor val)))]
        [(pred? val) (passed-guard (λ (val neg) (get val)))]
        [else
         (failed-guard
          (λ (val neg)
            (contract-error this syntax val pos
                            #:expected name)))]))

    (define/override (generate fuel)
      (match-define (seal-info ctor pred? get name polarity) info)
      (if polarity
          (ctor 0)
          (give-up syntax)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subscripts

(define SUBSCRIPTS '("₀" "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉"))

(define (integer->subscript i)
  (define subs
    (for/list ([digit (in-list (integer->digit* i))])
      (list-ref SUBSCRIPTS digit)))
  (string-join subs ""))

(define (integer->digit* i)
  (let loop ([acc '()]
             [prev 0]
             [m 1])
    (define m+ (* m 10))
    (define n (quotient (- (modulo i m+) prev) m))
    (define acc+ (cons n acc))
    (if (< i m+)
      acc+
      (loop acc+ (+ prev n) m+))))
