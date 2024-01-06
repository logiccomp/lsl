#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require #;(only-in rosette/safe
                  concrete?
                  symbolic?
                  assert
                  verify
                  evaluate)
         racket/class
         racket/match
         racket/string
         racket/syntax-srcloc
         errortrace/errortrace-key
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out exn:fail:user)
         (struct-out exn:fail:contract)
         (struct-out blame)
         (struct-out positive-blame)
         (struct-out negative-blame)
         contract%
         contract-error
         generate-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exceptions

(struct exn:fail:user exn:fail (value))

(struct exn:fail:contract exn:fail (srclocs)
  #:property prop:exn:srclocs
  (λ (self) (exn:fail:contract-srclocs self)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; blame

(struct blame (name path))
(struct positive-blame blame ())
(struct negative-blame blame ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contract%

(define contract%
  (class object%
    (super-new)

    (define/public (protect val pos)
      (unimplemented-error 'protect))

    (define/public (generate fuel)
      (none))

    (define/public (shrink fuel val)
      (none))

    (define/public (interact mode val)
      (unimplemented-error 'interact))

    (define/public (symbolic)
      (unimplemented-error 'symbolic))))

(define (unimplemented-error method-name)
  (error 'contract "TODO"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors

(define (contract-error ctc stx val blm)
  (define datum (syntax->datum (syntax-property stx 'unexpanded)))
  (define error-msg
    (match blm
      [(blame name path)
       (define polarity (blame->polarity blm))
       (format BLM-CTC-FMT name datum val path polarity)]
      [_ (format UNK-CTC-FMT datum val)]))
  (custom-error stx error-msg))

(define (blame->polarity blm)
  (if (positive-blame? blm) "server" "client"))

(define BLM-CTC-FMT
  (string-join
   '("~a: contract violation"
     "expected: ~a"
     "given: ~v"
     "blaming: ~a (as ~a)")
   "\n  "))

(define UNK-CTC-FMT
  (string-join
   '("contract violation"
     "expected: ~a"
     "given: ~v")
   "\n  "))

(define (generate-error stx)
  (define datum (syntax->datum stx))
  (custom-error (format GEN-FMT datum)))

(define GEN-FMT
  "cannot generate ~a")

(define (custom-error stx msg)
  (define cms (current-continuation-marks))
  (define stx-srclocs
    (cond
      [(and stx (syntax-srcloc stx)) => list]
      [else null]))
  (define cm-srclocs
    (match (continuation-mark-set->list cms errortrace-key)
      [(cons (cons datum srcloc-list) _) (list (apply srcloc srcloc-list))]
      [_ null]))
  (raise (exn:fail:contract msg cms (append stx-srclocs cm-srclocs))))
