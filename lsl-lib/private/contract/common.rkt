#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require racket/class
         racket/format
         racket/match
         racket/string
         racket/syntax-srcloc
         errortrace/errortrace-key
         (only-in rosette/safe
                  vc)
         "../guard.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out exn:root)
         (struct-out exn:user)
         (struct-out exn:contract)
         (struct-out blame)
         (struct-out positive-blame)
         (struct-out negative-blame)
         contract%
         contract->predicate
         contract-error
         generate-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exceptions

(struct exn:root exn (vc))
(struct exn:user exn:root (value))
(struct exn:contract exn:root (srclocs)
  #:property prop:exn:srclocs
  (λ (self) (exn:contract-srclocs self)))

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

    (define/public (interact val name mode)
      (unimplemented-error 'interact))

    (define/public (symbolic)
      (unimplemented-error 'symbolic))))

(define (unimplemented-error method-name)
  (error 'contract "TODO"))

(define ((contract->predicate ctc) val)
  (passed-guard? (send ctc protect val #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; errors

(define (contract-error ctc stx val blm
                        #:expected [expected #f]
                        #:given [given #f])
  (define expected-datum
    (or expected (syntax->datum (syntax-property stx 'unexpanded))))
  (define given-str
    (or given (~v val)))
  (define error-msg
    (match blm
      [(blame name path)
       (define polarity (blame->polarity blm))
       (format BLM-CTC-FMT name expected-datum given-str path polarity)]
      [_ (format UNK-CTC-FMT expected-datum given-str)]))
  (custom-error stx error-msg))

(define (blame->polarity blm)
  (if (positive-blame? blm) "server" "client"))

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
  (raise (exn:contract msg cms (vc) (append stx-srclocs cm-srclocs))))

(define BLM-CTC-FMT
  (string-join
   '("~a: contract violation"
     "expected: ~a"
     "given: ~a"
     "blaming: ~a (as ~a)")
   "\n  "))

(define UNK-CTC-FMT
  (string-join
   '("contract violation"
     "expected: ~a"
     "given: ~a")
   "\n  "))

(define (generate-error stx)
  (error 'check-contract GEN-FMT (syntax->datum stx)))

(define GEN-FMT
  "cannot generate ~a")
