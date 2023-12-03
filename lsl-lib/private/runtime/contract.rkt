#lang racket/base

;;
;; provide
;;

(provide (struct-out blame-struct)
         (struct-out positive-blame-struct)
         (struct-out negative-blame-struct)
         (struct-out flat-contract-struct)
         (struct-out contract-struct)
         contract-generate-function
         contract-exercise
         contract-verify
         contract-error
         verify-error
         generate-error
         value->contract
         impersonator-prop:contract
         has-impersonator-prop:contract?
         get-impersonator-prop:contract)

;;
;; require
;;

(require racket/string
         racket/list
         racket/match
         racket/syntax-srcloc
         errortrace/errortrace-key)

;;
;; data
;;

(struct blame-struct (name path))
(struct positive-blame-struct blame-struct ())
(struct negative-blame-struct blame-struct ())
(struct contract-struct (name protect generate symbolic interact))
(struct flat-contract-struct contract-struct (predicate))

(struct exn:fail:contract exn:fail (srclocs)
  #:property prop:exn:srclocs
  (λ (self) (exn:fail:contract-srclocs self)))

(define-values (impersonator-prop:contract
                has-impersonator-prop:contract?
                get-impersonator-prop:contract)
  (make-impersonator-property 'contract))

;;
;; functions
;;

(define (contract-generate-function ctc)
  (define generate (contract-struct-generate ctc))
  (if generate
      (generate)
      (generate-error (contract-struct-name ctc))))

(define (contract-exercise val [n 1])
  (define ctc (value->contract val))
  (define mode contract-generate-function)
  (when ctc
    (for ([_ (in-range n)])
      ((contract-struct-interact ctc) mode val))))

(define (contract-symbolic-function ctc)
  (define generate (contract-struct-symbolic ctc))
  (define msg (format "symbolic ~a" (contract-struct-name ctc)))
  (if generate (generate) (generate-error msg)))

(define (contract-verify val)
  (define ctc (value->contract val))
  (define mode contract-symbolic-function)
  (when ctc ((contract-struct-interact ctc) mode val)))

(define VERIFY-FMT
  (string-join
   '("verification failure"
     "expected: ~a"
     "counterexample: ~v"
     "blaming: ~a")
   "\n  "))

(define CTC-FMT
  (string-join
   '("~a: contract violation"
     "expected: ~a"
     "given: ~v"
     "blaming: ~a")
   "\n  "))

(define (syntax->name stx)
  (cond
    [(syntax-property stx 'origin) => (compose syntax->datum last)]
    [else "anonymous contract"]))

(define (contract-error blm stx val)
  (define error-msg
    (format CTC-FMT
            (blame-struct-name blm)
            (syntax->name stx)
            val
            (blame-struct-path blm)))
  (define stx-srclocs
    (cond
      [(and stx (syntax-srcloc stx)) => list]
      [else null]))
  (define cms (current-continuation-marks))
  (define cm-srclocs
    (match (continuation-mark-set->list cms errortrace-key)
      [(cons (cons datum srcloc-list) _) (list (apply srcloc srcloc-list))]
      [_ null]))
  (define srclocs (append stx-srclocs cm-srclocs))
  (raise (exn:fail:contract error-msg cms srclocs)))

(define (verify-error blm stx val)
  (define error-msg
    (format VERIFY-FMT (syntax->name stx) val (blame-struct-path blm)))
  (raise-user-error error-msg))

(define GEN-FMT
  "cannot generate ~a")

(define ((generate-error name))
  (raise-user-error (format GEN-FMT name)))

(define (value->contract val)
  (and (has-impersonator-prop:contract? val)
       (get-impersonator-prop:contract val)))
