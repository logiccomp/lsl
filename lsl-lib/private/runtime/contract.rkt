#lang racket/base

;;
;; provide
;;

(provide (struct-out blame-struct)
         (struct-out positive-blame-struct)
         (struct-out negative-blame-struct)
         (struct-out flat-contract-struct)
         (struct-out contract-struct)
         (struct-out contract-generate-failure)
         current-verify-name
         current-verify-arguments
         contract-generate-function
         contract-symbolic-function
         check-contract-function
         verify-contract-function
         contract-error
         custom-error
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
(struct contract-struct (syntax protect generate symbolic interact))
(struct flat-contract-struct contract-struct (predicate))
(struct contract-generate-failure ())

(struct exn:fail:contract exn:fail (srclocs)
  #:property prop:exn:srclocs
  (λ (self) (exn:fail:contract-srclocs self)))

(define-values (impersonator-prop:contract
                has-impersonator-prop:contract?
                get-impersonator-prop:contract)
  (make-impersonator-property 'contract))

(define current-verify-arguments
  (make-parameter #f))

(define current-verify-name
  (make-parameter #f))

;;
;; functions
;;

(define (contract-generate-function ctc [fuel 5])
  (define gen (contract-struct-generate ctc))
  (if (or (zero? fuel) (not gen))
      (contract-generate-failure)
      (gen fuel)))

(define (check-contract-function val n)
  (define ctc (value->contract val))
  (define mode contract-generate-function)
  (when ctc
    (for ([_ (in-range n)])
      ((contract-struct-interact ctc) mode val))))

(define (contract-symbolic-function ctc)
  (define generate (contract-struct-symbolic ctc))
  (define stx (original-syntax (contract-struct-syntax ctc)))
  (define msg (format "symbolic ~a" (syntax->datum stx)))
  (if generate (generate) (generate-error msg)))

(define (verify-contract-function val)
  (define ctc (value->contract val))
  (define mode contract-symbolic-function)
  (when ctc ((contract-struct-interact ctc) mode val)))

(define VERIFY-FMT
  (string-join
   '("verification failure"
     "expected: ~a"
     "counterexample: ~a"
     "blaming: ~a")
   "\n  "))

(define CTC-FMT
  (string-join
   '("~a: contract violation"
     "expected: ~a"
     "given: ~v"
     "blaming: ~a")
   "\n  "))

(define (original-syntax stx)
  (or (syntax-property stx 'original) stx))

(define (contract-error self pre-stx blm val)
  (define stx (original-syntax pre-stx))
  (define error-msg
    (format CTC-FMT
            (blame-struct-name blm)
            (syntax->datum stx)
            val
            (blame-struct-path blm)))
  (custom-error self stx error-msg))

(define (verify-error self blm pre-stx args)
  (define stx (original-syntax pre-stx))
  (define error-msg
    (format VERIFY-FMT
            (syntax->datum stx)
            (cons (current-verify-name) args)
            (blame-struct-path blm)))
  (custom-error self stx error-msg))

(define (custom-error self stx msg)
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
  (raise (exn:fail:contract msg cms srclocs)))

(define GEN-FMT
  "cannot generate ~a")

(define (generate-error stx)
  (raise-user-error (format GEN-FMT (syntax->datum stx))))

(define (value->contract val)
  (and (has-impersonator-prop:contract? val)
       (get-impersonator-prop:contract val)))
