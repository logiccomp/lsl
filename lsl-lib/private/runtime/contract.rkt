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
         name-contract
         check-contract
         verify-contract
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
         errortrace/errortrace-key
         struct-set)

;;
;; data
;;

(struct blame-struct (name path))
(struct positive-blame-struct blame-struct ())
(struct negative-blame-struct blame-struct ())
(struct/set contract-struct (name syntax protect generate symbolic interact))
(struct/set flat-contract-struct contract-struct (predicate))

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

(define (name-contract name-val ctc)
  (struct-set contract-struct ctc [name name-val]))

(define (contract-generate-function ctc)
  (define generate (contract-struct-generate ctc))
  (if generate
      (generate)
      (generate-error (contract-struct-name ctc))))

(define (check-contract val [n 1])
  (define ctc (value->contract val))
  (define mode contract-generate-function)
  (when ctc
    (for ([_ (in-range n)])
      ((contract-struct-interact ctc) mode val))))

(define (contract-symbolic-function ctc)
  (define generate (contract-struct-symbolic ctc))
  (define msg (format "symbolic ~a" (contract-struct-name ctc)))
  (if generate (generate) (generate-error msg)))

(define (verify-contract val)
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

(define (contract-error self stx blm val)
  (define error-msg
    (format CTC-FMT
            (blame-struct-name blm)
            (contract-struct-name self)
            val
            (blame-struct-path blm)))
  (custom-error self stx error-msg))

(define (verify-error self blm stx val)
  (define error-msg
    (format VERIFY-FMT
            (contract-struct-name self)
            val
            (blame-struct-path blm)))
  (custom-error self stx error-msg))

(define (custom-error self pre-stx msg)
  (define stx
    (or (syntax-property pre-stx 'original) pre-stx))
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

(define ((generate-error name))
  (raise-user-error (format GEN-FMT name)))

(define (value->contract val)
  (and (has-impersonator-prop:contract? val)
       (get-impersonator-prop:contract val)))
