#lang racket/base

;;
;; provide
;;

(provide (struct-out ctc)
         (struct-out blm)
         (struct-out positive-blm)
         (struct-out negative-blm)
         flat
         arrow)

;;
;; require
;;

(require racket/stream
         racket/string)

;;
;; data
;;

(struct ctc (protect interact concrete))
(struct blm (path))
(struct positive-blm blm ())
(struct negative-blm blm ())

(define (assert x [msg #f])
  (unless x
    (if msg (raise-user-error msg) (error 'assert))))

(define CTC-FMT
  (string-join
   '("contract violation"
     "expected: ~a"
     "given: ~v"
     "blaming: ~a")
   "\n  "))

(define (make-contract #:protect [protect #f]
                       #:interact [interact #f]
                       #:concrete [concrete #f])
  (ctc protect interact concrete))

(define-values (impersonator-prop:contract
                has-impersonator-prop:contract?
                get-impersonator-prop:contract)
  (make-impersonator-property 'contract))

(define ((random-sampler n) ctc)
  (stream-map (λ (k) ((ctc-concrete ctc)))
              (in-range n)))

(define (flat predicate [concrete #f])
  (define (protect val pos)
    (define error-msg
      (format CTC-FMT
              (object-name predicate)
              val
              (blm-path pos)))
    (assert (predicate val) error-msg)
    (λ (neg) val))
  (define (interact val sampler)
    (void))
  (make-contract
   #:protect protect
   #:interact interact
   #:concrete concrete))

(define (arrow doms cods)
  (define (protect val pos)
    (assert (procedure? val))
    (assert (procedure-arity-includes? val (length doms)))
    (λ (neg)
      (define (wrapper . args)
        (define args*
          (for/list ([dom (in-list doms)]
                     [arg (in-list args)])
            (define proj (ctc-protect (apply dom args)))
            ((proj arg pos) neg)))
        (define (results-wrapper . results)
          (define args+results (append args results))
          (define results*
            (for/list ([cod (in-list cods)]
                       [res (in-list results)])
              (define proj (ctc-protect (apply cod args+results)))
              ((proj res pos) neg)))
          (apply values results*))
        (apply values (cons results-wrapper args*)))
      (chaperone-procedure
       val wrapper
       impersonator-prop:contract self)))
  (define (interact val sampler)
    (for ([inputs (map sampler doms)])
      (apply val inputs)))
  (define ((concrete) . _)
    (define results
      (for/list ([cod (in-list cods)])
        ((ctc-concrete cod))))
    (apply values results))
  (define self
    (make-contract
     #:protect protect
     #:interact interact
     #:concrete concrete))
  self)

;;
;; contract functions
;;

(define (value->contract val)
  (and (has-impersonator-prop:contract? val)
       (get-impersonator-prop:contract val)))

(define (contract-exercise val sampler)
  (define ctc (value->contract val))
  (when ctc
    ((ctc-interact ctc) val sampler)))
