#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/id-table
                     syntax/parse
                     syntax/parse/class/struct-id)
         racket/stxparam
         racket/class
         (prefix-in ^ rosette/safe)
         (except-in racket/contract blame?)
         racket/match
         racket/provide
         racket/string
         rackunit
         rackunit/text-ui
         "equal.rkt"
         "../syntax/expand.rkt"
         "../syntax/compile.rkt"
         "../contract/common.rkt"
         "../proxy.rkt"
         "../util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (filtered-out
  (strip "$")
  (combine-out
   $define-run-tests
   $with-tests
   $test-suite

   $check-error
   $check-expect
   $check-member-of
   $check-random
   $check-range
   $check-satisfied
   $check-within
   $check-raises

   $contracted?
   $check-contract
   $verify-contract)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test environments

(define-syntax-parameter dont-push? #f)

(begin-for-syntax
  (define anon-tests null)
  (define test-suites null)

  (define (push-form! raw-stx [suite-name #f])
    (define stx #`(with-vc-reset #,raw-stx))
    (match (syntax-local-context)
      [(or 'top-level (? (λ _ (syntax-parameter-value #'dont-push?))))
       stx]
      [(or 'module 'module-begin)
       (cond
         [(not suite-name)
          (set! anon-tests (cons stx anon-tests))
          #'(void)]
         [(assoc suite-name test-suites)
          (raise-syntax-error 'check (format "test suite ~a already exists" suite-name) stx)]
         [else
          (set! test-suites (cons (cons suite-name stx) test-suites))
          #'(void)])]
      [(or 'expression (? list?))
       (raise-syntax-error 'check "a test cannot be inside a definition or expression" raw-stx)])))

(define-syntax $with-tests
  (syntax-parser
    [(_ e:expr ...)
     #'(syntax-parameterize ([dont-push? #t])
         (let ([result (void)])
           (run-tests
            (test-suite
             "interaction-area tests"
             (set! result (let () e ...))))
           result))]))

(define-syntax $define-run-tests
  (syntax-parser
    [(_ external:id internal:id)
     #:with (anon-test ...) (reverse anon-tests)
     #:with ([name . suite] ...) (reverse test-suites)
     #'(begin
         (define (run-anon-tests)
           (run-test (test-suite "anonymous tests" anon-test ...)))
         (define external
           (syntax-parameterize ([dont-push? #t])
             (hash (~@ (#%datum . name) (λ () (run-test suite))) ...
                   #f run-anon-tests
                   'logs (λ ()
                           (parameterize ([current-logs (hash)])
                             (run-anon-tests)
                             (current-logs))))))
         (define (internal)
           (syntax-parameterize ([dont-push? #t])
             (void
              (run-tests
               (test-suite
                "definition-area tests"
                suite ...
                (test-suite "anonymous tests" anon-test ...)))))))]))

(define-syntax $test-suite
  (syntax-parser
    [(_ name:string test:expr ...)
     (push-form! #'(test-suite name test ...) (syntax-e #'name))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; checks

(define-syntax ($check-expect stx)
  (syntax-parse stx
    [(_ actual expected)
     (push-form!
      (syntax/loc stx (check-equal? (error-if-parametric actual)
                                    (error-if-parametric expected))))]))

(define-syntax ($check-within stx)
  (syntax-parse stx
    [(_ actual expected ϵ)
     #:declare actual (expr/c #'number?)
     #:declare expected (expr/c #'number?)
     #:declare ϵ (expr/c #'number?)
     (push-form!
      (syntax/loc stx
        (let ([a actual.c] [e expected.c] [v ϵ.c])
          (check-within a e v))))]))

;; TODO: Need `error-if-parametric` here and other places.
(define-syntax ($check-member-of stx)
  (syntax-parse stx
    [(_ actual expecteds ...)
     (push-form!
      (syntax/loc stx (check-true ($member actual (list expecteds ...)))))]))

(define-syntax ($check-random stx)
  (syntax-parse stx
    [(_ actual expected)
     (push-form!
      (syntax/loc stx
        (let ([rng-vec (make-rng-vec)])
          (check-equal?
           (error-if-parametric (with-rng-vec rng-vec actual))
           (error-if-parametric (with-rng-vec rng-vec expected))))))]))

(define (make-rng-vec)
  (pseudo-random-generator->vector
   (make-pseudo-random-generator)))

(define-syntax-rule (with-rng-vec rng-vec e)
  (parameterize ([current-pseudo-random-generator
                  (vector->pseudo-random-generator rng-vec)])
    e))

(define-syntax ($check-range stx)
  (syntax-parse stx
    [(_ actual low high)
     #:declare low (expr/c #'number?)
     #:declare high (expr/c #'number?)
     #:declare actual (expr/c #'number?)
     (push-form!
      (syntax/loc stx
        (let ([l low.c] [a actual.c] [h high.c])
          (check-true (<= l a h)))))]))

(define-syntax ($check-satisfied stx)
  (syntax-parse stx
    [(_ actual pred)
     #:declare pred (expr/c #'predicate/c)
     (push-form!
      (syntax/loc stx
        (let ([v pred.c])
          (check-pred v actual))))]))

(define-syntax ($check-error stx)
  (syntax-parse stx
    [(_ body:expr)
     (push-form!
      (syntax/loc stx (check-exn (λ _ #t) (λ () body))))]
    [(_ body:expr msg)
     #:declare msg (expr/c #'string?)
     (push-form!
      (syntax/loc stx
        (let ([v msg.c])
          (check-exn (matches? v) (λ () body)))))]))

(define ((matches? msg) e)
  (string-contains? (exn-message e) msg))

(define-syntax ($check-raises stx)
  (syntax-parse stx
    [(_ body:expr)
     (push-form!
      (syntax/loc stx (check-exn exn:fail:lsl:user? (λ () body))))]
    [(_ body:expr exn:struct-id)
     (push-form!
      (syntax/loc stx
        (check-exn
         (λ (e)
           (and (exn:fail:lsl:user? e)
                (exn.predicate-id (exn:fail:lsl:user-val e))))
         (λ () body))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; verification

(define (proxy->contract p)
  (and (proxy? p) (proxy-contract p)))

;; HACK: Only for the `tick` homework. Figure something better out later.
(define ($contracted? p)
  (if (proxy->contract p) #t #f))

(define-syntax ($check-contract stx)
  (syntax-parse stx
    [(_ name:id (~optional n:number #:defaults ([n #'100])))
     #:with thk-body (syntax/loc stx (check-contract name 'name n))
     (push-form!
      #'(with-default-check-info*
          (list (make-check-params (list name (~? n))))
          (λ () (parameterize ([current-logs #f]) thk-body))))]))

;; TODO: parameterize by scaling?
(define (scale-fuel x)
  (if (zero? x) x (inexact->exact (ceiling (log x)))))

(define-check (check-contract val name n)
  (define ctc (proxy->contract val))
  (if ctc
      (for ([fuel (in-range 2 (+ n 2))])
        (check-or-verify-contract
         ctc val name
         (λ (ctc) (send ctc generate (scale-fuel fuel)))))
      (fail-check (format "unknown contract for ~a" name))))

(define-syntax ($verify-contract stx)
  (syntax-parse stx
    [(_ name:id)
     #:with thk-body (syntax/loc stx (verify-contract name 'name))
     (push-form!
      #'(with-default-check-info*
          (list (make-check-params (list name)))
          (λ () thk-body)))]))

(define-check (verify-contract val name)
  (define ctc (proxy->contract val))
  (if ctc
      (check-or-verify-contract ctc val name symbolic-mode)
      (fail-check (format "unknown contract for ~a" name))))

(define (check-or-verify-contract ctc val name mode)
  (match (send ctc interact val name mode)
    [(list eg exn)
     (fail-check (format VERIFY-FMT eg (indent (exn-message exn))))]
    [(none)
     (fail-check "failed to generate values associated with contract")]
    [#f
     (void)]))

(define (indent str)
  (string-append "  " (string-replace str "\n" "\n    ")))

(define VERIFY-FMT
  (string-join
   '("discovered a counterexample"
     "counterexample: ~a"
     "error:"
     "~a")
   "\n  "))
