#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/parse
                     syntax/parse/class/struct-id)
         racket/class
         (except-in racket/contract
                    blame?)
         racket/format
         racket/match
         racket/provide
         racket/string
         racket/stxparam
         rackunit
         rackunit/text-ui
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
   $check-contract)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test environments

(define-syntax-parameter dont-push? #f)

(begin-for-syntax
  (define anon-tests null)
  (define test-suites null)

  (define (push-form! stx [suite-name #f])
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
       (raise-syntax-error 'check "a test cannot be inside a definition or expression" stx)])))

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

;; Special case in `internal` needed to prevent printing "0 tests succeeded"
;; results in case no tests exist.
(define-syntax $define-run-tests
  (syntax-parser
    [(_ external:id internal:id)
     #:with (anon-test ...) (reverse anon-tests)
     #:with ([name . suite] ...) (reverse test-suites)
     #`(begin
         (define (run-anon-tests)
           (run-test (test-suite "anonymous tests" anon-test ...)))
         (define external
           (syntax-parameterize ([dont-push? #t])
             (hash (~@ (#%datum . name) (λ () (run-test suite))) ...
                   #f run-anon-tests
                   'tyche (λ ()
                            (parameterize ([current-pbt-stats (list)])
                              (run-anon-tests)
                              (reverse (current-pbt-stats))))
                   'logs (λ ()
                           (parameterize ([current-logs (hash)])
                             (run-anon-tests)
                             (current-logs))))))
         (define (internal)
           (syntax-parameterize ([dont-push? #t])
             #,(if (and (empty? anon-tests) (empty? test-suites))
                   #'(void)
                   #'(void
                      (run-tests
                       (test-suite
                        "definition-area tests"
                        suite ...
                        (test-suite "anonymous tests" anon-test ...))))))))]))

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

(define (default-size fuel)
  fuel)

(define-syntax ($check-contract stx)
  (syntax-parse stx
    [(_ name:id) #'($check-contract name 100 default-size)]
    [(_ name:id n:number) #'($check-contract name n default-size)]
    [(_ name:id n:number size:expr)
     #:with thk-body (syntax/loc stx (check-contract name 'name n size))
     (push-form!
      #'(with-default-check-info*
          (list (make-check-params (list name (~? n))))
          (λ () (parameterize ([current-logs #f]) thk-body))))]))

(define (get-fuel size n)
  (match size
    [(? procedure?) (size n)]
    [(? number?) size]
    [_ (fail-check "Expected procedure or number for size parameter")]))

(define-check (check-contract val name n size)
  (define ctc (proxy->contract val))
  (define time (current-seconds))
  (define tyche? (current-pbt-stats))
  (cond
    [ctc
     (define sample-results
       (for/list ([iter (in-range 2 (+ n 2))])
         (do-check-contract
          ctc val name time
          (λ (ctc)
            (send ctc generate (get-fuel size iter))))))
     (unless tyche?
       (for/first ([result (in-list sample-results)]
                   #:when (procedure? result))
         (result))
       (when (andmap exn:fail:gave-up? sample-results)
         (raise (car sample-results))))]
     [else (fail-check (format "~a either has no contract or does not have a function contract" name))]))

(define (push-stats! ht)
  (define stats (current-pbt-stats))
  (when stats (current-pbt-stats (cons ht stats))))

(define (do-check-contract ctc val name time contract->value)
  (define base-hash
    (hash 'type "test_case"
          'run_start time
          'status_reason ""
          'features (hash)
          'coverage "no_coverage_info"
          'metadata (hash)
          'property (~a name)))
  (define (handle-gave-up exn)
    (push-stats! (hash-set* base-hash 'representation "" 'status "gave_up"))
    exn)
  (define (handle-invalid exn)
    (define witness (exn:fail:invalid-witness exn))
    (push-stats! (hash-set* base-hash 'representation (~a witness) 'status "gave_up"))
    (λ () (raise exn)))
  (define (handle-other exn)
    (push-stats! (hash-set* base-hash
                            'representation (format "error: ~a" (exn-message exn))
                            'status "gave_up"))
    (λ () (raise exn)))
  (with-handlers ([exn:fail:gave-up? handle-gave-up]
                  [exn:fail:invalid? handle-invalid]
                  [exn? handle-other])
    (match (send ctc interact val name contract->value)
      [(list shrunk-eg init-eg feats exn)
       (push-stats!
        (hash-set* base-hash
                   'status "failed"
                   'representation (~a init-eg)
                   'features feats))
       (λ () (fail-check (format VERIFY-FMT shrunk-eg (indent (exn-message exn)))))]
      [(list pass-eg feats)
       (push-stats!
        (hash-set* base-hash
                   'status "passed"
                   'representation (~a pass-eg)
                   'features feats))])))

(define (indent str)
  (string-append "  " (string-replace str "\n" "\n    ")))

(define VERIFY-FMT
  (string-join
   '("discovered a counterexample"
     "counterexample: ~a"
     "error:"
     "~a")
   "\n  "))
