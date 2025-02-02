#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         racket/file
         racket/match
         lsl/private/ui/tyche)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (define (make-lsl-file s)
    (define tmp (make-temporary-file))
    (write-to-file `(module _ lsl ,@s) tmp #:exists 'replace)
    tmp)

  (define p1
    (make-lsl-file
     '((: foo (-> Integer Integer))
       (define (foo x) x)

       (check-contract foo)

       (: bar (-> Integer Integer))
       (define (bar x) 5)

       (check-contract bar))))

  (define p2
    (make-lsl-file
     '((define-contract Even
         (Immediate (check (λ (x) (and (integer? x) (even? x))))
                    (generate (λ (fuel) (* 2 (contract-generate Integer fuel))))
                    (feature "positive?" positive?)))

       (: foo (-> Even positive?))
       (define (foo x) x)

       (check-contract foo))))

  (define p3
    (make-lsl-file
     '((define-contract Even
         (Immediate (check (λ (x) (and (integer? x) (even? x))))
                    (generate (λ (fuel) (+ 1 (* 2 (contract-generate Integer fuel)))))
                    (feature "positive?" positive?)))
       (: bar (-> Even Integer))
       (define (bar x) x)
       (check-contract bar))))

  (define p4
    (make-lsl-file
     '((: bar (-> (Immediate (check even?)) Integer))
       (define (bar x) x)
       (check-contract bar))))

  (chk
   #:do (define p1-result (process-prog p1))
   #:t (and (= (length p1-result) 200)
            (andmap (λ (r) (equal? (hash-ref r 'status) "passed")) p1-result))

   #:do (define p2-result (process-prog p2))
   #:t (ormap (λ (r) (equal? (hash-ref r 'status) "failed")) p2-result)
   #:t (ormap (λ (r) (equal? (hash-ref r 'status) "passed")) p2-result)
   #:t (for/and ([ht (in-list p2-result)])
         (match ht
           [(hash 'status "passed" 'features (hash 'positive? #t) #:open) #t]
           [(hash 'status "failed" 'features (hash 'positive? #f) #:open) #t]
           [_ #f]))

   #:do (define p3-result (process-prog p3))
   #:t (and (= (length p3-result) 100)
            (andmap (λ (r) (equal? (hash-ref r 'status) "invalid")) p3-result))

   #:do (define p4-result (process-prog p4))
   #:t (and (= (length p4-result) 100)
            (andmap (λ (r) (equal? (hash-ref r 'status) "gave_up")) p4-result))
   ))
