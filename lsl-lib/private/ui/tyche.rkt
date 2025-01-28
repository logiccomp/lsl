#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide tyche-button)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require images/icons/misc
         json
         net/sendurl
         racket/gui
         racket/runtime-path)

(define-runtime-path tyche-site (build-path "site" "index.html"))
(define-runtime-path tyche-data.js (build-path "site" "static" "js" "data.js"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; button

(define tyche-button
  (list
   "Tyche"
   (magnifying-glass-icon #:height 16)
   (λ (window)
     (define editor (send window get-definitions-text))
     (define tmp (make-temporary-file))
     (with-output-to-file tmp (lambda () (write-string (editor->string editor))) #:exists 'replace)
     (define open-pbt-stats-json (process-prog tmp))
     (with-output-to-file tyche-data.js
       (lambda () (printf (hash-to-json-string open-pbt-stats-json))) #:exists 'replace)
     (open-html-in-browser (path->string tyche-site))
     #f)
   #f))

(define (editor->string ed)
  (send ed get-text))

(define (open-html-in-browser file-path)
  (send-url/file file-path))

(define (hash-to-json-string hash)
  (let ([json-data (jsexpr->string hash)])
    (string-append "var MY_GLOBAL = " json-data)))

;; process-prog : Path -> OpenPBTStats
;; Produces an OpenPBTStats JSON representing the results of
;; running the PBT in the given LSL file.
(define (process-prog f)
  ((hash-ref (dynamic-require f 'run-tests) 'tyche)))

(module+ test
  (define (make-lsl-file s)
    (define tmp (make-temporary-file))
    (with-output-to-file tmp (lambda ()
                               (printf "~a\n" "#lang lsl")
                               (map (lambda (x) (write x)) s)) #:exists 'replace)
    tmp)

  (define p1
    (make-lsl-file
     (quote
      ((: foo (-> Integer Integer))
       (define (foo x) x)

       (check-contract foo)

       (: bar (-> Integer Integer))
       (define (bar x) 5)

       (check-contract bar)))))

  (define p2
    (make-lsl-file
     (quote
      ((: foo (-> Integer positive?))
       (define (foo x) x)

       (check-contract foo)))))

  (define p3
    (make-lsl-file
     (quote
      ((define-contract Even
         (Immediate (check (lambda (x) (and (integer? x) (even? x))))
                    (generate (lambda (fuel) (+ 1 (* 2 (contract-generate Integer fuel)))))
                    (feature "positive?" positive?)))
       (: bar (-> Even Integer))
       (define (bar x) x)
       (check-contract bar)))))

  (require rackunit)

  (define p1-result (process-prog p1))
  (check-true (and (> (length p1-result) 0)
                   (andmap (lambda (r) (equal? (hash-ref r 'status) "passed")) p1-result)))

  (define p2-result (process-prog p2))
  (check-true (ormap (lambda (r) (equal? (hash-ref r 'status) "failed")) p2-result))
  (check-true (ormap (lambda (r) (equal? (hash-ref r 'status) "passed")) p2-result))

  (define p3-result (process-prog p3))
  (check-true (and (> (length p3-result) 0)
                   (andmap (lambda (r) (equal? (hash-ref r 'status) "gave_up")) (process-prog p3)))))

