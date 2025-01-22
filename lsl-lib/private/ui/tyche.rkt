#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide tyche-button)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require json
         racket/class
         racket/gui
         racket/runtime-path
         net/sendurl
         pict)

(require "tyche-core.rkt")

(define-runtime-path tyche-site "./site/index.html")
(define-runtime-path tyche-data.js "./site/static/js/data.js")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; button

(define tyche-button
  (list
   "Launch Tyche"
   (pict->bitmap (standard-fish 32 16 #:color "salmon"))
   (λ (window)
     (define editor (send window get-definitions-text))
     (define tmp (make-temporary-file))
     (with-output-to-file tmp (lambda () (write-string (editor->string editor))) #:exists 'replace)
     (define open-pbt-stats-json (process-prog tmp))
     (with-output-to-file tyche-data.js (lambda () (printf (hash-to-json-string open-pbt-stats-json))) #:exists 'replace)
     (open-html-in-browser (path->string tyche-site))
     #f)
   #f))

(define (editor->string ed)
  (send ed get-text))

(define (open-html-in-browser file-path)
  (send-url file-path))

(define (hash-to-json-string hash)
  (let ([json-data (jsexpr->string hash)])
    (string-append "var MY_GLOBAL = " json-data)))
