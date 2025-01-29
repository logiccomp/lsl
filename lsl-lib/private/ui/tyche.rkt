#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide tyche-button
         process-prog)

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
