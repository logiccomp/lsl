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

(define ERROR "Encountered an error while running Tyche.\nTry clicking Run for more information.")

(define tyche-thread #f)
(define tyche-button
  (list
   "Tyche"
   (magnifying-glass-icon #:height 16)
   (λ (window)
     (define editor (send window get-definitions-text))
     (define tmp (make-temporary-file))
     (with-output-to-file tmp (lambda () (write-string (editor->string editor))) #:exists 'replace)
     (unless tyche-thread
       (set! tyche-thread
             (thread
              (λ ()
                (define open-pbt-stats-json (process-prog tmp))
                (cond
                  [open-pbt-stats-json
                   (with-output-to-file tyche-data.js
                     (λ () (display (hash-to-json-string open-pbt-stats-json))) #:exists 'replace)
                   (open-html-in-browser (path->string tyche-site))]
                  [else (message-box "Tyche Error" ERROR)])
                (set! tyche-thread #f))))))
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
  (with-handlers ([exn? (λ _ #f)])
    ((hash-ref (dynamic-require f 'run-tests) 'tyche))))
