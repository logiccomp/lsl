#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide (struct-out root)
         (struct-out proc)
         (struct-out proxy)
         unproxy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct root ()
  #:transparent
  #:mutable
  #:methods gen:equal+hash
  [(define (equal-proc self other recur)
     (recur (unproxy self) (unproxy other)))
   (define (hash-proc self recur)
     (recur (unproxy self)))
   (define (hash2-proc self recur)
     (recur (unproxy self)))])

(struct proc root (target)
  #:property prop:procedure 0)

(struct proxy root (target contract)
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (define recur
       (case mode
         [(#t) write]
         [(#f) display]
         [else (λ (p port) (print p port mode))]))
     (recur (unproxy self) port))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; definitions

(define (unproxy st)
  (if (proxy? st) (unproxy (proxy-target st)) st))
