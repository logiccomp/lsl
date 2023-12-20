#lang racket/base

(require (for-syntax racket/base))

;; From `phc-toolkit` by Suzanne Soy.

(provide syntax/whole-loc
         quasisyntax/whole-loc)

(define (fold-syntax f stx)
  (let process ([stx stx])
    (cond
      [(syntax? stx)
       (f stx (λ (x)
                (let ([p (process (syntax-e x))])
                  (if (syntax? p)
                      p
                      (datum->syntax stx p stx stx)))))]
      [(pair? stx)
       (cons (process (car stx))
             (process (cdr stx)))]
      [(null? stx)
       stx]
      [(vector? stx)
       (list->vector (map process (vector->list stx)))]
      [(box? stx)
       (box (process (unbox stx)))]
      [(hash? stx)
       (define processed (process (hash->list stx)))
       (cond
         [(hash-equal? stx) (make-hash processed)]
         [(hash-eqv? stx) (make-hasheqv processed)]
         [(hash-eq? stx) (make-hasheq processed)])]
      [(prefab-struct-key stx)
       (apply make-prefab-struct
              (prefab-struct-key stx)
              (map process (vector->list (struct->vector stx))))]
      [else
       stx])))

;; Use the following function to replace the loc throughout stx
;; instead of stopping the depth-first-search when the syntax-source
;; is not old-source anymore
(define (replace-whole-loc stx old-source new-loc)
  (fold-syntax
   (λ (stx rec)
     (datum->syntax stx (syntax-e (rec stx)) new-loc stx))
   stx))

(define-syntax (syntax/whole-loc stx)
  (syntax-case stx ()
    [(self loc template)
     #'(replace-whole-loc #'template (syntax-source #'self) loc)]))

(define-syntax (quasisyntax/whole-loc stx)
  (syntax-case stx ()
    [(self loc template)
     #'(replace-whole-loc #`template (syntax-source #'self) loc)]))
