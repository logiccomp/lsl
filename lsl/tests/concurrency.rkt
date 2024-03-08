#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(module+ test
  (chk
   (run (define p
          (process
           (on-start
            (λ (self others)
              (action 0 (list (send-packet 0 1)))))
           (on-receive
            (λ (state pkt)
              (cond
                [(> state 10)
                 (action state (list))]
                [else
                 (let ([state* (add1 (receive-packet-msg pkt))])
                   (action state* (list (send-packet 0 state*))))])))))
        (start first (list p)))
   '(11)

   (run (define p
          (process
           (on-start
            (λ (self others)
              (let ([id (modulo (add1 self) (add1 (length others)))])
                (action 'none (list (send-packet id id))))))
           (on-receive
            (λ (state pkt)
              (action (receive-packet-msg pkt) (list))))))
        (start (λ (x) (list-ref x (sub1 (length x))))
               (list p p p p p)))
   '(0 1 2 3 4)

   (run (define (p xs)
          (process
           (on-start
            (λ (self others)
              (action xs (list))))
           (on-receive
            (λ (state pkt)
              (let ([from (receive-packet-from pkt)])
                (if (empty? state)
                    (action '()
                            (list (send-packet from 'done)))
                    (action (rest state)
                            (list (send-packet from (first state))))))))))
        (define m
          (process
           (on-start
            (λ (self others)
              (action 0 (map (λ (x) (send-packet x 'gimme)) others))))
           (on-receive
            (λ (state pkt)
              (let ([from (receive-packet-from pkt)]
                    [msg (receive-packet-msg pkt)])
                (if (eq? 'done msg)
                    (action state '())
                    (action (+ state msg)
                            (list (send-packet from 'gimme)))))))))
        (start (λ (x) (list-ref x (random (length x))))
               (list m (p '(1 2 3)) (p '(4 5 6)) (p '(7 8 9)))))
   '(45 () () ())
   ))
