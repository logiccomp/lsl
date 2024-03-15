#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; integration tests

(define (hash=? x y)
  (equal? (make-hash (map (λ (l) (cons (list-ref l 0) (list-ref l 1))) x))
          (make-hash y)))

(module+ test
  (chk
   #:eq hash=?
   (run (define p
          (process
           (name "A")
           (on-start
            (λ (others)
              (action 0 (list (send-packet "A" 1)))))
           (on-receive
            (λ (state pkt)
              (cond
                [(> state 10)
                 (action state (list))]
                [else
                 (let ([state* (add1 (receive-packet-msg pkt))])
                   (action state* (list (send-packet "A" state*))))])))))
        (start first (list p)))
   '(["A" . 11])

   #:eq hash=?
   (run (define (p self)
          (process
           (name (number->string self))
           (on-start
            (λ (others)
              (let* ([id-num (modulo (add1 self) (add1 (length others)))]
                     [id (number->string self)])
                (action 'none (list (send-packet id id-num))))))
           (on-receive
            (λ (state pkt)
              (action (receive-packet-msg pkt) (list))))))
        (start (λ (x) (list-ref x (sub1 (length x))))
               (build-list 5 p)))
   '(["0" . 1] ["1" . 2] ["2" . 3] ["3" . 4] ["4" . 0])

   #:eq hash=?
   (run (define (p self xs)
          (process
           (name self)
           (on-start
            (λ (others)
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
           (name "manager")
           (on-start
            (λ (others)
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
               (list m
                     (p "alice" '(1 2 3))
                     (p "bob" '(4 5 6))
                     (p "charlie" '(7 8 9)))))
   '(["manager" . 45] ["bob" . ()] ["charlie" . ()] ["alice" . ()])
   ))
