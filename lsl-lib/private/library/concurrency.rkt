#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (rename-out
  [process-macro process])
 (struct-out packet)
 (struct-out send-packet)
 (struct-out receive-packet)
 action
 start
 start-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         racket/match)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data
(struct process (start recv))
(struct action (state packets))

(struct packet (from to msg))
(struct send-packet (to msg))
(struct receive-packet (from msg))

(struct channel (from to msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations

(define-syntax process-macro
  (syntax-parser
    #:datum-literals (on-start on-receive)
    [(_ (~alt (~once (on-start start:expr))
              (~once (on-receive recv:expr))) ...)
     #'(process start recv)]))

(define (start-debug scheduler processes)
  (start scheduler processes #t))

;; TODO: contract to ensure that one of eligible packets is in input
(define (start scheduler processes [debug #f])
  (define n (length processes))
  (define init-actions
    (for/list ([k (in-naturals)]
               [p (in-list processes)])
      ((process-start p) k (remove k (range n)))))
  (define init-states (map action-state init-actions))
  (define init-pkts (map action-packets init-actions))
  (define init-channels
    (for*/fold ([channels '()])
               ([from (in-range n)]
                [to (in-range n)])
      (define msgs
        (filter-map
         (λ (pkt)
           (match pkt
             [(send-packet (== to) msg) msg]
             [_ #f]))
         (list-ref init-pkts from)))
      (cons (channel from to msgs) channels)))
  (let go ([states init-states]
           [channels init-channels])
    (define possible (eligible-packets channels))
    (cond
      [(empty? possible) states]
      [else
       (define pkt (scheduler possible))
       (match-define (packet from to msg) pkt)
       (define recv (process-recv (list-ref processes to)))
       (define old-state (list-ref states to))
       (match-define (action next-state next-packets)
         (recv old-state (receive-packet from msg)))
       (go (list-set states to next-state)
           (route* to next-packets (unroute pkt channels) debug))])))

(define (eligible-packets cs)
  (append-map
   (λ (c)
     (match c
       [(channel from to (cons msg _))
        (list (packet from to msg))]
       [_ '()]))
   cs))

(define (route* from pkts channels [debug #f])
  (for/fold ([channels channels])
            ([pkt (in-list pkts)])
    (when debug
      (displayln (format ";;;; ~a -> ~a --- ~a"
                         from
                         (send-packet-to pkt)
                         (send-packet-msg pkt))))
    (route from pkt channels)))

;; TODO: Don't append, too slow?
(define (route from pkt channels)
  (match-define (send-packet to msg) pkt)
  (let go ([channels channels])
    (match channels
      [(list) (list)]
      [(cons (channel (== from) (== to) msgs) rst)
       (cons (channel from to (append msgs (list msg))) rst)]
      [(cons c rst) (cons c (go rst))])))

(define (unroute pkt channels)
  (match-define (packet from to msg) pkt)
  (let go ([channels channels])
    (match channels
      [(list) (list)]
      [(cons (channel (== from) (== to) (cons msg rst-msgs)) rst)
       (cons (channel from to rst-msgs) rst)]
      [(cons c rst) (cons c (go rst))])))
