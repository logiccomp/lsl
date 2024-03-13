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
 SendPacket
 ReceivePacket
 Action
 start
 start-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/list
         racket/match
         "../syntax/interface.rkt"
         "../syntax/grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data
(struct process (start recv))
(struct action (state packets))

(struct packet (from to msg))
(struct send-packet (to msg) #:transparent)
(struct receive-packet (from msg) #:transparent)

(struct channel (from to msgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define-contract SendPacket
  (Immediate (check send-packet?)
             (generate (λ (fuel) (send-packet (contract-generate Natural fuel) (contract-generate Any fuel))))))

(define-contract ReceivePacket
  (Immediate (check receive-packet?)
             (generate (λ (fuel) (receive-packet (contract-generate Natural fuel) (contract-generate Any fuel))))))

(define-contract Action
  (Immediate (check action?)))

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
       (when debug
         (displayln (format ";;;; (packet #:from ~e #:to ~e #:msg ~e)"
                            (packet-from pkt)
                            (packet-to pkt)
                            (packet-msg pkt))))
       (define recv (process-recv (list-ref processes to)))
       (define old-state (list-ref states to))
       (match-define (action next-state next-packets)
         (recv old-state (receive-packet from msg)))
       (when debug
         (displayln (format ";;;; (state #:process ~e #:value ~e)" to next-state)))
       (go (list-set states to next-state)
           (route* to next-packets (unroute pkt channels)))])))

(define (eligible-packets cs)
  (append-map
   (λ (c)
     (match c
       [(channel from to (cons msg _))
        (list (packet from to msg))]
       [_ '()]))
   cs))

(define (route* from pkts channels)
  (for/fold ([channels channels])
            ([pkt (in-list pkts)])
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
