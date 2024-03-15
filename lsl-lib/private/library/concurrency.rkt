#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide
 (rename-out
  [process-macro process])
 (struct-out packet)
 (rename-out [SendPacket~ SendPacket]
             [make-send-packet send-packet]
             [ReceivePacket~ ReceivePacket]
             [make-receive-packet receive-packet]
             [Action~ Action]
             [make-action action])
 send-packet-to
 send-packet-msg
 send-packet?
 receive-packet-from
 receive-packet-msg
 receive-packet?
 start
 start-debug)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         racket/list
         racket/match
         (prefix-in ^ rosette/safe)
         (only-in "core.rkt" define-struct)
         "../syntax/interface.rkt"
         "../syntax/grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct process (name start recv) #:transparent)


(define-struct action (state packets))

(struct packet (from to msg))
(define-struct send-packet (to msg))
(define-struct receive-packet (from msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define-contract (SendPacket~ S)
  (Struct send-packet String S))

(define-contract (ReceivePacket~ S)
  (Struct receive-packet String S))

(define-contract (Action~ S)
  (Struct action S (List (SendPacket~ Any))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations

(define-syntax process-macro
  (syntax-parser
    #:datum-literals (name on-start on-receive)
    [(_ (~alt (~once (name n:expr))
              (~once (on-start start:expr))
              (~once (on-receive recv:expr))) ...)
     #'(process n start recv)]))

(define (start-debug scheduler processes)
  (start scheduler processes #t))

;; TODO: contract to ensure that one of eligible packets is in input
;; TODO: contract to ensure msg sender is valid process
(define (start scheduler processes [debug #f])
  (define process-hash
    (for/hash ([p (in-list processes)])
      (values (process-name p) p)))
  (define names (hash-keys process-hash))
  (define init-actions
    (for/hash ([(name proc) (in-hash process-hash)])
      (define others (remove name names))
      (values name ((process-start proc) others))))
  (define init-states
    (for/hash ([(name action) (in-hash init-actions)])
      (values name (action-state action))))
  (define init-channels
    (for*/fold ([acc (hash)])
               ([from (in-list names)]
                [action (in-value (hash-ref init-actions from))]
                [pkt (in-list (action-packets action))])
      (match-define (send-packet to msg) pkt)
      (define (update ht)
        (hash-update ht from (curryr append (list msg)) null))
      (hash-update acc to update (hash))))
  (let go ([states init-states]
           [channels init-channels])
    (define possible (eligible-packets channels))
    (cond
      [(empty? possible)
       (for/list ([(name state) (in-hash states)])
         (^list name state))]
      [else
       (match-define (packet from to msg)
         (scheduler possible))
       (when debug
         (displayln (format ";;;; (packet #:from ~e #:to ~e #:msg ~e)" from to msg)))
       (define recv (process-recv (hash-ref process-hash to)))
       (define old-state (hash-ref states to))
       (match-define (action next-state next-packets)
         (recv old-state (make-receive-packet from msg)))
       (when debug
         (displayln (format ";;;; (state #:process ~e #:value ~e)" to next-state)))
       (go (hash-set states to next-state)
           (route* (pop-inbox channels from to) to next-packets))])))

(define (eligible-packets channels)
  (for*/list ([(to inbox) (in-hash channels)]
              [(from msgs) (in-hash inbox)]
              #:when (not (empty? msgs)))
    (packet from to (first msgs))))

(define (route* channels from pkts)
  (for/fold ([channels channels])
            ([pkt (in-list pkts)])
    (route channels from pkt)))

(define (route channels from pkt)
  (match-define (send-packet to msg) pkt)
  (define (update ht)
    (hash-update ht from (curryr append (list msg)) null))
  (hash-update channels to update (hash)))

(define (pop-inbox channels from to)
  (define inboxes (hash-ref channels to))
  (hash-set channels to (hash-update inboxes from rest)))
