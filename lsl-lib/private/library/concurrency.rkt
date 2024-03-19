#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [start start/c]
  [start-debug start/c])
 (rename-out
  [process-macro process])
 (rename-out [Packet~ Packet]
             [SendPacket~ SendPacket]
             [ReceivePacket~ ReceivePacket]
             [Action~ Action]
             [make-action action]
             [make-send-packet send-packet]
             [make-receive-packet receive-packet])
 packet-from
 packet-to
 packet-msg
 packet?
 send-packet-to
 send-packet-msg
 send-packet?
 receive-packet-from
 receive-packet-msg
 receive-packet?

 action-state
 action-packets
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     syntax/parse)
         racket/function
         racket/list
         racket/match
         (prefix-in ^ rosette/safe)
         (only-in "core.rkt" define-struct)
         (only-in "contract.rkt" String Any)
         "../syntax/interface.rkt"
         "../syntax/grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct process (name start recv) #:transparent)
(define-struct action (state packets))

(define-struct packet (from to msg))
(define-struct send-packet (to msg))
(define-struct receive-packet (from msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define-contract (Packet~ S)
  (Struct packet String String S))

(define-contract (SendPacket~ S)
  (Struct send-packet String S))

(define-contract (ReceivePacket~ S)
  (Struct receive-packet String S))

(define-contract (Action~ S)
  (Struct action S (List (SendPacket~ Any))))

(define (action/c names)
  (and/c
   action?
   (flat-contract-with-explanation
    (λ (val)
      (define pkts (action-packets val))
      (cond
        [(findf (negate send-packet?) pkts)
         =>
         (λ (bad-packet)
           (λ (blm)
             (raise-blame-error blm val
                                '(expected: "SendPacket" given: "~v")
                                bad-packet)))]
        [(findf (invalid-send-packet names) pkts)
         =>
         (λ (bad-packet)
           (λ (blm)
             (raise-blame-error blm val
                                '(expected: "a valid process name" given: "~a")
                                (send-packet-to bad-packet))))]
        [else #t])))))

(define ((invalid-send-packet names) pkt)
  (not (member (send-packet-to pkt) names)))

(define (valid-packet? candidates)
  (flat-named-contract
   'valid-packet?
   (curryr member candidates)))

(define (self/c make-ctc)
  (make-chaperone-contract
   #:name 'self/c
   #:late-neg-projection
   (λ (blm)
     (λ (arg neg-party)
       (define ctc (coerce-contract 'self/c (make-ctc arg)))
       (define late-neg-proj (get/build-late-neg-projection ctc))
       ((late-neg-proj blm) arg neg-party)))))

(define start/c
  (-> (->i ([candidates (listof packet?)])
           [result (candidates) (and/c packet? (valid-packet? candidates))])
      (and/c
       (listof process?)
       (self/c
        (λ (procs)
          (define names (map process-name procs))
          (listof (struct/c process
                            any/c
                            (-> any/c (action/c names))
                            (-> any/c any/c (action/c names)))))))
      (listof (cons/c string? any/c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; operations

(define-syntax process-macro
  (syntax-parser
    #:datum-literals (name on-start on-receive)
    [(_ (~alt (~once (name n))
              (~once (on-start start))
              (~once (on-receive recv))) ...)
     #:declare n (expr/c #'string? #:name "process name")
     #:declare start (expr/c #'(-> (listof string?) action?)
                             #:name "start handler")
     #:declare recv (expr/c #'(-> any/c receive-packet? action?)
                            #:name "receive handler")
     #'(process n.c start.c recv.c)]))

(define (start-debug scheduler processes)
  (start scheduler processes #t))

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
    (make-packet from to (first msgs))))

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
