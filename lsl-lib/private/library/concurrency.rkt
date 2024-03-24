#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract)
(provide
 (contract-out
  [start start/c]
  [start-debug start/c]
  [start-gui start/c])
 (rename-out
  [process-macro process])
 Process
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
 action-packets)

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
         "../syntax/grammar.rkt"

         (except-in metapict identity text color table window @)
         (prefix-in metapict: (only-in metapict text color))
         racket/format
         racket/string
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match
         racket/math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; constants

(define R 1)
(define AH 0.05)
(define SIZE 800)
(set-curve-pict-size SIZE SIZE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data

(struct process (name start recv) #:transparent)
(define-struct action (state packets))

(define-struct packet (from to msg))
(define-struct send-packet (to msg))
(define-struct receive-packet (from msg))

(struct system (states channels))
(struct move (system))
(struct recv-move move (packet))
(struct send-move move (packets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; contracts

(define-contract Process process?)

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

(define (start scheduler processes)
  (start-help scheduler processes))

(define (start-debug scheduler processes)
  (start-help scheduler processes #:debug #t))

(define (start-transcript scheduler processes)
  (start-help scheduler processes #:transcript null))

(define (start-help scheduler processes
                    #:debug [debug #f]
                    #:transcript [transcript #f])
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
       (define result
         (for/list ([(name state) (in-hash states)])
           (^list name state)))
       (if transcript
           (values result (reverse transcript))
           result)]
      [else
       (define pkt (scheduler possible))
       (match-define (packet from to msg) pkt)
       (when debug
         (displayln (format ";;;; (packet #:from ~e #:to ~e #:msg ~e)" from to msg)))
       (define recv (process-recv (hash-ref process-hash to)))
       (define old-state (hash-ref states to))
       (define act (recv old-state (make-receive-packet from msg)))
       (match-define (action next-state next-packets) act)
       (when debug
         (displayln (format ";;;; (state #:process ~e #:value ~e)" to next-state)))
       (define states* (hash-set states to next-state))
       (define channels* (route* (pop-inbox channels from to) to next-packets))
       (when transcript
         (set! transcript (cons (recv-move (system states channels) pkt) transcript))
         (define out-pkts
           (for/list ([pkt (in-list next-packets)])
             (make-packet to (send-packet-to pkt) (send-packet-msg pkt))))
         (set! transcript (cons (send-move (system states* channels*) out-pkts) transcript)))
       (go states* channels*)])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gui

(define (start-gui scheduler processes)
  (define-values (result transcript)
    (start-transcript scheduler processes))
  (define @steps (@ (make-history transcript)))
  (define @tab (@ 'graph))

  (define graph-view
    (pict-canvas
     #:min-size (list SIZE SIZE)
     (@steps . ~> . history-current)
     move->pict))

  (define queues-view
    (table
     (list "To" "From" "Messages")
     (@steps . ~> . (compose1 move->table history-current))))

  (render
   (window
    (vpanel
     (tabs
      '(graph queues)
      #:choice->label (compose1 string-titlecase symbol->string)
      (λ (event _choices selection)
        (match event
          ['select (@tab . := . selection)]))
      (observable-view
       @tab
       (match-lambda
         ['graph graph-view]
         ['queues queues-view])))
     (hpanel
      #:alignment '(center center)
      (button "←"
              (λ () (@steps . <~ . history-prev))
              #:enabled? (@steps . ~> . history-has-prev?))
      (button "→"
              (λ () (@steps . <~ . history-next))
              #:enabled? (@steps . ~> . history-has-next?))))))

  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; history

(struct history (context focus))

(define (make-history xs)
  (history '() xs))

(define (history-next h)
  (match-define (history ctx (cons fst rst)) h)
  (history (cons fst ctx) rst))

(define (history-prev h)
  (match-define (history (cons fst rst) focus) h)
  (history rst (cons fst focus)))

(define (history-current h)
  (first (history-focus h)))

(define (history-has-next? h)
  (cons? (rest (history-focus h))))

(define (history-has-prev? h)
  (cons? (history-context h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drawing helpers

(define (move->table m)
  (define sys (move-system m))
  (define channels (system-channels sys))
  (for*/vector ([(to inbox) (in-hash channels)]
                [(from msgs) (in-hash inbox)])
    (vector from to (string-join (map ~a msgs) ", "))))

(define (move->pict m)
  (define sys (move-system m))
  (define ht (system->positions sys))
  (draw
   (draw-system ht sys)
   (cond
     [(recv-move? m) (draw-recv-move ht (recv-move-packet m))]
     [(send-move? m) (draw-send-move ht (send-move-packets m))])))

(define (draw-recv-move ht pkt)
  (draw-packets ht (list pkt) 'recv))

(define (draw-send-move ht pkts)
  (define groups (group-by packet-to pkts))
  (for/draw ([group (in-list groups)])
    (draw-packets ht group 'send)))

(define (draw-system ht sys)
  (for/draw ([(proc st) (in-hash (system-states sys))])
    (define p (hash-ref ht proc))
    (dot-label (format "~a ⟨~a⟩" proc st)
               p
               (if (> (pt-y p) 1/2) (top) (bot)))))

(define (draw-packets ht pkts dir)
  (for/draw ([pkt (in-list pkts)] [k (in-naturals 1)])
    (draw-packet ht pkt (/ k (add1 (length pkts))) dir)))

(define (draw-packet ht pkt α dir)
  (match-define (packet from to msg) pkt)
  (define src (hash-ref ht from))
  (define dst (hash-ref ht to))
  (define txt (~a (packet-msg pkt)))
  (define dot (dot-label txt (med α src dst) (top)))
  (let* ([result
          (if (equal? dir 'send)
              (draw-arrow (curve src .. dst)
                             #:tail (reverse-head arrow-head)
                             #:head line-head
                             #:length AH
                             #:color "green")
              (draw-arrow (curve src .. dst)
                             #:head arrow-head
                             #:length AH
                             #:color "orange"))]
         [result (draw result dot)])
    result))

(define (system->positions sys)
  (define states (system-states sys))
  (for/hash ([(proc state) (in-hash states)]
             [θ (in-range 0 360 (/ 360 (hash-count states)))])
    (values proc (pt@d R θ))))
