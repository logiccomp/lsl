#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(require racket/contract
         (submod "concurrency.rkt" private))
(provide
 (contract-out
  [start-gui start-gui/c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (except-in metapict identity text color table window @)
         (prefix-in metapict: (only-in metapict text color))
         (only-in racket/vector vector-sort)
         racket/format
         racket/function
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
;; gui

(define (start-gui scheduler processes summarize-state)
  (render-state (λ (st) (format "~a" (summarize-state st))))
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
     (list "From" "To" "Messages")
     (@steps . ~> . (compose1 move->table history-current))))

  (define history-view
    (table
     (list "States")
     (@steps . ~> . history->table)))

  (render
   (window
    (vpanel
     (tabs
      '(graph queues history)
      #:choice->label (compose1 string-titlecase symbol->string)
      (λ (event _choices selection)
        (match event
          ['select (@tab . := . selection)]))
      (observable-view
       @tab
       (match-lambda
         ['graph graph-view]
         ['queues queues-view]
         ['history history-view])))
     (hpanel
      #:alignment '(center center)
      (button "←"
              (λ () (@steps . <~ . history-prev))
              #:enabled? (@steps . ~> . history-has-prev?))
      (input (@steps . ~> . (compose1 number->string history-when))
             (λ (evt num)
               (when (eq? evt 'return)
                 (@steps . <~ . (curry history-move (string->number num)))))
             #:min-size '(48 0)
             #:stretch '(#f #f)
             #:value=? (λ _ #f))
      (text (format " of ~a" (sub1 (length transcript))))
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

(define (history-when h)
  (length (history-context h)))

(define (history-move n h)
  (match-define (history context focus) h)
  (define full (append (reverse context) focus))
  (define N (length full))
  (cond
    [(and n (<= 0 n) (< n N))
     (define-values (rev-context* focus*) (split-at full n))
     (history (reverse rev-context*) focus*)]
    [else
     h]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; drawing helpers

(define render-state (make-parameter (λ (st) (format "~a" st))))

(define (history->table h)
  (for*/vector ([move (in-list (history-context h))])
    (vector (string-join (map (λ(st) (format "~a:~a" (car st) ((render-state) (cdr st))))
                              (sort (hash->list (system-states (move-system move)))
                                    (λ(p1 p2) (string<? (car p1) (car p2))))) ", "))))

(define (move->table m)
  (define sys (move-system m))
  (define channels (system-channels sys))
  (vector-sort
   (for*/vector ([(to inbox) (in-hash channels)]
                 [(from msgs) (in-hash inbox)])
     (vector from to (string-join (map ~v msgs) ", ")))
   (λ(v1 v2) (string<? (vector-ref v1 0) (vector-ref v2 0)))))

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
    (dot-label (format "~a ⟨~a⟩" proc ((render-state) st))
               p
               (if (> (pt-y p) 1/2) (top) (bot)))))

(define (draw-packets ht pkts dir)
  (for/draw ([pkt (in-list (reverse pkts))] [k (in-naturals 1)])
    (draw-packet ht pkt (/ k (add1 (length pkts))) dir)))

(define (move-towards twds orig)
  (pt (+ (* 0.8 (pt-x orig)) (* 0.2 (pt-x twds)))
      (+ (* 0.8 (pt-y orig)) (* 0.2 (pt-y twds)))))

(define (draw-packet ht pkt α dir)
  (match-define (packet from to msg) pkt)
  (define src (hash-ref ht from))
  (define dst (hash-ref ht to))
  (define txt (~v (packet-msg pkt)))
  (define dot (dot-label txt (med α src dst) (top)))
  (let* ([result
          (if (equal? dir 'send)
              (draw-arrow (curve src .. (move-towards src dst))
                          #:length AH
                          #:color "green")
              (draw-arrow (curve (move-towards dst src) .. dst)
                          #:length AH
                          #:color "orange"))]
         [result (draw result dot)])
    result))

(define (system->positions sys)
  (define states (system-states sys))
  (for/hash ([(proc state) (in-hash states)]
             [θ (in-range 0 360 (/ 360 (hash-count states)))])
    (values proc (pt@d R θ))))
