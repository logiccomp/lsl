#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require chk
         "util.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test

(define CONST-FOLDING-PROG
  '(begin
     (define-struct push [num])
     (define-struct add [])
     (define-struct mul [])
     (define-struct sub [])
     (define-contract SimpleInstr (OneOf (Push Integer) (Add) (Mul) (Sub)))

     (: simple-eval (-> (List Integer) (List SimpleInstr) (List Integer)))
     (define (simple-eval stk instrs)
       (local [; stack-binop : [Number Number -> Number] [List-of Number]
               ;               [List-of SimpleInstr] -> [List-of Number]
               ; evaluates a binary operator on top two numbers of stack, if present
               (define (stack-binop op stk instrs)
                 (if (>= (length stk) 2)
                     (simple-eval (cons (op (first stk) (second stk))
                                        (rest (rest stk)))
                                  instrs)
                     (list)))

               ; eval-instr : Instr [List-of Number] [List-of SimpleInstr] -> [List-of Number]
               ; evaluates a single instruction, given a stack and rest of instructions
               (define (eval-instr i stk instrs)
                 (cond [(add? i) (stack-binop + stk instrs)]
                       [(mul? i) (stack-binop * stk instrs)]
                       [(sub? i) (stack-binop - stk instrs)]
                       [(push? i) (simple-eval (cons (push-num i) stk) instrs)]))]
         (cond [(empty? instrs) stk]
               [(cons? instrs) (eval-instr (first instrs) stk (rest instrs))])))

     (: simple-stack-verify (-> (List SimpleInstr) (List SimpleInstr) Boolean))
     (define (simple-stack-verify p1 p2)
       (let* ([simple-eval-stack1 (simple-eval '() p1)]
              [simple-eval-stack2 (simple-eval '() p2)])
         (equal? simple-eval-stack1 simple-eval-stack2)))

     (: simple-const-fold-prop (-> (List SimpleInstr) True))
     (define (simple-const-fold-prop p)
       (let* ([folded-program (simple-const-fold p)])
         (simple-stack-verify p folded-program)))

     (define-contract BoundedProgram
       (OneOf (Tuple)
              (Tuple SimpleInstr)
              (Tuple SimpleInstr SimpleInstr)
              (Tuple SimpleInstr SimpleInstr SimpleInstr)
              (Tuple SimpleInstr SimpleInstr SimpleInstr SimpleInstr)))

     (: simple-const-fold-prop-symbolic (-> BoundedProgram True))
     (define (simple-const-fold-prop-symbolic p)
       (simple-const-fold-prop p))))

(define CORRECT
  '(begin
     (: simple-const-fold (-> (List SimpleInstr) (List SimpleInstr)))
     (define (simple-const-fold p)
       (cond [(< (length p) 3) p]
             [(and (push? (first p))
                   (push? (second p))
                   (add? (third p)))
              (cons (make-push (+ (push-num (first p))
                                  (push-num (second p))))
                    (simple-const-fold (rest (rest (rest p)))))]
             [else (cons (first p) (simple-const-fold (rest p)))]))))

(define INCORRECT
  '(begin
     (: simple-const-fold (-> (List SimpleInstr) (List SimpleInstr)))
     (define (simple-const-fold p)
       (cond [(< (length p) 3) p]
             [(and (push? (first p)) (push? (second p)) (add? (third p)))
              (cons (make-push (- (push-num (first p))
                                  (push-num (second p))))
                    (simple-const-fold (rest (rest (rest p)))))]
             [else (cons (first p) (simple-const-fold (rest p)))]))))

(module+ test
  (chk
   #:t
   (run/sexp #:no-result #t
             `(begin ,CONST-FOLDING-PROG
                     ,CORRECT
                     (verify-contract simple-const-fold-prop-symbolic)))
   #:x
   (run/sexp #:no-result #t
             `(begin ,CONST-FOLDING-PROG
                     ,INCORRECT
                     (verify-contract simple-const-fold-prop-symbolic)))
   "discovered a counterexample"
   ))
