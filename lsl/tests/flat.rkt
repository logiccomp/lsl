#lang racket/base

(require chk
         "util.rkt")

(chk
 (run (: x Integer)
      (define x 10)
      x)
 10

 #:x
 (run (: x Boolean)
      (define x 10)
      x)
 "expected: Boolean"
 )
