#lang racket
(define cc '())

(* 3
   (call/cc (lambda (k)
              (set! cc k)
              (+ 1 2))))
; #<continuation>#


(+ 100 (cc 3))
; 9
