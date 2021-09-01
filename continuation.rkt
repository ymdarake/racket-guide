#lang racket
(define cc '())

(* 3
   (call/cc (lambda (k)
              (set! cc k)
              (+ 1 2))))
; #<continuation>

(+ 100 (cc 3))
; 9



(define ccc '())

(* 100
   (* 3
      (call/cc (lambda (k)
                 (set! ccc k)
                 (+ 1 2)))))
; #<continuation>

(+ 100 (cc 3))
; 900


(define (find-leaf obj tree)
  (call/cc
   (lambda (cc)
     (letrec ((iter
	       (lambda (tree)
		 (cond
		  ((null?  tree) #f)
		  ((pair? tree)
		   (iter (car tree))
		   (iter (cdr tree)))
		  (else
                   (begin
                     (displayln tree)
                     (when (eqv? obj tree)
		       (cc obj))))))))
       (iter tree)))))

(find-leaf 7 '(1 (2 3) 4 (5 (6 7)))) ; prints all
(find-leaf 3 '(1 (2 3) 4 (5 (6 7)))) ; prints 1,2,3