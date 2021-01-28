#lang racket


;; 16.1.1 define-syntax-rule
(define-syntax-rule (swap x y)
  (let ([tmp x])
    (set! x y)
    (set! y tmp)))


;; 16.1.2 Lexical Scope
(let ([tmp 5]
      [other 6])
  (swap tmp other)
  (list tmp other))

(let ([set! 5]
      [other 6])
  (swap set! other)
  (list set! other))

; the local set! binding doesn’t interfere with the assignments introduced by the macro template.


;; 16.1.3 define-syntax and syntax-rules

; Racket’s macro system supports transformers that match multiple patterns starting with the same identifier.
(define-syntax rotate
  (syntax-rules ()
    [(rotate a b) (swap a b)]
    [(rotate a b c) (begin
                      (swap a b)
                      (swap b c))]))

(let ([red 1] [green 2] [blue 3])
  (rotate red green); 2 1
  (rotate red green blue); 1 3 2
  (list red green blue)); '(1 3 2)


;; 16.1.4 Matching Sequences

; To match a use of rotate with any number of identifiers, we need a pattern form that has something like a Kleene star. In a Racket macro pattern, a star is written as '...'.
(define-syntax rotate-seq
  (syntax-rules ()
    [(rotate-seq a) (void)]
    [(rotate-seq a b c ...) (begin
                              (swap a b)
                              (rotate-seq b c ...))]))

(let ([a 1])
    (rotate-seq a)
    (list a)); '(1)

(let ([a 1] [b 2])
    (rotate-seq a b)
    (list a b)); '(2 1)

(let ([a 1] [b 2] [c 3])
    (rotate-seq a b c)
    (list a b c)); '(2 3 1)

; A more efficient rotate would move the first value directly to the last variable. We can use ... patterns to implement the more efficient variant using a helper macro:
(define-syntax rotate-efficiently
  (syntax-rules ()
    [(rotate-efficiently a c ...)
     (shift-to (c ... a) (a c ...))]))
 
(define-syntax shift-to
  (syntax-rules ()
    [(shift-to (from0 from ...) (to0 to ...))
     (let ([tmp from0])
       (set! to from) ...
       (set! to0 tmp))]))

(let ([a 1])
    (rotate-efficiently a)
    (list a)); '(1)

(let ([a 1] [b 2])
    (rotate-efficiently a b)
    (list a b)); '(2 1)

(let ([a 1] [b 2] [c 3])
    (rotate-efficiently a b c)
    (list a b c)); '(2 3 1)


