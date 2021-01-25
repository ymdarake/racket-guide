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

