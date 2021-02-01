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
; cf. https://docs.racket-lang.org/reference/stx-patterns.html

(let ([a 1])
    (rotate-efficiently a)
    (list a)); '(1)

(let ([a 1] [b 2])
    (rotate-efficiently a b)
    (list a b)); '(2 1)

(let ([a 1] [b 2] [c 3])
    (rotate-efficiently a b c)
    (list a b c)); '(2 3 1)


;; 16.1.5 Identifier Macros

; An identifier macro is a pattern-matching macro that works when used by itself without parentheses. For example, we can define val as an identifier macro that expands to (get-val), so (+ val 3) would expand to (+ (get-val) 3).
(define-syntax val
  (lambda (stx)
    (syntax-case stx ()
      [val (identifier? (syntax val)) (syntax (get-val))])))
(define-values (get-val put-val!)
  (let ([private-val 0])
    (values (lambda () private-val)
            (lambda (v) (set! private-val v)))))

val
(+ val 3)
(put-val! 4)
val


;; 16.1.6 set! Transformers

; To invoke the macro when val is used with set!, we create an assignment transformer with make-set!-transformer.
; We must also declare set! as a literal in the syntax-case literal list.

(define-syntax val2
  (make-set!-transformer
   (lambda (stx)
     (syntax-case stx (set!)
       [val2 (identifier? (syntax val2)) (syntax (get-val))]
       [(set! val2 e) (syntax (put-val! e))]))))


;; 16.1.7 Macro-Generating Macros

(define-syntax-rule (define-get/put-id id get put!)
  (define-syntax id
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [id (identifier? (syntax id)) (syntax (get))]
         [(set! id e) (syntax (put! e))])))))

(define-get/put-id val3 get-val put-val!)
val3
val2
val
(set! val3 11)
val3
val2
val


;; 16.1.8 Extended Example: Call-by-Reference Functions

; For example, if define-cbr is like define except that it defines a call-by-reference function, then
;(define-cbr (f a b)
;  (swap a b))
;
;(let ([x 1] [y 2])
;  (f x y)
;  (list x y))
; produces (2 1).

(define-syntax-rule (define-cbr (id arg ...) body)
  (begin
    (define-syntax id
      (syntax-rules ()
        [(id actual (... ...))
         (do-f (lambda () actual)
               (... ...)
               (lambda (v)
                 (set! actual v))
               (... ...))]))
    (define-for-cbr do-f (arg ...)
      () ; explained below...
      body)))

; As it turns out, lexical scope gives us a way around this problem.
; The trick is to iterate expansions of define-for-cbr once for each argument in the function,
; and that’s why define-for-cbr starts with an apparently useless () after the argument list. 
(define-syntax define-for-cbr
  (syntax-rules ()
    [(define-for-cbr do-f (id0 id ...); ex: 'id0' => 'a', 'id' => 'b'
       (gens ...) body); gens ... => (define-get/put!-id a get-a put-a!) ...
     (define-for-cbr do-f (id ...)
       (gens ... (id0 get put)) body)]
    [(define-for-cbr do-f ()
       ((id get put) ...) body)
     (define (do-f get ... put ...)
       (define-get/put-id id get put) ...
       body)]))
; Step-by-step, expansion proceeds as follows:
;  (define-for-cbr do-f (a b)
;    () (swap a b))
;  => (define-for-cbr do-f (b)
;       ([a get_1 put_1]) (swap a b))
;  => (define-for-cbr do-f ()
;       ([a get_1 put_1] [b get_2 put_2]) (swap a b))
;  => (define (do-f get_1 get_2 put_1 put_2)
;       (define-get/put-id a get_1 put_1)
;       (define-get/put-id b get_2 put_2)
;       (swap a b))

(define-for-cbr do-swap-cbr (a b)
  () (swap a b))

(define-cbr (swap-cbr a b)
  (swap a b))

(let ([x 1] [y 2])
  (swap-cbr x y)
  (list x y))
