#lang racket

;; 16.2.1 Syntax Objects

; The input and output of a macro transformer (i.e., source and replacement forms) are represented as syntax objects.
; A syntax object contains symbols, lists, and constant values (such as numbers) that essentially correspond to the quoted form of the expression.
; For example, a representation of the expression (+ 1 2) contains the symbol '+ and the numbers 1 and 2, all in a list.

; In addition to this quoted content, a syntax object associates source-location and lexical-binding information with each part of the form.
; The source-location information is used when reporting syntax errors (for example),
; and the lexical-binding information allows the macro system to maintain lexical scope.
;! To accommodate this extra information,
;! the representation of the expression (+ 1 2) is not merely '(+ 1 2),
;! but a packaging of '(+ 1 2) into a syntax object.

(syntax (+ 1 2))
#'(+ 1 2)
; > #<syntax:eval:1:0 (+ 1 2)>



(identifier? #'car)
; > #t

(identifier? #'(+ 1 2))
; > #f

(free-identifier=? #'car #'cdr)
; > #f

(free-identifier=? #'car #'car)
; > #t

(require (only-in racket/base [car also-car]))
(free-identifier=? #'car #'also-car)
; > #t


;To see the lists, symbols, numbers, etc. within a syntax object, use syntax->datum:
(syntax->datum #'(+ 1 2))
; > '(+ 1 2)

; The syntax-e function is similar to syntax->datum,
; but it unwraps a single layer of source-location and lexical-context information,
; leaving sub-forms that have their own information wrapped as syntax objects:

(syntax-e #'(+ 1 2))
; > '(#<syntax:eval:1:0 +> #<syntax:eval:1:0 1> #<syntax:eval:1:0 2>)

; The syntax-e function always leaves syntax-object wrappers around sub-forms
; that are represented via symbols, numbers, and other literal values.
; The only time it unwraps extra sub-forms is when unwrapping a pair,
; in which case the cdr of the pair may be recursively unwrapped, depending on how the syntax object was constructed.


;; 16.2.2 Macro Transformer Procedures

; Any procedure of one argument can be a macro transformer.
; As it turns out, the syntax-rules form is a macro that expands to a procedure form.
; For example, if you evaluate a syntax-rules form directly (instead of placing on the right-hand of a define-syntax form), the result is a procedure:

(syntax-rules () [(nothing) something])
; > #<procedure>

; Instead of using syntax-rules, you can write your own macro transformer procedure directly using lambda.
; The argument to the procedure is a syntax object
; that represents the source form, and the result of the procedure must be a syntax object that represents the replacement form:

(define-syntax self-as-string
  (lambda (stx)
    (datum->syntax stx
                   (format "~s" (syntax->datum stx)))))
(self-as-string (+ 1 2))
; > "(self-as-string (+ 1 2))"

;! (syntax->datum (syntax (self-as-string (+ 1 2))))
;! > '(self-as-string (+ 1 2))
;! (format "~s" '(self-as-string (+ 1 2)))
;! > "(self-as-string (+ 1 2))"

; The define-syntax form supports the same shortcut syntax for functions as define,
; so that the following self-as-string definition is equivalent to the one that uses lambda explicitly:
(define-syntax (self-as-string-2 stx)
  (datum->syntax stx
                 (format "~s" (syntax->datum stx))))
(self-as-string-2 (+ 1 2))
; > "(self-as-string (+ 1 2))"


; self study: Doing this by using syntax-rules:
(define-syntax self-as-string-rules
  (syntax-rules ()
    [(self-as-string-rules) (format "~s" "self-as-string-rules")]
    [(self-as-string-rules exp) (format "~s" '(self-as-string-rules exp))]
    [(self-as-string-rules exp ...) (format "~s" '(self-as-string-rules exp ...))]))

(self-as-string-rules (+ 1 2))
(self-as-string-rules (+ 1 2) 3 4)
; The procedure produced by syntax-rules raises a syntax error if its argument corresponds to a use of the identifier by itself, which is why syntax-rules does not implement an identifier macro.
; self-as-string-rules

