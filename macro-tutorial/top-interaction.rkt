#lang racket

(begin-for-syntax (define a 42))
(define-syntax (m stx) (with-syntax ([v a]) #''v))
(define-syntax (n stx) (displayln 'n) (set! a 43) #'(begin))
(n)
(m) ; => 43

; Now in DrRacket:
; > (m)
; 42

; Note that it gives 42 and not 43.
; The assignment (set! a 43) happens during syntax expansion,
; so the effect is gone, when the repl starts.

; We need to reinstate the value the first time an expression
; is evaluated in the repl. #%top-interaction to the rescue...
; The repl wraps each expression in #%top-interaction, so
; we just need to redefined #%top-interaction.

(begin-for-syntax
  (define first-interaction? #t))

(define-syntax (#%top-interaction stx)
  (syntax-case stx ()
    [(_ . expr)
     (when first-interaction?
       (set! a 43)
       (set! first-interaction? #f))
     #'expr]))
