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
; The 

#;(define-syntax (#%top-interaction stx)
  (syntax-case stx ()
    [(_ . expr)
     (set! a 43)
     #'expr]))
