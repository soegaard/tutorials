#lang racket
;;; syntax-local-introduce

(define-syntax (m stx) 
  (with-syntax ([a #'a])
    #'(define a 43)))
(m)
; a ; unbound  

(define-syntax (n stx) 
    (with-syntax ([b (syntax-local-introduce #'b)])
      #'(define b 42)))
(n)
b



