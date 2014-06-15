#lang racket
;;; syntax-local-context

; this example shows
;   1) the first element of a context identifies the internal definition context
;   2) the length of the context list is *not* related to the nesting depth

(require (for-syntax syntax/parse racket/format))

(begin-for-syntax
  ; an association list from "first elements of syntax-local-context return values"
  ;                     to   names (symbols)
  (define scopes '()))

; (name-scope name)
;   Use in an internal definition context.
;   It associates the internal definition context with a name
(define-syntax (name-scope stx)
  (syntax-parse stx
    [(_ n)
     (define name (syntax->datum #'n))
     (define ctx (syntax-local-context))
     (cond
       [(list? ctx) ; internal definition context
        (set! scopes (cons (cons (car ctx) name) scopes))
        #'(void)]
       [else (error 'name-scope "not in an internal definition context")])]))

; (scope)
;   Use in an internal definition context.
;   Displays the name of the current internal definition context.
(define-syntax (scope stx)
  (define ctx (syntax-local-context))
  (display (length ctx))
  (define (name ctx-elm) (cond [(assq ctx-elm scopes) => cdr]
                               [else #f]))
  (define names (map name ctx))
  (with-syntax ([names names])
    #'(display 'names)))

(let ()
  (name-scope a)
  (scope)                 ; displays (a)
  (let ()
    (scope)               ; displays (#f)
    (name-scope b)
    (scope))              ; displays (b)
  (scope))                ; displays (a)
