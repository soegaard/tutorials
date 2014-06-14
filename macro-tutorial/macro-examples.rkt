#lang racket
(require (for-syntax syntax/parse racket/match))
;;;
;;; MACRO TUTORIAL EXAMPLES
;;;

;;; syntax-local-context
(displayln "--- syntax-local-context ---")

(begin-for-syntax
  (define (expansion-context)
    (match (syntax-local-context)
      ['expression   'expression]
      ['top-level    'top-level]
      ['module       'module]
      ['module-begin 'module-begin]
      [(cons _ __)   'internal-definition-context])))

(define-syntax (use-ec stx)
  ; expands into a symbol indicating the expansion context
  #`'#,(expansion-context))

(use-ec)              ; module
(let () (use-ec))     ; internal-definition-context


;;; syntax-local-lift-expression
(displayln "--- syntax-local-context ---")

(define-syntax (m stx)
  (syntax-parse stx
    [(_ n) (syntax-local-lift-expression #'(display n))]))

(m 1)
(let ()
  (display 2)
  (m 3)
  (display 4)
  (m 5))
(m 6)
; This prints 135246



