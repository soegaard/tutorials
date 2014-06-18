#lang racket
;;; free-identifier=? bound-identifier=?

; Ryan  http://lists.racket-lang.org/users/archive/2011-April/045087.html

;You might have a notion of the scoping rules of lambda and let, 
;but free-identifier=? et al don't; they can only tell you what the macro 
;expander has discovered about the program's binding structure.
;
;Before any expansion, the macro expander hasn't made any discoveries, so 
;all xs look alike.
;
;After expansion, you get something that looks like this:
;   (lambda (x y) (let-values ([(x) '2]) x)
;so you'll have to rewrite your pattern, but now you can ask questions 
;using free-identifier=?. (IIRC, free-identifier=? and bound-identifier=? 
;are equivalent on fully-expanded programs.)
;
;During macro expansion, free-identifier=? and bound-identifier=? compare 
;identifiers based on the discoveries available at that point in time.
;
;(free-identifier=? x y) means if these two identifiers were turned into 
;references right now, would they refer to the same binding. It's how 
;cond, for example, recognizes the else keyword: does the identifier the 
;macro gets refer to the same binding as a known good reference to else.
;
;(bound-identifier=? x y) means if one of the identifiers were turned 
;into a binding occurrence and the other were turned into a reference in 
;its scope, eg (lambda (x) y), would the second refer to the first. This 
;is usually only used when you're implementing a new binding form to 
;check for duplicates, etc, or for really odd macros that act kind of 
;like binding forms but don't use Racket's core binding forms to do it.
     

; Ryan:

; http://stackoverflow.com/questions/6801482/difference-between-free-identifier-and-bound-identifier

(define-syntax (compare-with-x stx)
  (syntax-case stx ()
    [(_ y)
     (with-syntax ([free=?  (free-identifier=?  #'x #'y )]
                   [bound=? (bound-identifier=? #'x #'y )])
       #'(list free=? bound=?))]))

(compare-with-x x)  ;; => '(#t #f)
(let ([x 42])
  (compare-with-x x))  ;; => '(#f #f)

; Operational explanation:
;     The expander adds a mark to x before calling the macro transformer
;     for compare-with-x. This means that y is bound to #'x with a mark on it.
; Conceptual explanation:
;     The #'x in the body of compare-with-x is ... Hmmm.


; To show that the mark is the cause, we can add remove the mark with
; syntax-local-introduce.

(define-syntax (compare-with-x2 stx)
  (syntax-case stx ()
    [(_ y)
     (with-syntax ([free=?  (free-identifier=?  #'x (syntax-local-introduce #'y))]
                   [bound=? (bound-identifier=? #'x (syntax-local-introduce #'y))])
       #'(list free=? bound=?))]))

(compare-with-x2 x)     ;; => '(#t #t)

(let ([x 42])
  (compare-with-x2 x))  ;; => '(#f #f)
      

;;; From R6RS:
; Id1 and id2 must be identifiers. The procedure bound-identifier=? returns #t 
; if a binding for one would capture a reference to the other in the output of 
; the transformer, assuming that the reference appears within the scope of the 
; binding, and #f otherwise.

; We can demonstrate this by introducing a binding for either x or y 
; in the output:

(define-syntax (compare-with-x3 stx)
  (syntax-case stx ()
    [(_ y)
     #'(let ([x 42])    ; this x binds
         (list x y))])) ; this x, but the x which y is bound to is not
  
; (compare-with-x3 x)  ;; x: unbound identifier in module in: x

(define-syntax (compare-with-x4 stx)
  (syntax-case stx ()
    [(_ y)
     #'(let ([y 42])    ; this y binds
         (list x y))])) ; the original x, but not the x in here  <= error in this line

; (compare-with-x4 x)









