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
     
