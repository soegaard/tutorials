#lang racket
(require (for-syntax syntax/parse racket/format))

(begin-for-syntax
  (define module-level-variables '())
  ; each internal definition contexts is given an index
  (define intdefs (make-hasheq))
  ; each index is associated to a list of declared names
  (define local-variables (make-hash))
  
  (define (new-intdef? intdef)     (hash-has-key? intdefs intdef))
  (define (index-of-intdef intdef) (hash-ref! intdefs intdef (hash-count intdefs)))
  (define (add-local-var! index var)
    (hash-update! local-variables index (λ (old-vars) (cons var old-vars)) '())))
 
(define-syntax (vars stx) 
  (with-syntax ([vs module-level-variables])
    #''vs))

(begin-for-syntax
  (define refresh-identifier (compose syntax-local-introduce syntax-local-get-shadower)))

(define-syntax (declare stx)
  (syntax-parse stx
    [(_ id)
     (define var (syntax->datum #'id))
     (define ctx (syntax-local-context))
     (cond 
       [(eq? ctx 'module)
        (set! module-level-variables (cons var module-level-variables))
        #'(begin)]
       [(list? ctx) ; internal definition context
        (define old-scope? (new-intdef? ctx))
        (define index (index-of-intdef ctx))
        (add-local-var! index var)
        (cond [old-scope? #'(begin)]
              [else       (with-syntax ([vars     (refresh-identifier #'vars)]
                                        [old-vars (syntax-local-get-shadower #'vars)]
                                        [index    index]
                                        [....     #'(... ...)])
                            #'(define-syntax (vars st)
                                (define locals (hash-ref local-variables index))
                                (define others (local-expand #'(old-vars) (syntax-local-context) #f))
                                ; others = (quote (var ...)) so we skip the quote here: 
                                (with-syntax ([(local  ....) locals] 
                                              [(_ (other ....)) others])
                                  #`'(local .... other ....))))])]
       [else (error 'declare 
                    (~a "declarations are only allowed at the module level "
                        "and in internal definition contexts"))])]))

(declare a)
(display (vars))
(let ()
  (display (vars))
  (declare x)
  (display (vars))
  (let ()
    (display (vars))
    (declare s)
    (declare t)
    (display (vars)))
  (declare y)
  (display (vars)))
(declare b)

; displays: (b a)(y x b a)(y x b a)(t s y x b a)(t s y x b a)(y x b a)