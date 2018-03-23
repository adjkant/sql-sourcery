#lang racket

(provide id->string
         first-failing
         syntax-list->begin)

(require syntax/parse
         (for-template racket))

;; Id -> String
;; Convert an identifier to a string
(define (id->string id)
  (symbol->string `,(syntax->datum id)))  

;; [X -> Boolean] [X -> Y] [List-of X] -> [Maybe Y]
;; return the first item in the list to fail the predicate, translated by the given function
;; return false if no item fails the predicate
(define (first-failing proc return-form lst)
  (cond [(empty? lst) #false]
        [(cons? lst)
         (if (proc (first lst))
             (first-failing proc return-form (rest lst))
             (return-form (first lst)))]))

;; [List-of Syntax] -> Syntax
;; combine a list of syntax into a single begin syntax
(define (syntax-list->begin syntaxes)
  (foldl
   (λ (stx stx-so-far)
     (syntax-parse stx-so-far
       [((~literal begin) items ...)
        #`(begin items ... #,stx)]))
   #`(begin)
   syntaxes))

