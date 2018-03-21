#lang racket

(provide id->string
         first-failing)

(require syntax/parse)

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


