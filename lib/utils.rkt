#lang racket

(provide rows->lists
         comma-separate
         id->string)


;; Id -> String
;; Convert an identifier to a string
(define (id->string id)
  (symbol->string `,(syntax->datum id)))

;; [List-of Vector] -> [List-of [List-of Any]]
;; turn a list of vectors into a list of lists
(define (rows->lists rows)
  (map vector->list rows))

;; [List-of X] -> String
;; Join a list of items as their string value with comma separation at compile time
(define (comma-separate l)
  (let [(comma-list (foldr
                     (λ (col-def so-far)
                       (format "~a, ~a"
                               col-def
                               so-far))
                     ""
                     l))]
    (if (> (string-length comma-list) 2)
        (substring comma-list
                   0 (- (string-length comma-list) 2))
        "")))