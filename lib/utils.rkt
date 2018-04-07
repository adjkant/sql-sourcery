#lang racket

(provide rows->lists
         comma-separate
         id->string
         quote-field
         unquote-field
         field-valid?)


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

;; -----------------------------------------------------------------------
;; Racket Struct Names and SQL Table Field Names

;; String -> String
;; translate a racket field name to a sql field name
(define (quote-field f)
  (string-append "\"" f "\""))


;; String -> String
;; translate a sql field name to a racket field name
(define (unquote-field f)
  (substring f 1 (- (length f) 2)))

(define (field-valid? f)
  (if (and (> (string-length f) 1) (string=? (substring f 0 2) "__"))
      (error 'sourcery-struct (format "Invalid field name: ~a" f))
      (void)))