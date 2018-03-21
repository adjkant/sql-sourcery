#lang racket

(provide
 TYPES
 get-type-info
 comma-separate)

;; [Listof SQLSourceryTypeInfo]
(define TYPES
  (list (list "INTEGER"
              integer?
              identity
              identity)
        
        (list "STRING"
              string?
              (λ (s) (format "\"~a\"" s))
              identity)
        
        (list "BOOLEAN"
              boolean?
              (λ (b) (if b "\"TRUE\"" "\"FALSE\""))
              (λ (b) (if (string=? "TRUE" b) #t #f)))))

;; String -> SQLSourceryTypeInfo
(define (get-type-info name)
  (let [(type (filter (λ (ti) (string=? (first ti) name)) TYPES))]
    (if (= 1 (length type))
        (first type)
        (error 'sourcery-struct (format "Invalid type: ~a" name)))))

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
    (substring comma-list
               0 (- (string-length comma-list) 2))))