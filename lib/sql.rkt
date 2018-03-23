#lang racket

(provide get-created-id
         get-val-from-row
         create-set-values-list
         
         (for-syntax table-creation-string
                     gen-accessor-query-format))

(require db
         "sourcery-connection.rkt"
         "utils.rkt"
         "type-support.rkt"
         (for-syntax racket
                     "utils.rkt"
                     "utils-phase-1.rkt"))


;; -----------------------------------------------------------------------
;; SQL Query Generation

;; String String -> String
;; generate a format string for select by sourcery_id for the given table/field
(define-for-syntax (gen-accessor-query-format struct-name struct-field)
  (format "SELECT ~a FROM ~a WHERE sourcery_id = ~~a" struct-name struct-field))

;; Syntax Syntax Syntax -> String
;; Given the name of the structure and the fields/types, create a table creation string
(define-for-syntax (table-creation-string struct-name fields types)
  (format "CREATE TABLE IF NOT EXISTS ~a (~a)"
          (id->string struct-name)
          (string-append "sourcery_id INTEGER PRIMARY KEY AUTOINCREMENT, "
                         (comma-separate
                          (map (λ (ft-pair)
                                 (format "~a ~a"
                                         (id->string (first ft-pair))
                                         (id->string (second ft-pair))))
                               (map list (syntax->list fields) (syntax->list types)))))))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; SQLSourcery Runtime Library
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; SQL Query Utilities

;; String -> Integer
;; Get ID of last created row in given table
(define (get-created-id table-name)
  (get-val-from-row (query-rows sourcery-connection
                                (string-append
                                 "SELECT MAX(sourcery_id) FROM "
                                 table-name))
                    0))


;; Syntax Syntax -> [List-of String]
;; create a set statement for the given fields and args
(define (create-set-values-list fields args)
  (begin
    (map (λ (f a) (format "~a = ~a" f (format-sql-type a)))
         (map id->string (syntax->list fields))
         (syntax->list args))))

;; -----------------------------------------------------------------------
;; SQL Rows Parsing

;; [List-of Vector] Integer -> Any
;; get the value from a single row at the given index position
(define (get-val-from-row row idx)
  (if (= 1 (length row))
      (if (< idx (length (first (rows->lists row))))
          (list-ref (first (rows->lists row)) idx)
          (error 'get-val-from-row "index out of bounds in row"))
      (error 'get-val-from-row (format "~a rows given, expected 1 row" (length row)))))