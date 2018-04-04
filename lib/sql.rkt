#lang racket

(provide get-created-id
         get-row
         get-val-from-row
         create-set-values-list
         
         (for-syntax table-creation-string
                     gen-accessor-query-format))

(require db
         "sourcery-connection.rkt"
         "utils.rkt"
         "type-support.rkt"
         (for-syntax racket
                     "utils.rkt"))


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
          (quote-field (id->string struct-name))
          (string-append "sourcery_id INTEGER PRIMARY KEY AUTOINCREMENT, "
                         (comma-separate
                          (map (λ (ft-pair)
                                 (format "~a ~a"
                                         (quote-field (id->string (first ft-pair)))
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
  (get-val-from-row (query-rows (get-sourcery-connection)
                                (string-append
                                 "SELECT MAX(sourcery_id) FROM "
                                 (quote-field table-name)))
                    0))


;; Syntax [List-of SupportedStructType] -> [List-of String]
;; create a set statement for the given fields and args
(define (create-set-values-list fields args)
  (begin
    (map (λ (f a) (format "~a = ~a" f (format-sql-type a)))
         (map (λ (f) (quote-field (id->string f))) (syntax->list fields))
         args)))

;; -----------------------------------------------------------------------
;; SQL Rows Parsing

;; String Number -> [Maybe [List-of SupportedStructType]]
(define (get-row table id)
  (let [(rows (query-rows (get-sourcery-connection)
                          (format "SELECT * FROM ~a WHERE sourcery_id = ~a"
                                  (quote-field table) id)))]
    (if (= 1 (length rows))
        (rest (vector->list (first rows)))
        #false)))

;; [List-of Vector] Integer -> Any
;; get the value from a single row at the given index position
(define (get-val-from-row row idx)
  (if (= 1 (length row))
      (if (< idx (length (first (rows->lists row))))
          (list-ref (first (rows->lists row)) idx)
          (error 'get-val-from-row "index out of bounds in row"))
      (error 'get-val-from-row (format "~a rows given, expected 1 row" (length row)))))
