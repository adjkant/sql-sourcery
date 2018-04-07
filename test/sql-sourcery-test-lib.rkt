#lang racket

;; -----------------------------------------------------------------------
;; Library Provide
(provide

 ;; Sourcery ID Field Name
 SOURCERY_ID_FIELD_NAME

 ;; Alternate name for check-true
 check-success
 
 ;; Display Checks
 read-from-file
 gen-output-file

 ;; Checking compile-time errors
 compile
 syntax

 ;; Database Testing Helper Function
 stest-conn
 stest-teardown
 stest-table?
 stest-fields
 stest-field
 stest-rows)

;; -----------------------------------------------------------------------
;; Internal Requirements
(require rackunit
         db
         "../lib/sourcery-connection.rkt")


;; -----------------------------------------------------------------------
;; Testing Utilities
(define (check-success x)
  (check-true x))

(define (read-from-file n path)
  (call-with-input-file path
    (lambda (in) (read-string n in))))

(define (gen-output-file path)
  (open-output-file path #:mode 'text #:exists 'replace))

;; -----------------------------------------------------------------------
;; SQLSourcery Testing Functions
(define test-conn #f)

;; String -> Void
;; create a database connection
(define (stest-conn db-location)
  (set! test-conn
        (sqlite3-connect #:database db-location #:mode 'create)))
 
;; String -> Void
;; delete the database
(define (stest-teardown db-location)
  (delete-file db-location))

;; String -> Boolean
;; verify a table exists in the database
(define (stest-table? table-name)
  (= 1
     (length (query-rows test-conn
                         (format "SELECT name FROM sqlite_master WHERE type='table' AND name='~a'"
                                 table-name)))))

;; String -> Integer
;; determine how many fields the table has
(define (stest-fields table-name)
  (length (query-rows test-conn
                      (format "SELECT * FROM pragma_table_info('~a')" table-name))))

;; String String -> String
;; verify field exists and return a string representing its type
(define (stest-field table-name field-name)
  (let
      [(table-info (query-rows test-conn
                               (format "SELECT * FROM pragma_table_info('~a') WHERE name='~a'"
                                       table-name field-name)))]
    (if (= (length table-info) 1)
        (third (vector->list (first table-info)))
        void)))

;; String String SourceryType -> Number
;; Given a table name, field, and field value, check how many rows in the table exist
;; with the matching field value
(define (stest-rows table-name field-name field-value)
  (map vector->list
       (query-rows test-conn
                   (format "SELECT * FROM ~a WHERE ~a = ~a" table-name field-name field-value)))) 


