#lang racket

;; -----------------------------------------------------------------------
;; Library Provide
(provide

 ;; Basic Testing Functions
 check-equal?
 check-not-equal?
 check-pred
 check-exn
 check-true
 check-false
 check-success ; Alternate name for check-true

 ;; Needed for testing only
 void
 display
 print
 write

 ;; String -> Void
 ;; create a database connection
 stest-conn
 
 ;; String -> Boolean
 ;; delete all records from a given table and reset the autoincrement
 stest-clear-table
 
 ;; String -> Void
 ;; delete the database
 stest-teardown
 
 ;; String -> Boolean
 ;; verify a table exists in the database
 stest-table?

 ;; String -> Integer
 ;; determine how many fields the table has
 stest-fields

 ;; String String -> String
 ;; verify field exists and return a string representing its type
 stest-field

 ;; String String SourceryType -> Number
 ;; Given a table name, field, and field value, check how many rows in the table exist
 ;; with the matching field value
 stest-rows)


;; -----------------------------------------------------------------------
;; Internal Requirements
(require rackunit
         db)


;; -----------------------------------------------------------------------
;; Testing Utilities
(define (check-success x)
  (check-true x))


;; -----------------------------------------------------------------------
;; Runtime Library
(define test-conn #f)


;; -----------------------------------------------------------------------
;; SQLSourcery Testing Functions


(define (stest-conn db-location)
  (set! test-conn
        (sqlite3-connect #:database db-location #:mode 'create)))


(define (stest-clear-table table-name)
  (begin
    (query-exec test-conn
                (format "DELETE FROM ~a" table-name))
    (query-exec test-conn
                (format "UPDATE SQLITE_SEQUENCE SET SEQ=0 WHERE NAME='~a'" table-name))
    #true))


(define (stest-teardown db-location)
  (delete-file db-location))


(define (stest-table? table-name)
  (= 1
     (length (query-rows test-conn
                         (format "SELECT name FROM sqlite_master WHERE type='table' AND name='~a'"
                                 table-name)))))

(define (stest-fields table-name)
  (length (query-rows test-conn
                      (format "SELECT * FROM pragma_table_info('~a')" table-name))))

  

(define (stest-field table-name field-name)
  (let
      [(table-info (query-rows test-conn
                               (format "SELECT * FROM pragma_table_info('~a') WHERE name='~a'"
                                       table-name field-name)))]
    (if (= (length table-info) 1)
        (third (vector->list (first table-info)))
        void)))

(define (stest-rows table-name field-name field-value)
  (map vector->list
       (query-rows test-conn
                   (format "SELECT * FROM ~a WHERE ~a = ~a" table-name field-name field-value)))) 





