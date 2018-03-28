#lang racket

;; -----------------------------------------------------------------------
;; Library Provide
(provide

 ;; Basic Testing Functions
 check-equal?
 check-eq?
 check
 check-not-equal?
 check-pred
 check-exn
 exn:fail?
 check-true check-success ; Alternate name for check-true
 check-false
 
 ;; Test Running Library
 (rename-out [test-suite-auto-run test-suite])
 run-tests

 ;; Setup and Teardown Definition Helpers + Snytax Helpers
 action-compose
 thunk-beginify
 define-thunk-beginify
 sql-sourcery-tests
 
 ;; Display Checks
 displayln
 read-from-file
 gen-output-file

 ;; Allowed Mutation for testing library
 set!

 ;; Database Help
 stest-conn
 stest-clear-table
 stest-teardown
 stest-table?
 stest-fields
 stest-field
 stest-rows)


;; -----------------------------------------------------------------------
;; Internal Requirements
(require rackunit
         rackunit/text-ui
         (for-syntax syntax/parse)
         db)


;; -----------------------------------------------------------------------
;; Testing Utilities
(define (check-success x)
  (check-true x))

(define (action-compose . actions)
  (λ () (begin
          (map (λ (a) (a)) actions)
          (void))))

(define-syntax thunk-beginify
  (syntax-parser
    [(_ actions ...)
     #`(λ () (begin
               actions ...
               (void)))]))

(define-syntax define-thunk-beginify
  (syntax-parser
    [(_ name:id actions ...)
     #`(define name
         (λ () (begin
                 actions ...
                 (void))))]))

(define-syntax test-suite-auto-run
  (syntax-parser
    [(_ pieces ...)
     #'(add-to-tests (test-suite pieces ...))]))

(define-syntax sql-sourcery-tests
  (syntax-parser
    [(_) #'(begin
             (define unsuccessful-tests (run-tests (make-test-suite "SQLSourcery Tests" all-tests)))
             (void))]))

(define (read-from-file n path)
  (call-with-input-file path
    (lambda (in) (read-string n in))))

(define (gen-output-file path)
  (open-output-file path #:mode 'text #:exists 'replace))
               

;; -----------------------------------------------------------------------
;; Runtime Library
(define test-conn #f)
(define all-tests '())

(define (add-to-tests t)
  (set! all-tests (cons t all-tests)))

;; -----------------------------------------------------------------------
;; SQLSourcery Testing Functions

;; String -> Void
;; create a database connection
(define (stest-conn db-location)
  (set! test-conn
        (sqlite3-connect #:database db-location #:mode 'create)))

;; String -> Boolean
;; delete all records from a given table and reset the autoincrement
(define (stest-clear-table table-name)
  (begin
    (query-exec test-conn
                (format "DELETE FROM ~a" table-name))
    (query-exec test-conn
                (format "UPDATE SQLITE_SEQUENCE SET SEQ=0 WHERE NAME='~a'" table-name))
    #true))
 
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


