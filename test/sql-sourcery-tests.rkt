#lang s-exp "../lib/lang.rkt"

;; Database connection and structure definition
(sourcery-db "test.db")
(sourcery-struct user [(name STRING) (grade INTEGER) (failing BOOLEAN)])

;; Using testing library
(require "sql-sourcery-test-lib.rkt")
(stest-conn "test.db")

;; Check table validity
(check-true (stest-table? "user"))
(check-equal? (stest-fields "user") 4)
(check-equal? (stest-field "user" "sourcery_id") "INTEGER")
(check-equal? (stest-field "user" "name") "STRING")
(check-equal? (stest-field "user" "grade") "INTEGER")
(check-equal? (stest-field "user" "failing") "BOOLEAN")
(check-equal? (stest-field "user" "eh") void)

;; structure creation
(define bobby (user-create "Bob Jones" 90 #false))
(check-equal? (length (stest-rows "user" "sourcery_id" "1")) 1)
(check-equal? (first (stest-rows "user" "sourcery_id" "1")) (list 1 "Bob Jones" 90 "FALSE"))

;; structure display
(check-equal? (display bobby) (void))

;; Clear table
(check-success (stest-clear-table "user"))

;; Delete testing database
(stest-teardown "test.db")

