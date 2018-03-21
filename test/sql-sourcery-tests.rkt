#lang s-exp "../lib/sql-sourcery.rkt"

;; Database connection and structure definition
(sourcery-db "test.db")
(sourcery-struct student [(name STRING) (grade INTEGER) (failing BOOLEAN)])
(sourcery-struct prof [(name STRING)])

;; Using testing library
(require "sql-sourcery-test-lib.rkt")
(stest-conn "test.db")

;; Test environment setup
(define bobby (student-create "Bob Smith" 90 #false))
(define ben (prof-create "Ben"))

;; Check table validity
(check-true (stest-table? "student"))
(check-equal? (stest-fields "student") 4)
(check-equal? (stest-field "student" "sourcery_id") "INTEGER")
(check-equal? (stest-field "student" "name") "STRING")
(check-equal? (stest-field "student" "grade") "INTEGER")
(check-equal? (stest-field "student" "failing") "BOOLEAN")
(check-equal? (stest-field "student" "eh") void)

;; structure creation
(check-equal? (length (stest-rows "student" "sourcery_id" "1")) 1)
(check-equal? (first (stest-rows "student" "sourcery_id" "1")) (list 1 "Bob Smith" 90 "FALSE"))
#;(check-exn (student-create 90 90 #false) "expected type STRING for name: got 90")

;; structure predicates
(check-true (prof? ben))
(check-true (student? bobby))
(check-false (prof? bobby))
(check-false (student? ben))
(check-false (prof? 1))

;; structure access
(check-equal? (student-name bobby) "Bob Smith")
(check-equal? (student-grade bobby) 90)
(check-equal? (student-failing bobby) #f)
#;(check-exn (student-name ben) "expected student, given:")

;; structure display
(check-equal? (displayln bobby) (void))
(check-equal? (println bobby) (void))
(check-equal? (writeln bobby) (void))

;; Clear table
(check-success (stest-clear-table "student"))

;; Delete testing database
(stest-teardown "test.db")

