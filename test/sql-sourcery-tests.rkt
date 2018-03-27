#lang s-exp "../lib/sql-sourcery.rkt"

;; Database connection and structure definition
(sourcery-db "test.db")
(sourcery-struct student [(name STRING) (grade INTEGER) (failing BOOLEAN)])
(sourcery-struct prof [(name STRING)])

;; Using testing library
(require "sql-sourcery-test-lib.rkt")
(stest-conn "test.db")


(define bob #false)
(define ben #false)
(define steve #false)

;; Setup Functions
(define-thunk-beginify su-create-bob
  (set! bob (student-create "Bob Smith" 90 #false)))

(define su-create-ben
  (thunk-beginify
   (set! ben (prof-create "Ben"))))

(define su-create-steve
  (thunk-beginify
   (set! steve (student-create "Steve Steve" 20 #true))))

(define su-create-all (action-compose su-create-bob su-create-ben su-create-steve))

;; Teardown Functions
(define td-clear-all-tables
  (thunk-beginify
   (stest-clear-table "student")
   (stest-clear-table "prof")))

(define td-unset-vars
  (thunk-beginify

;; Test environment setup
(su-create-all)
(sourcery-load student)

(student? bob)
(sourcery-delete bob)
(student? bob)
(sourcery-filter-delete (λ (x) #f) (sourcery-load student))
;(sourcery-filter-delete (λ (x) #t) (list bob 2))
bob
steve
;(sourcery-filter-delete (λ (x) #t) 2)

(define x
  (test-suite
   "An example suite"
   #:before (lambda () (displayln "Before"))
   #:after  (lambda () (displayln "After"))
   (test-case
    "An example test"
    (check-eq? 1 1))
   (test-suite "A nested test suite"
               (test-case "Another test"
                          (check < 1 2)))))

(run-tests x)

#;(
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
   (check-exn exn:fail? (λ () (student-create 90 90 #false))
              "expected type STRING for name: got 90")

   ;; structure predicates
   (check-true (prof? ben))
   (check-true (student? bob))
   (check-false (prof? bob))
   (check-false (student? ben))
   (check-false (prof? 1))

   ;; structure access
   (check-equal? (student-name bob) "Bob Smith")
   (check-equal? (student-grade bob) 90)
   (check-equal? (student-failing bob) #f)
   (check-exn exn:fail? (λ () (student-name ben)) "expected student, given:")

   ;; structure update
   (define bobby (student-update bob "Bobby Smith" 91 #true))
   (check-equal? bobby bob)
   (check-equal? (student-name bob) "Bobby Smith")
   (check-equal? (student-grade bob) 91)
   (check-equal? (student-failing bob) #true)

   ;; structure display
   (check-equal? (displayln bob) (void))
   (check-equal? (println bob) (void))
   (check-equal? (writeln bob) (void))

   ;; structure deletion
   (check-true (student? bob))
   (check-true (sourcery-delete bob))
   (check-exn exn:fail? (λ () (sourcery-delete 1)) "Expected sourcery-struct, got: 1")
   (check-exn exn:fail? (λ () (student-name bob)) "sourcery reference does not exist")
   (check-false (student? bob))
   (check-true (sourcery-delete bob))

   ;; Clear table
   (check-success (stest-clear-table "student"))
   )

;; Delete testing database
(stest-teardown "test.db")

