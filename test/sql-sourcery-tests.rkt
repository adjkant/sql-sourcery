#lang racket

(require "../lib/sql-sourcery.rkt")

;; Database connection and structure definition
(sourcery-db "test.db")
(sourcery-struct student [(name STRING) (grade INTEGER) (failing BOOLEAN)])
(sourcery-struct prof [(name STRING)])

;; Using testing library with test connection
(require "sql-sourcery-test-lib.rkt")
(stest-conn "test.db")

;; Defile Display/Print/Write files
(define display-file (gen-output-file "outputs/display.txt"))
(define dead-ref-display-file (gen-output-file "outputs/display-dead-ref.txt"))

;; -----------------------------------------------------------------------
;; Results, Setup, and Teardown Library

;; Results
(declare-test-vars bob ben steve bobby sourcery-load-results)

;; Setup Functions
(define-action su-create-bob
  (set-test-var! bob (student-create "Bob Smith" (* 45 2) #false)))

(define-action su-create-ben
  (set-test-var! ben (prof-create "Ben")))

(define-action su-create-steve
  (set-test-var! steve (student-create "Steve Steve" 20 #true)))

(define-action su-update-bob-create-bobby
  (set-test-var! bobby (student-update bob "Bobby Smith" (+ (student-grade bob) 1) #true)))

(define-action su-update-while-create-joe-john
  (student-update (student-create "Temp Student" -1 #false)
                  "Joe"
                  (student-grade (student-create "John" 100 #f))
                  #t))

(define-action su-create-joe-delete
  (sourcery-delete (student-create "No Name" (student-grade (student-create "Joe" 1 #true)) #false)))

(define-action printer-bob-display
  (displayln bob display-file))

(define-action printer-bob-dead-ref
  (displayln bob dead-ref-display-file))

(define-action su-delete-bob
  (sourcery-delete bob))

(define-composed-action su-create-all [su-create-bob su-create-ben su-create-steve])

(define-action su-load-students
  (set-test-var! sourcery-load-results (sourcery-load student)))

(define-action su-none)

;; Teardown Functions
(define-action td-clear-all-tables
  (clear-sourcery-structs student prof))

(define-action td-unset-vars
  (clear-test-vars! bob ben steve bobby sourcery-load-results))

(define-composed-action td-complete [td-clear-all-tables td-unset-vars])

(define td-none su-none)

;; -----------------------------------------------------------------------
;; Language Tests

(sourcery-test-suite
 "Table Vailidity"
 #:before su-none
 #:after  td-none
 (check-true (stest-table? "student"))
 (check-equal? (stest-fields "student") 4)
 (check-equal? (stest-field "student" SOURCERY_ID_FIELD_NAME) "INTEGER")
 (check-equal? (stest-field "student" "name") "STRING")
 (check-equal? (stest-field "student" "grade") "INTEGER")
 (check-equal? (stest-field "student" "failing") "BOOLEAN")
 (check-equal? (stest-field "student" "eh") void))


(sourcery-test-suite
 "Structure Definition Errors"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail? (λ () (begin (sourcery-struct blank []) 1))
            "sourcery-struct must have at least one field"))

(sourcery-test-suite
 "Structure Definition Compile Time Errors"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail?
            (λ () (compile #'(begin 
                               (sourcery-struct student [(name STRING)])
                               (sourcery-struct student [(name STRING)])))))
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-struct))))
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-struct err [(name INVALIDTYPE)]))))
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-struct err_2 [(name)])))))

(sourcery-test-suite
 "Structure Creation"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 1)
 (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "1"))
               (list 1 "Bob Smith" 90 "FALSE"))
 (check-exn exn:fail? (λ () (student-create 90 90 #false))
            "expected type STRING for name: got 90"))

(sourcery-test-suite
 "Structure Create Compile Time Errors"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail?
            (λ () (compile #'(student-create))))
 (check-exn exn:fail?
            (λ () (compile #'(student-create "too many" 1 #t "arguments")))))

(sourcery-test-suite
 "Structure Predicates"
 #:before su-create-all
 #:after  td-complete
 (check-true (prof? ben))
 (check-true (student? bob))
 (check-false (prof? bob))
 (check-false (student? ben))
 (check-false (prof? 1)))

(sourcery-test-suite
 "Structure Access"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (student-name bob) "Bob Smith")
 (check-equal? (student-grade bob) 90)
 (check-equal? (student-failing bob) #f)
 (check-exn exn:fail? (λ () (student-name ben)) "expected student, given:"))

(sourcery-test-suite
 "Structure Access Compile Time Errors"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail?
            (λ () (compile #'(student-name))))
 (check-exn exn:fail?
            (λ () (compile #'(student-name bob "extra arguments")))))

(sourcery-test-suite
 "Structure Update"
 #:before (action-compose su-create-all su-update-bob-create-bobby)
 #:after  td-complete
 (check-equal? bobby bob)
 (check-equal? (student-name bob) "Bobby Smith")
 (check-equal? (student-grade bob) 91)
 (check-equal? (student-failing bob) #true))

(sourcery-test-suite
 "Structure Creation Within Update"
 #:before su-update-while-create-joe-john
 #:after  td-complete
 (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 1)
 (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) (list 1 "Joe" 100 "TRUE"))
 (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "2")) (list 2 "John" 100 "FALSE")))

(sourcery-test-suite
 "Structure Update Compile Time Errors"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail?
            (λ () (compile #'(student-update))))
 (check-exn exn:fail?
            (λ () (compile #'(student-update bob "too many" 1 #t "arguments")))))

(sourcery-test-suite
 "Structure Printer"
 #:before (action-compose su-create-all printer-bob-display)
 #:after  td-complete
 (check-equal? (read-from-file 50 "outputs/display.txt") "#<student: Bob Smith 90 #f>"))

(sourcery-test-suite
 "Structure Deletion"
 #:before (action-compose su-create-all su-update-bob-create-bobby su-delete-bob printer-bob-dead-ref)
 #:after  td-complete
 (check-exn exn:fail? (λ () (sourcery-delete 1)) "Expected sourcery-struct, got: 1")
 (check-exn exn:fail? (λ () (student-name bob)) "sourcery reference does not exist")
 (check-exn exn:fail? (λ () (student-name bobby)) "sourcery reference does not exist")
 (check-false (student? bob))
 (check-equal? (read-from-file 50 "outputs/display-dead-ref.txt") "#<student: dead-reference>")
 (check-true (sourcery-delete bob))
 (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 0))

(sourcery-test-suite
 "Creation Within Deletion"
 #:before su-create-joe-delete
 #:after  td-complete
 (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 1)
 (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) (list 1 "Joe" 1 "TRUE")))

(sourcery-test-suite
 "sourcery-load"
 #:before (action-compose su-create-all su-load-students)
 #:after  td-complete
 (check-equal? (length sourcery-load-results) 2)
 (check-equal? (student-name (first sourcery-load-results)) "Bob Smith")
 (check-equal? (student-name (second sourcery-load-results)) "Steve Steve"))

(sourcery-test-suite
 "Structure Load Compile Time Errors"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-load))))
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-load "too many" "arguments"))))
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-load not-a-real-struct)))))

(sourcery-test-suite
 "sourcery-filter-delete None"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (sourcery-filter-delete (λ (s) #t) (sourcery-load student))) 2)
 (check-equal? (student-name (first (sourcery-load student))) "Bob Smith")
 (check-equal? (student-name (second (sourcery-load student))) "Steve Steve"))

(sourcery-test-suite
 "sourcery-filter-delete All"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (sourcery-filter-delete (λ (s) #f) (sourcery-load student))) 0))

(sourcery-test-suite
 "sourcery-filter-delete Some"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (sourcery-filter-delete (λ (s) (string=? (student-name s) "Steve Steve"))
                                               (sourcery-load student)))
               1)
 (check-equal? (student-name (first (sourcery-load student))) "Steve Steve"))

(sourcery-test-suite
 "sourcery-dead-reference?"
 #:before (action-compose su-create-all su-delete-bob)
 #:after  td-complete
 (check-true  (sourcery-dead-reference? bob))
 (check-false (sourcery-dead-reference? steve))
 (check-exn exn:fail? (λ () (sourcery-dead-reference? 1)) "Expected sourcery-struct, got: 1"))

(sourcery-test-suite
 "Struct/sourcery-struct Creation with same names"
 #:before su-none
 #:after  td-none
 (check-exn exn:fail?
            (λ () (compile #'(begin
                               (struct a [])
                               (sourcery-struct a [(name STRING)])))))
 (check-exn exn:fail?
            (λ () (compile #'(begin
                               (sourcery-struct b [(x STRING)])
                               (struct b [])))))
 (check-exn exn:fail?
            (λ () (compile #'(sourcery-struct x [(__field STRING)]))))
 (check-exn exn:fail?
            (λ () (compile #'((sourcery-struct y [(create INTEGER)
                                                  (update INTEGER)
                                                  (map INTEGER)
                                                  (unmapped INTEGER)]))))))

(sourcery-test-suite
 ""
 #:before su-create-all
 #:after  td-complete
 )

;; Run all tests
(run-sourcery-tests)

;; Delete testing database
(stest-teardown "test.db")

