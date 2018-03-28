#lang s-exp "../lib/sql-sourcery.rkt"

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
(define bob #f)
(define ben #f)
(define steve #f)
(define bobby #f)
(define sourcery-load-results #f)

;; Setup Functions
(define-thunk-beginify su-create-bob
  (set! bob (student-create "Bob Smith" 90 #false)))

(define-thunk-beginify su-create-ben
  (set! ben (prof-create "Ben")))

(define-thunk-beginify su-create-steve
  (set! steve (student-create "Steve Steve" 20 #true)))

(define-thunk-beginify su-update-bob-create-bobby
  (set! bobby (student-update bob "Bobby Smith" 91 #true)))

(define-thunk-beginify printer-bob-display
  (displayln bob display-file))

(define-thunk-beginify printer-bob-dead-ref
  (displayln bob dead-ref-display-file))

(define-thunk-beginify su-delete-bob
  (sourcery-delete bob))

(define su-create-all (action-compose su-create-bob su-create-ben su-create-steve))

(define-thunk-beginify su-load-students
  (set! sourcery-load-results (sourcery-load student)))

(define su-none (λ () (void)))

;; Teardown Functions
(define-thunk-beginify td-clear-all-tables
  (stest-clear-table "student")
  (stest-clear-table "prof"))

(define-thunk-beginify td-unset-vars
  (set! bob #f)
  (set! ben #f)
  (set! steve #f)
  (set! bobby #f)
  (set! sourcery-load-results #f))

(define td-complete (action-compose td-clear-all-tables td-unset-vars))

(define td-none su-none)

;; -----------------------------------------------------------------------
;; Language Tests

(test-suite
 "Table Vailidity"
 #:before su-none
 #:after  td-none
 (check-true (stest-table? "student"))
 (check-equal? (stest-fields "student") 4)
 (check-equal? (stest-field "student" "sourcery_id") "INTEGER")
 (check-equal? (stest-field "student" "name") "STRING")
 (check-equal? (stest-field "student" "grade") "INTEGER")
 (check-equal? (stest-field "student" "failing") "BOOLEAN")
 (check-equal? (stest-field "student" "eh") void))

(test-suite
 "Structure Creation"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (stest-rows "student" "sourcery_id" "1")) 1)
 (check-equal? (first (stest-rows "student" "sourcery_id" "1")) (list 1 "Bob Smith" 90 "FALSE"))
 (check-exn exn:fail? (λ () (student-create 90 90 #false))
            "expected type STRING for name: got 90"))

(test-suite
 "Structure Predicates"
 #:before su-create-all
 #:after  td-complete
 (check-true (prof? ben))
 (check-true (student? bob))
 (check-false (prof? bob))
 (check-false (student? ben))
 (check-false (prof? 1)))

(test-suite
 "Structure Access"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (student-name bob) "Bob Smith")
 (check-equal? (student-grade bob) 90)
 (check-equal? (student-failing bob) #f)
 (check-exn exn:fail? (λ () (student-name ben)) "expected student, given:"))

(test-suite
 "Structure Update"
 #:before (action-compose su-create-all su-update-bob-create-bobby)
 #:after  td-complete
 (check-equal? bobby bob)
 (check-equal? (student-name bob) "Bobby Smith")
 (check-equal? (student-grade bob) 91)
 (check-equal? (student-failing bob) #true))

(test-suite
 "Structure Printer"
 #:before (action-compose su-create-all printer-bob-display)
 #:after  td-complete
 (check-equal? (read-from-file 50 "outputs/display.txt") "#<student: Bob Smith 90 #f>"))

(test-suite
 "Structure Deletion"
 #:before (action-compose su-create-all su-update-bob-create-bobby su-delete-bob printer-bob-dead-ref)
 #:after  td-complete
 (check-exn exn:fail? (λ () (sourcery-delete 1)) "Expected sourcery-struct, got: 1")
 (check-exn exn:fail? (λ () (student-name bob)) "sourcery reference does not exist")
 (check-exn exn:fail? (λ () (student-name bobby)) "sourcery reference does not exist")
 (check-false (student? bob))
 (check-equal? (read-from-file 50 "outputs/display-dead-ref.txt") "#<student: dead-reference>")
 (check-true (sourcery-delete bob))
 (check-equal? (length (stest-rows "student" "sourcery_id" "1")) 0))

(test-suite
 "sourcery-load"
 #:before (action-compose su-create-all su-load-students)
 #:after  td-complete
 (check-equal? (length sourcery-load-results) 2)
 (check-equal? (student-name (first sourcery-load-results)) "Bob Smith")
 (check-equal? (student-name (second sourcery-load-results)) "Steve Steve"))

(test-suite
 "sourcery-filter-delete None"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (sourcery-filter-delete (λ (s) #t) (sourcery-load student))) 2)
 (check-equal? (student-name (first (sourcery-load student))) "Bob Smith")
 (check-equal? (student-name (second (sourcery-load student))) "Steve Steve"))

(test-suite
 "sourcery-filter-delete All"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (sourcery-filter-delete (λ (s) #f) (sourcery-load student))) 0))

(test-suite
 "sourcery-filter-delete Some"
 #:before su-create-all
 #:after  td-complete
 (check-equal? (length (sourcery-filter-delete (λ (s) (string=? (student-name s) "Steve Steve"))
                                               (sourcery-load student)))
               1)
 (check-equal? (student-name (first (sourcery-load student))) "Steve Steve"))

(test-suite
 ""
 #:before su-create-all
 #:after  td-complete
 )


;; Run all tests
(sql-sourcery-tests)

;; Delete testing database
(stest-teardown "test.db")

