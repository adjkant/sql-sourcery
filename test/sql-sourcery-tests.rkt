#lang s-exp "../lib/lang.rkt"

(sourcery-db "test.db")
(sourcery-struct user [(name INTEGER)])

;; Using Testing Library
(require "sql-sourcery-test-lib.rkt")
(stest-conn "test.db")


(check-true (stest-table? "user"))
(check-equal? (stest-field "user" "name") "INTEGER")
(check-success (stest-clear-table "user"))

;; Delete Testing Database
(stest-teardown "test.db")

