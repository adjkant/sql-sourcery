#lang racket

(require "../sql-sourcery/main.rkt")
(sourcery-db "simple-example-1.db")

;; map users to db with given types
(sourcery-struct user [(name STRING) (grade INTEGER)])

;; loads state from db
(define original-users (sourcery-load user))

;; add two records to db
(define new-users (list (user-create "Matthias" 1)
                        (user-create "Ben L" 0)))

;; does not change database
(define users (append new-users original-users))

;; User -> Boolean
;; Purpose: determine if failing
(define (is-passing? u)
  (> (user-grade u) 0))

;; User -> User
;; Purpose: updates user grade
(define (up-grade u)      
  (user-update u (user-name u) (+ 1 (user-grade u))))

;; does not update DB
(define failures (filter (λ (u) (not (is-passing? u))) users))

;; Remove from db, users still
(define winners (sourcery-filter-delete is-passing? users))

;; tracks the db structures
;; throws an error: a given user does not exist
(map up-grade failures)

(sourcery-load user)
