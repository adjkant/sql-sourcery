(sourcery-db “path/to/database.sqlite”)                 ;; Use existing database or create
(sourcery-struct user [(name String) (grade Integer)])  ;; map users to db with given types

(define original-users (sourcery-load user))            ;; loads state from db
(define new-users (list (user-create “Matthias” 1)      ;; add two records to db
                        (user-create “Ben L” 0)))    

(define users (append new-users original-users))        ;; does not change database

(define (is-failing? u)                                 ;; User -> Boolean
(= (user-grade u) 0))                                   ;; Purpose: determine if failing

(define (up-grade u)                                    ;; User -> User
(user-update u (user-name u) (+ 1 (user-grade u))))   	;; Purpose: updates user grade

(define failures (filter is-failing? users))            ;; does not update DB

(set! users (sourcery-filter is-failing? users))        ;; Remove from db, users still
                                                        ;; tracks the db structures

(sourcery-map up-grade failures)                        ;; throws an error: a given user does
                                                        ;; not exist

