#lang s-exp "lang.rkt"

(sourcery-db "test.db")
(sourcery-struct user [(name INTEGER)])
(sourcery-struct users [(name INTEGER) (grade INTEGER)])
(user-create -1)
(users-create 3 4)
(users-create 2 3)
1
2

(define x 1)

(list 1 2 3)
((λ (x) 1) 2)
