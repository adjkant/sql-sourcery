#lang s-exp "lang.rkt"

(sourcery-db "test.db")
(sourcery-struct user [(name STRING) (grade INTEGER) (failing BOOLEAN)])
(user-create "FRED" "4" #true)
(user-create "STEVE" 3 #false)
;(sourcery-struct bad [(bad X)])
1
2

(define x 1)

(list 1 2 3)
((λ (x) 1) 2)
