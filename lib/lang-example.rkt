#lang s-exp "lang.rkt"
(sourcery-db "test.db")

(sourcery-struct user [(name STRING) (grade INTEGER) (failing BOOLEAN)])

(define fred (user-create "FRED" 4 #true))
(define steve (user-create "STEVE" 3 #false))

(define x 1)

1
2
(list 1 2 3)


((λ (x) 1) 2)

fred