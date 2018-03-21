#lang s-exp "lang.rkt"
(sourcery-db "test.db")

(sourcery-struct user [(name STRING) (grade INTEGER) (failing BOOLEAN)])
(sourcery-struct professor [(name STRING)])

(define fred (user-create "FRED" 4 #true))
(define steve (user-create "STEVE" 3 #false))
(define ben (professor-create "BEN"))

fred
(user? fred)
(user? ben)
(user-name fred)
(user-grade fred)
(user-failing fred)