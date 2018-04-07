#lang racket

(require "../../lib/sql-sourcery.rkt")
(sourcery-db "test-1.db")

;; sourcery-struct
(sourcery-struct frog [(color STRING)])
#;(sourcery-struct )
#;(sourcery-struct frog [(bob LOG)])
#;(sourcery-struct frog [(bog)])

(define froggy (frog-create "green"))
#;(frog-create )
#;(frog-create "green" "mean")
;; still runtime
;(frog-create 1)


(frog-update froggy "blue")
#;(frog-update) 
(frog-update "blue" "green" "black")
;; still runtime
;(frog-update froggy froggy)
;(frog-update "blue")


(frog-color froggy)
#;(frog-color 1 1)
#;(frog-color)
;; still runtime
;(frog-color 1)

(sourcery-load frog)
#;(sourcery-load)
#;(sourcery-load 1 2)
#;(sourcery-load steve)


;;check-syntax
;; 9.2

#;(struct a [])
#;(sourcery-struct a [(__a STRING)])
#;(sourcery-struct a [(create INTEGER) (update INTEGER) (map INTEGER) (unmapped INTEGER)])
#;(struct student [])

