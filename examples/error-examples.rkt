#lang racket

(require "../lib/sql-sourcery.rkt")
(sourcery-db "err-examples.db")

;; ----------------------------------------------------------------------------
;; sourcery-struct

#;(struct frog [color])

;; Valid
(sourcery-struct frog [(color STRING)])

#;(struct frog [color])

;; Compile Time Errors
#;(sourcery-struct [])
#;(sourcery-struct frog [(bog)])
#;(sourcery-struct frog [(bob LOG)])
#;(sourcery-struct frog [(__a STRING)])
#;(sourcery-struct frog [(create INTEGER) (update INTEGER) (map INTEGER) (unmapped INTEGER)])
#;(sourcery-struct frog [(color STRING)])

;; ----------------------------------------------------------------------------
;; structure creation

;; Valid
(define froggy (frog-create "green"))

;; Compile Time Errors
#;(frog-create )
#;(frog-create "green" "mean")

;; Runtime Errors
#;(frog-create 1)

;; ----------------------------------------------------------------------------
;; structure update

;; Valid
(frog-update froggy "blue")

;; Compile Time
#;(frog-update) 
#;(frog-update froggy "blue" "green")
#;(frog-update "blue")

;; Runtime
#;(frog-update froggy 1)

;; ----------------------------------------------------------------------------
;; structure access

;; Valid
(frog-color froggy)

;; Compile Time
#;(frog-color 1 1)
#;(frog-color)

;; Runtime
(frog-color 1)

;; ----------------------------------------------------------------------------
;; sourcery-load

;; Valid
(sourcery-load frog)

;; Compile Time
#;(sourcery-load)
#;(sourcery-load 1 2)
#;(sourcery-load steve)

