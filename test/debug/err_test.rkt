#lang s-exp "../lib/sql-sourcery.rkt"
(sourcery-db "test-1.db")

(require racket)

;; sourcery-struct
(sourcery-struct frog [(color STRING)])
#;(sourcery-struct )
#;(sourcery-struct frog [(bob LOG)])
#;(sourcery-struct frog [(bog)])

(define froggy (frog-create "green"))
#;(frog-create )
(frog-create 1)
;; still runtime
(frog-create "green" "mean")

(frog-update froggy "blue")
#;(frog-update)  ;; needs better error message
;; still runtime
(frog-update froggy froggy)
(frog-update "blue")
(frog-update "blue" "green" "black")

(frog-color froggy)
#;(frog-color 1 1)
#;(frog-color)
;; still runtime
(frog-color 1)

(sourcery-load frog)
#;(sourcery-load)
#;(sourcery-load 1 2)
#;(sourcery-load steve)

;;check-syntax
;; 9.2

