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
;; still broken
(frog-create "green" "mean")

(frog-color froggy)
#;(frog-color 1 1)
#;(frog-color)
;; still broken
(frog-color 1)

;;check-syntax
;; 9.2


(sourcery-load frog)
#;(sourcery-load) ;-> need better err
#;(sourcery-load 1 2) ;-> better err?
#;(sourcery-load steve)