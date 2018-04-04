#lang racket

(provide

 sourcery-connection
 get-sourcery-connection
 set-sourcery-connection!)

;; -----------------------------------------------------------------------
;; Database Connection

;; The single database connection for a SQLSourcery program
(define sourcery-connection #f)

;; DBConnection -> Void
;; Set the connection
(define (set-sourcery-connection! conn)
  (begin
    (set! sourcery-connection conn)
    (void)))

(define (get-sourcery-connection)
  (if (equal? #false sourcery-connection)
      (error 'sourcery-connection "No sourcery connection set - must use sourcery-db before call")
      sourcery-connection))