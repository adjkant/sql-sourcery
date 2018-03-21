#lang racket

(provide

 sourcery-connection
 set-conn!)

;; -----------------------------------------------------------------------
;; Database Connection

;; The single database connection for a SQLSourcery program
(define sourcery-connection #f)

;; DBConnection -> Void
;; Set the connection
(define (set-conn! conn)
  (begin
    (set! sourcery-connection conn)
    (void)))