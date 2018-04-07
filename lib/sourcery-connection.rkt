#lang racket

(provide
 sourcery-connection
 get-sourcery-connection
 set-sourcery-connection!
 SOURCERY_ID_FIELD_NAME)

(require db)

;; -----------------------------------------------------------------------
;; Database Connection

(define SOURCERY_ID_FIELD_NAME "__sourcery_id")

;; The single database connection for a SQLSourcery program
(define sourcery-connection #f)

;; String -> Void
;; Set the connection
(define (set-sourcery-connection! path)
  (begin
    (if (sqlite3-available?)
        (set! sourcery-connection (sqlite3-connect #:database path #:mode 'create))
        (error 'sqllite3 (string-append "SQLite 3 is required to run SQLSourcery and is " 
                                        "not available on this system")))
    (void)))

;; Void -> DBConnection
;; get the current sourcery connection or error if none is set
(define (get-sourcery-connection)
  (if (equal? #false sourcery-connection)
      (error 'sourcery-connection "No sourcery connection set - must use sourcery-db before call")
      sourcery-connection))