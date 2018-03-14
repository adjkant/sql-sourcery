#lang racket

(provide
 
 ;; Language Basics
 (rename-out (sourcery-begin #%module-begin))
 #%app
 #%datum
 #%top
 #%top-interaction

 ;; Sourcery Constructs
 sourcery-db
 sourcery-struct

 ;; Basic Language Constructs
 define
 let
 lambda λ

 ;; Logical
 or and not

 ;; Numerical
 + - * / modulo
 number? integer? real?

 ;; Strings
 string? string=? string-length substring string-append

 ;; Lists
 cons first rest empty list length append
 filter foldr andmap ormap
 ;; need to overwite map
 
 )

;; Language Requirements
(require db
         (for-syntax syntax/parse
                     racket
                     db))

;; Confirm Program Shape
(define-syntax sourcery-begin
  (syntax-parser
    [(_ ((~literal sourcery-db) path:string)
        ((~literal sourcery-struct) name fields) ...
        prog ...)
     (if (sqlite3-available?)
         #`(#%module-begin
            (sourcery-db path)
            (sourcery-struct name fields) ...
            prog ...)
         #`(#%module-begin (error 'sqllite3 "SQLite 3 is not available on this system.")))]))


;; Ensure Existence of Database
(define-syntax sourcery-db
  (syntax-parser
    [(_ path:string)
     #'(set-conn!
         (sqlite3-connect
          #:database path
          #:mode 'create))]))

;; Ensure Table Existence
(define-syntax sourcery-struct
  (syntax-parser
    [(_ struct-name:id [(field type) ...])
     (let
         [(creation-string (table-creation-string #'struct-name #'(field ...) #'(type ...)))]
       #`(query-exec sourcery-connection #,creation-string))]))

;; -----------------------------------------------------------------------
;; SQL CREATE Generation

;; Syntax Syntax Syntax -> String
;; Given the name of the structure and the fields/types, create a table creation string
(define-for-syntax (table-creation-string struct-name fields types)
  (let [(fields-decs (string-append "sourcery_id INT PRIMARY KEY AUTOINCREMENT, "
                                    (table-field-declarations fields types)))]
    (string-append
     "CREATE TABLE IF NOT EXISTS " (id->string struct-name)
     "(" (substring fields-decs 0 (- (string-length fields-decs) 2)) ")")))

;; Syntax Syntax -> String
;; take the fields and types syntax objects and convert to a creation string for fields in a SQL table
(define-for-syntax (table-field-declarations fields types)
  (foldl
   (λ (col-def so-far)
     (string-append
      col-def
      ", "
      so-far))
   ""
   (map (λ (ft-pair)
          (string-append
           (id->string (first ft-pair))
           " "
           (sourcery-t->db-t (id->string (second ft-pair)))))
        (map list (syntax->list fields) (syntax->list types)))))


;; String -> String
;; convert a sourcery type to a database type
(define-for-syntax (sourcery-t->db-t external-type-string)
  external-type-string)

;; -----------------------------------------------------------------------
;; Phase 1 Utilities

;; ID to String
;; Convert an identifier to a string
(define-for-syntax (id->string id)
  (symbol->string `,(syntax->datum id)))  

;; -----------------------------------------------------------------------
;; Sourcery Connection Runtime Library

;; The single database connection for a SQLSourcery program
(define sourcery-connection #f)

;; Set the connection
(define (set-conn! conn)
  (begin
    (set! sourcery-connection conn)
    (void)))
