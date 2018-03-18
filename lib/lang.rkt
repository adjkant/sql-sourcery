#lang racket

(provide
 
 ;; Language Basics
 (rename-out (sourcery-begin #%module-begin))
 #%app
 #%datum
 #%top
 #%top-interaction
 require

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
                     racket/syntax
                     racket
                     db))

;; Reference Structure
(define-struct sourcery-ref [table id])

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
         #`(#%module-begin
            (error 'sqllite3 "SQLite 3 is not available on this system.")))]))


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
     #:with name-create (format-id #'struct-name "~a-create" #'struct-name)
     #:with name-update (format-id #'struct-name "~a-update" #'struct-name)
  
     #`(begin
         ;; Check argument types
         (let [(res (andmap #,(compose validate-type symbol->string syntax->datum)
                            (syntax->list #'(type ...))))]
           (if res
               (void)
               (error 'sourcery-struct "bad type given")))
         
         ;; Create the table (if it doesn't already exist in the db)
         #,(let
               [(creation-string (table-creation-string #'struct-name
                                                        #'(field ...)
                                                        #'(type ...)))]
             #`(query-exec sourcery-connection #,creation-string))
         
         ;; Create function to create a structure
         (define-syntax name-create
           (syntax-parser
             [(_ . args)
              #`(begin
                  ;; Check input types
                  (#,(create-arg-type-checker #'struct-name #'(field ...) #'(type ...)) #'args)
                  ;; Insert into database
                  (query-exec sourcery-connection
                              #,(format "INSERT INTO ~a (~a) VALUES (~a)"
                                        (id->string #'struct-name)
                                        (comma-separate (map (λ (f)
                                                               (id->string f))
                                                             (syntax->list #'(field ...))))
                                        (comma-separate (map format-sql-types
                                                             (syntax->list #'args)))))
                  ;; Return a sourcery reference for access to structure
                  (sourcery-ref #,(id->string #'struct-name)
                                (get-created-id #,(id->string #'struct-name))))])))]))

(define-for-syntax (create-arg-type-checker struct fields types)
  (λ (args)
    (begin
      (map (λ (f t a)
             (let
                 [(field (syntax->datum f))
                  (type (symbol->string (syntax->datum t)))
                  (arg (syntax->datum a))]
               (if ((type->predicate type) arg)
                   (void)
                   (error (string-append (id->string struct) "-create:")
                          (format "expected type ~a for field ~a: got ~v"
                                  type field arg)))))
           (syntax->list fields)
           (syntax->list types)
           (syntax->list args))
      (void))))

(define-for-syntax (format-sql-types stx)
  (let [(dat (syntax->datum stx))] 
    (cond [(integer? dat) dat]
          [(string? dat) (format "\"~a\"" dat)]
          [(boolean? dat) (if dat "\"TRUE\"" "\"FALSE\"")])))

(define-for-syntax (type->predicate type)
  (cond [(string=? type "INTEGER") integer?]
        [(string=? type "STRING")  string?]
        [(string=? type "BOOLEAN") boolean?]
        [else (error 'sourcery-struct (format "Invalid type ~a" type))]))

(define-for-syntax TYPES (list "INTEGER" "STRING" "BOOLEAN"))

(define-for-syntax (validate-type type)
  (ormap (λ (t) (string=? t type)) TYPES))

;; -----------------------------------------------------------------------
;; SQL Query Generation

;; Syntax Syntax Syntax -> String
;; Given the name of the structure and the fields/types, create a table creation string
(define-for-syntax (table-creation-string struct-name fields types)
  (format "CREATE TABLE IF NOT EXISTS ~a (~a)"
          (id->string struct-name)
          (string-append "sourcery_id INTEGER PRIMARY KEY AUTOINCREMENT, "
                         (comma-separate
                          (map (λ (ft-pair)
                                 (format "~a ~a"
                                         (id->string (first ft-pair))
                                         (sourcery-t->db-t (id->string (second ft-pair)))))
                               (map list (syntax->list fields) (syntax->list types)))))))

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

;; Join a list of values with commas
(define-for-syntax (comma-separate l)
  (let [(comma-list (foldl
                     (λ (col-def so-far)
                       (format "~a, ~a"
                               col-def
                               so-far))
                     ""
                     l))]
    (substring comma-list
               0 (- (string-length comma-list) 2))))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; Sourcery Connection Runtime Library
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Database Connection

;; The single database connection for a SQLSourcery program
(define sourcery-connection #f)

;; Set the connection
(define (set-conn! conn)
  (begin
    (set! sourcery-connection conn)
    (void)))


;; -----------------------------------------------------------------------
;; SQL Query Utilities

(define (get-created-id table-name)
  (get-val-from-row (query-rows sourcery-connection
                                (string-append
                                 "SELECT MAX(sourcery_id) FROM "
                                 table-name))
                    0))

;; -----------------------------------------------------------------------
;; SQL Rows Parsing

(define (get-val-from-row row idx)
  (if (= 1 (length row))
      (list-ref (first (rows->lists row)) idx)
      (error 'sql-row-parse "number of rows given is not a single row")))

(define (rows->lists rows)
  (map vector->list rows))