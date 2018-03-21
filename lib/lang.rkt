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
(require racket/struct
         db
         "lang-utils.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     racket
                     db
                     "lang-utils.rkt"))

;; An SQLData is one of:
;; - String
;; - Number

;; A SQLTypeName is one of:
;; - "STRING"
;; - "INTEGER"
;; - "BOOLEAN"

;; A SupportedStructType is one of:
;; - String
;; - Integer
;; - Boolean

;; A SQLSourceryTypeInfo is a:
;; (list SQLTypeName
;;       [Any -> Boolean]
;;       [SupportedStructType -> SQLData]
;;       [SQLData -> SupportedStructType])
;; Interpretation for list:
;; - SQLTypeName is the name of the data
;; - predicate for the SupportedStructType
;; - translator from SupportedStructType to SQLData
;; - translator from SQLData to SupportedStructType

;; A SQLSourceryStructInfo is a:
;; - (list String [List-of String] [List-of SQLTypeName])
;; Interpretation for list:
;; - the first item is the name of the struct
;; - the second item is the list of field names
;; - the third item is the list of types for the given fields

;; structure to represent table rows and print them as the user assumed structures
(define-struct sourcery-ref [table id]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (sourcery-ref-table obj))
      (lambda (obj)
        (append
         (struct-field-values (sourcery-ref-table obj) (sourcery-ref-id obj))))))])

;; convert [List-of SQLData] to [List-of SupportedStructType] using the [List-of SQLTypeName]
;; for internal use only to allow for printing
(define-syntax struct-field-values
  (syntax-parser
    [(_ table id)
     #`(let
           [(sourcery-struct-i
             (first (filter (λ (i) (string=? (first i) table))
                            sourcery-struct-info)))]
         (translate-types
          (vector->list (first (query-rows sourcery-connection
                                           (format "SELECT ~a FROM ~a WHERE sourcery_id = ~a"
                                                   (comma-separate (second sourcery-struct-i))
                                                   table id))))
          (third sourcery-struct-i)))]))

;; [List-of SQLSourceryStructInfo]
;; the sorucery structs defined at runtime
(define sourcery-struct-info '())

;; SQLSourceryStructInfo -> Void
;; add the given struct info to the runtime environment
(define (update-sourcery-struct-info to-add)
  (set! sourcery-struct-info (cons to-add sourcery-struct-info)))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; SQLSourcery Language Constructs
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; #%module-begin
;; verify the shape of the program is valid
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

;; sourcery-db
;; create a database connection
(define-syntax sourcery-db
  (syntax-parser
    [(_ path:string)
     #'(set-conn!
        (sqlite3-connect
         #:database path
         #:mode 'create))]))

;; sourcery-struct
;; Create sourcery-struct database table, loader, constructors, updator, and accessors
(define-syntax sourcery-struct
  (syntax-parser
    [(_ struct-name:id [(field:id type:id) ...])
     #:with name-create (format-id #'struct-name "~a-create" #'struct-name)
     #:with name-pred   (format-id #'struct-name "~a?" #'struct-name)
     #:with name-update (format-id #'struct-name "~a-update" #'struct-name)
     #`(begin
         ;; Check types of fields in structure defition
         (let [(res #,(first-failing (compose validate-type symbol->string syntax->datum)
                                     (compose symbol->string syntax->datum)
                                     (syntax->list #'(type ...))))]
           (if (equal? #false res)
               (void)
               (error #,(string-append (id->string #'struct-name) ":")
                      (format "bad type given in sourcery-struct definition ~a: ~a"
                              #,(id->string #'struct-name) res))))
         
         ;; Create the table (if it doesn't already exist in the db)
         #,(let
               [(creation-string (table-creation-string #'struct-name
                                                        #'(field ...)
                                                        #'(type ...)))]
             #`(query-exec sourcery-connection #,creation-string))

         ;; update sourcery-struct-info with structure
         (update-sourcery-struct-info (list (symbol->string (syntax->datum #'struct-name))
                                            (list (symbol->string (syntax->datum #'field)) ...)
                                            (list (symbol->string (syntax->datum #'type)) ...)))
         
         ;; Define create
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
                                        (comma-separate (map id->string
                                                             (syntax->list #'(field ...))))
                                        (comma-separate (map format-sql-types
                                                             (syntax->list #'args)))))
                  
                  ;; Return a sourcery reference for access to structure
                  (sourcery-ref #,(id->string #'struct-name)
                                (get-created-id #,(id->string #'struct-name))))]))

         ;; Define predicate
         (define-syntax name-pred
           (syntax-parser
             [(_ x)
              #`(and (sourcery-ref? x)
                     (string=? (sourcery-ref-table x) #,(id->string #'struct-name)))]))
         
         ;; Define accessors
         #,(generate-accessors #'struct-name
                               #'(field ...)
                               #'(type ...)))]))

(define-for-syntax (generate-accessors struct-name fields types)
  (syntax-list->begin (map (λ (f t) (generate-accessor struct-name f t))
                           (syntax->list fields)
                           (syntax->list types))))

(define-for-syntax (generate-accessor struct-name field type)
  (let [(accessor-id (format-id struct-name "~a-~a" struct-name (syntax->datum field)))
        (pred-name (format-id struct-name "~a?" struct-name))]
    #`(define-syntax #,accessor-id
        (syntax-parser
          [(_ ref)
           #`(if (#,#'#,(format-id struct-name "~a?" struct-name) ref)
                 
                 (let [(query-result (query-rows sourcery-connection
                                                 (format #,(gen-accessor-query-format
                                                            (symbol->string (syntax->datum #'#,field))
                                                            (symbol->string (syntax->datum #'#,struct-name)))
                                                         (sourcery-ref-id ref))))]
                   (if (= (length query-result) 1)
                       (let [(type-translator (fourth (get-type-info #,#,(symbol->string (syntax->datum type)))))]
                         (type-translator (first (vector->list (first query-result)))))
                       (error "sourcery-ref " "sourcery reference does not exist")))
                 
                 (error #,#,(string-append (id->string accessor-id) ":")
                        (format "expected ~a, given: ~a"
                                #,#,(id->string struct-name)
                                ref)))]))))

(define-for-syntax (gen-accessor-query-format struct-name struct-field)
  (format "SELECT ~a FROM ~a WHERE sourcery_id = ~~a" struct-name struct-field))
          

(define-for-syntax (syntax-list->begin syntaxes)
  (foldl
   (λ (stx stx-so-far)
     (syntax-parse stx-so-far
       [((~literal begin) items ...)
        #`(begin items ... #,stx)]))
   #`(begin)
   syntaxes))
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; SQLSourcery Compile Time Library
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; -----------------------------------------------------------------------
;; Structure Type Functions

;; Syntax Syntax Syntax -> [Syntax Syntax Syntax -> Void]
;; create function to check arguments for structure creation
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
                          (format "expected type ~a for ~a: got ~v"
                                  type field arg)))))
           (syntax->list fields)
           (syntax->list types)
           (syntax->list args))
      (void))))

;; Syntax -> Data
;; Format a piece of data into its matching format by type as determined by type predicate
(define-for-syntax (format-sql-types stx)
  (let* [(data (syntax->datum stx))
         (stx-type (filter (λ (t) ((second t) data)) TYPES))]
    (if (= (length stx-type) 1)
        ((third (first stx-type)) data)
        (error 'sourcery-struct (format "Invalid type: ~a" data)))))    

;; SQLTypeName -> [Any -> Boolean]
;; Turn a given type name into its matching predicate
(define-for-syntax (type->predicate name)
  (let [(name-type (filter (λ (t) (string=? (first t) name)) TYPES))]
    (if (= (length name-type) 1)
        (second (first name-type))
        (error 'sourcery-struct (format "Invalid type: ~a" name)))))

;; String -> Boolean
;; Validate that a given string is a SQLTypeName
(define-for-syntax (validate-type type)
  (ormap (λ (t) (string=? (first t) type)) TYPES))

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

;; Id -> String
;; Convert an identifier to a string
(define-for-syntax (id->string id)
  (symbol->string `,(syntax->datum id)))  

;; [X -> Boolean] [X -> Y] [List-of X] -> [Maybe Y]
;; return the first item in the list to fail the predicate, translated by the given function
;; return false if no item fails the predicate
(define-for-syntax (first-failing proc return-form lst)
  (cond [(empty? lst) #false]
        [(cons? lst)
         (if (proc (first lst))
             (first-failing proc return-form (rest lst))
             (return-form (first lst)))]))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; SQLSourcery Runtime Library
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

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


;; -----------------------------------------------------------------------
;; Type Translation

;; SQLData SQLTypeName -> SupportedStructType
;; translate a value to the given type as used in structures
(define (translate-types values types)
  (map translate-type values types))
  
(define (translate-type value type)
  (let* [(t-info (filter (λ (t) (string=? type (first t))) TYPES))]
    (if (= (length t-info) 1)
        ((fourth (first t-info)) value)
        (error 'sourcery-struct (format "Invalid translation type: ~a" type)))))


;; -----------------------------------------------------------------------
;; SQL Query Utilities

;; String -> Integer
;; Get ID of last created row in given table
(define (get-created-id table-name)
  (get-val-from-row (query-rows sourcery-connection
                                (string-append
                                 "SELECT MAX(sourcery_id) FROM "
                                 table-name))
                    0))

;; -----------------------------------------------------------------------
;; SQL Rows Parsing

;; [List-of Vector] Integer -> Any
;; get the value from a single row at the given index position
(define (get-val-from-row row idx)
  (if (= 1 (length row))
      (if (< idx (length (first (rows->lists row))))
          (list-ref (first (rows->lists row)) idx)
          (error 'get-val-from-row "index out of bounds in row"))
      (error 'get-val-from-row (format "~a rows given, expected 1 row" (length row)))))


;; [List-of Vector] -> [List-of [List-of Any]]
;; turn a list of vectors into a list of lists
(define (rows->lists rows)
  (map vector->list rows))