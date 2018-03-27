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
 sourcery-delete
 sourcery-load
 sourcery-filter-delete
 
 ;; Basic Language Constructs
 define
 let
 lambda λ
 begin
 void

 ;; Logical
 or and not

 ;; Numerical
 + - * / modulo
 number? integer? real?
 > < = >= <=
 ;; Strings
 string? string=? string-length substring string-append

 ;; Lists
 cons first rest empty list length append
 filter foldr andmap ormap map
 ;; need to overwite map
 
 )

;; Language Requirements
(require racket/struct
         db
         "sourcery-refs.rkt"
         "sourcery-connection.rkt"
         "sql.rkt"
         "type-support.rkt"
         "utils.rkt"
         (for-syntax syntax/parse
                     racket/syntax
                     racket
                     db
                     "sourcery-refs.rkt"
                     "type-support.rkt"
                     "utils.rkt"
                     "utils-phase-1.rkt"))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-begin and sourcery-db
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

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-struct
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

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
                                        (comma-separate (map format-sql-type
                                                             (syntax->list #'args)))))
                  
                  ;; Return a sourcery reference for access to structure
                  (sourcery-ref #,(id->string #'struct-name)
                                (get-created-id #,(id->string #'struct-name))))]))

         ;; Define predicate
         (define-syntax name-pred
           (syntax-parser
             [(_ x)
              #`(and (sourcery-ref? x)
                     (string=? (sourcery-ref-table x) #,(id->string #'struct-name))
                     (list? (get-row (sourcery-ref-table x) (sourcery-ref-id x))))]))
         
         ;; Define accessors
         #,(generate-accessors #'struct-name
                               #'(field ...)
                               #'(type ...))

         ;; Define updator
         (define-syntax name-update
           (syntax-parser
             [(_ ref . args)
              #`(begin
                  ;; Check input types
                  (#,(create-arg-type-checker #'struct-name #'(field ...) #'(type ...)) #'args)

                  ;; Insert into database
                  (query-exec sourcery-connection
                              (format
                               #,(format "UPDATE ~a SET ~~a WHERE sourcery_id = ~~a"
                                         (id->string #'struct-name))
                               (comma-separate (create-set-values-list #'(field ...) #'args))
                               (sourcery-ref-id ref)))

                  ;; Return the same reference
                  ref)])))]))

;; -----------------------------------------------------------------------
;; Utilities for sourcery-struct

;; Syntax Syntax Syntax -> Syntax
;; generate creation of accessors for structure definition
(define-for-syntax (generate-accessors struct-name fields types)
  (syntax-list->begin (map (λ (f t) (generate-accessor struct-name f t))
                           (syntax->list fields)
                           (syntax->list types))))

;; Syntax Syntax Syntax -> Syntax
;; generate single accessors for given structure, field, and type
(define-for-syntax (generate-accessor struct-name field type)
  (let [(accessor-id (format-id struct-name "~a-~a" struct-name (syntax->datum field)))
        (pred-name (format-id struct-name "~a?" struct-name))]
    #`(define-syntax #,accessor-id
        (syntax-parser
          [(_ ref)
           #`(if (#,#'#,(format-id struct-name "~a?" struct-name) ref)
                 
                 (let [(query-result
                        (query-rows sourcery-connection
                                    (format #,(gen-accessor-query-format
                                               (symbol->string (syntax->datum #'#,field))
                                               (symbol->string (syntax->datum #'#,struct-name)))
                                            (sourcery-ref-id ref))))]
                   (if (= (length query-result) 1)
                       (let [(type-translator
                              (fourth (get-type-info #,#,(symbol->string (syntax->datum type)))))]
                         (type-translator (first (vector->list (first query-result)))))
                       (error "sourcery-ref: " "sourcery reference does not exist")))
                 
                 (error #,#,(string-append (id->string accessor-id) ":")
                        (format "expected ~a, given: ~a"
                                #,#,(id->string struct-name)
                                ref)))]))))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-load
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

(define-syntax sourcery-load
  (syntax-parser
    [(_ tbl:id)
     #`(let* [(tbl-string #,(id->string #'tbl))
              (s-s-i (get-sourcery-struct-info tbl-string))]
         (map (λ (r) (sourcery-ref tbl-string (first r)))
              (rows->lists (query-rows sourcery-connection
                                       (format "SELECT * FROM ~a"
                                               tbl-string)))))]))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-delete
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

(define (sourcery-delete ref)
  (if (sourcery-ref? ref)
      (begin (query-exec sourcery-connection
                         (format "DELETE FROM ~a WHERE sourcery_id = ~a"
                                 (sourcery-ref-table ref)
                                 (sourcery-ref-id ref)))
             #t)
      (error 'sourcery-delete
             (format "Expected sourcery-struct, got: ~a" ref))))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-filter-delete
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

(define (sourcery-filter-delete pred refs)
  (if (and (list? refs) (andmap valid-sourcery-ref? refs))
      (let [(to-delete (filter pred refs))
            (return-list (filter (λ (x) (not (pred x))) refs))]
        (begin
          (map sourcery-delete to-delete)
          return-list))
      (error 'sourcery-filter-delete
             (format "Expected list of sourcery-structs, got: ~a" refs))))

