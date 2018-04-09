#lang racket

(provide

 ;; Sourcery Construct Basics
 sourcery-db
 sourcery-struct
 sourcery-load
 sourcery-delete

 ;; Complex Sourcery Constructs
 sourcery-filter-delete
 sourcery-dead-reference?

 ;; Override normal structures
 (rename-out [error-check-struct struct])

 ;; User Testing
 (all-from-out "user-testing.rkt")
 )

;; Language Requirements
(require db
         "user-testing.rkt"
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
                     "sourcery-connection.rkt"
                     "type-support.rkt"
                     "utils.rkt"
                     "utils-phase-1.rkt"))


(define-syntax error-check-struct
  (syntax-parser
    [(_ name parts ...)
     (if (not (sourcery-struct-exists? (id->string #'name)))
         (begin
           (add-defined-struct (id->string #'name))
           #'(struct name parts ...))
         (error 'struct (format (string-append "cannot create a struct with the name "
                                               "of an existing sourcery-struct: ~a")
                                (id->string #'name))))]))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-begin and sourcery-db
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-db
;; create a database connection to the given path and set as current connection
(define-syntax sourcery-db
  (syntax-parser
    [(_ path:string)
     #'(set-sourcery-connection! path)]
    [else (error 'sql-sourcery "sourcery-db must take in a single string")]))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-struct
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-struct
;; Create sourcery-struct database table create, constructors, updator, and accessors
(define-syntax sourcery-struct
  (syntax-parser
    [(_ struct-name:id [(field:id type:id) ...])
     #:with name-create   (format-id #'struct-name "~a-create" #'struct-name)
     #:with name-create-if-not-exists
     (format-id #'struct-name "~a-create-if-not-exists" #'struct-name)
     #:with name-pred     (format-id #'struct-name "~a?" #'struct-name)
     #:with name-update   (format-id #'struct-name "~a-update" #'struct-name)
     #:with name-unmapped (format-id #'struct-name "~a-unmapped" #'struct-name)
     #:with name-map      (format-id #'struct-name "~a-map" #'struct-name)
     #:with num-fields    (length (syntax->list #`(field ...)))
     (let [(struct-arg-checker (create-arg-type-checker #'struct-name #'(field ...) #'(type ...)))]
       (begin
         
         ;; Check field names are valid
         (map field-valid? (map id->string (syntax->list #'(field ...))))
         
         #`(begin
             ;; Create a syntax object for the struct-name to be used for arrows
             (define-syntax struct-name
               #,(id->string #'struct-name))
           
             ;; Check sourcery-struct does not already exist      
             #,(if (sourcery-struct-exists? (id->string #'struct-name))
                   (error (string-append (id->string #'struct-name) ":")
                          "multiple sourcery-struct definitions")
                   (void))

             ;; Check sourcery-struct does not share a name with a struct
             #,(if (struct-defined? (id->string #'struct-name))
                   (error (string-append (id->string #'struct-name) ":")
                          "existing structure with same name previously defined")
                   (void))

             ;; Check struct has at least one field
             (if (zero? (syntax->datum #'num-fields))
                 (error #,(string-append (id->string #'struct-name) ":")
                        "sourcery-struct must have at least one field")
                 (void))

          
             ;; Check types of fields in structure defition
             (let [(res #,(first-failing (compose validate-type symbol->string syntax->datum)
                                         (compose symbol->string syntax->datum)
                                         (syntax->list #'(type ...))))]
               (if (equal? #false res)
                   (void)
                   (error #,(string-append (id->string #'struct-name) ":")
                          (format "bad type given in sourcery-struct definition ~a: ~a"
                                  #,(id->string #'struct-name) res))))

             ;; Check structure definition does not overwrite previous declaration in database
             (let [(table-count (first
                                 (first
                                  (rows->lists
                                   (query-rows (get-sourcery-connection)
                                               (format (string-append "SELECT count(*) "
                                                                      "FROM sqlite_master WHERE "
                                                                      "type='table' AND name='~a'")
                                                       #,(id->string #'struct-name)))))))]
               (if (= table-count 1)
                   (let [(table-info (map (λ (r) (list (second r) (third r)))
                                          (rows->lists
                                           (query-rows (get-sourcery-connection)
                                                       (format "pragma table_info(~a)"
                                                               #,(quote-field
                                                                  (id->string #'struct-name)))))))
                         (dec-info
                          (cons (list SOURCERY_ID_FIELD_NAME "INTEGER")
                                (map list
                                     (list (symbol->string (syntax->datum #'field)) ...)
                                     (list (symbol->string (syntax->datum #'type)) ...))))]
                     (if (and (= (length table-info) (length dec-info))
                              (andmap equal? table-info dec-info))
                         (void)
                         (error #,(string-append (id->string #'struct-name) ":")
                                (format (string-append "defition does not match database table: "
                                                       "expects (sourcery-struct ~a ~a")
                                        #,(id->string #'struct-name)
                                        (rest table-info)))))
                   (begin
                     ;; Create the table
                     #,(let
                           [(creation-string (table-creation-string #'struct-name
                                                                    #'(field ...)
                                                                    #'(type ...)))]
                         #`(query-exec (get-sourcery-connection) #,creation-string))
                     (void))))

             ;; update sourcery-struct-info at phase 0
             (update-sourcery-struct-info (list (symbol->string (syntax->datum #'struct-name))
                                                (list (symbol->string (syntax->datum #'field)) ...)
                                                (list (symbol->string (syntax->datum #'type)) ...)))

             ;; update sourcery-struct-info at phase 1
             #,(update-sourcery-struct-info (list (symbol->string (syntax->datum #'struct-name))
                                                  (map id->string (syntax->list #'(field ...)))
                                                  (map id->string (syntax->list #'(type ...)))))

             ;; Define predicate
             (define-syntax name-pred
               (syntax-parser
                 [(_ x)
                  #`(and (sourcery-ref? x)
                         (string=? (sourcery-ref-table x) #,(id->string #'struct-name))
                         (list? (get-row (sourcery-ref-table x) (sourcery-ref-id x))))]))
         
             ;; Define create
             (define-syntax name-create
               (syntax-parser
                 [(_ ) (error 'struct-create (format "Expected ~a arguments for struct-create"
                                                     (syntax->datum #'num-fields)))]
                 [(_ . args)
                  (if (= (syntax->datum #'num-fields) (length (syntax->list #'args)))
                      #`(begin
                          (let [(arg-vals (list . args))]
                            ;; Check input types
                            (#,#,struct-arg-checker arg-vals "create")
                  
                            ;; Insert into database
                            (query-exec (get-sourcery-connection)
                                        (format "INSERT INTO ~a (~a) VALUES (~a)"
                                                #,(quote-field (id->string #'struct-name))
                                                #,(comma-separate (map (λ (f)
                                                                         (quote-field (id->string f)))
                                                                       (syntax->list #'(field ...))))
                                                (comma-separate (map format-sql-type arg-vals)))))
                  
                          ;; Return a sourcery reference for access to structure
                          (sourcery-ref #,(id->string #'struct-name)
                                        (get-created-id #,(id->string #'struct-name))))
                      (error 'struct-create (format "invalid number of arguments, expected ~a got: ~a"
                                                    (syntax->datum #'num-fields)
                                                    (length (syntax->list #'args)))))]))

             ;; Define create-if-not-exists
             (define-syntax name-create-if-not-exists
               (syntax-parser
                 [(_ ) (error 'struct-create-if-not-exists (format "Expected ~a arguments for struct-create"
                                                     (syntax->datum #'num-fields)))]
                 [(_ . args)
                  (if (= (syntax->datum #'num-fields) (length (syntax->list #'args)))
                      #`(begin
                          (let [(arg-vals (list . args))
                                (query-result (query-rows (get-sourcery-connection)
                                                          (format "SELECT * FROM ~a WHERE ~a"
                                                                  #,(quote-field (id->string #'struct-name))
                                                                  (create-set-values-list #'(field ...)
                                                                                          args))))]
                            ;; Check input types
                            (#,#,struct-arg-checker arg-vals "create")
                            (if (> (length query-result) 0)
                                (apply name-create (list . args))
                                (void))))
                      (error 'struct-create-if-not-exists (format "invalid number of arguments, expected ~a got: ~a"
                                                    (syntax->datum #'num-fields)
                                                    (length (syntax->list #'args)))))]
                 [else (error 'struct-create-if-not-exists "shit don match")]))
         
             ;; Define accessors
             #,(generate-accessors #'struct-name
                                   #'(field ...)
                                   #'(type ...))

             ;; Define updator
             (define-syntax name-update
               (syntax-parser
                 [(_ ) (error 'struct-update (format "Expected ~a arguments for struct-update"
                                                     (+ 1 (syntax->datum #'num-fields))))]
                 [(_ ref . args)
                  (if (= (syntax->datum #'num-fields) (length (syntax->list #'args)))
                      #`(let [(ref-res ref)
                              (arg-vals (list . args))]
                          (begin
                            ;; Check input types
                            (#,#,struct-arg-checker arg-vals "update")

                            ;; Insert into database
                            (query-exec (get-sourcery-connection)
                                        (format
                                         #,(format "UPDATE ~a SET ~~a WHERE ~a = ~~a"
                                                   (quote-field (id->string #'struct-name))
                                                   SOURCERY_ID_FIELD_NAME)
                                         (comma-separate (create-set-values-list #'(field ...)
                                                                                 arg-vals))
                                         (sourcery-ref-id ref-res)))

                            ;; Return the same reference
                            ref-res))
                      (error 'struct-update (format "invalid number of arguments, expected ~a got: ~a"
                                                    (+ 1 (syntax->datum #'num-fields))
                                                    (+ 1 (length (syntax->list #'args))))))])))))]
    [else  (error 'sourcery-struct (string-append "expected expression of form (sourcery-struct id "
                                                  "[(field type)..])"))]))
                                                          

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
           #`
           (let [(ref-res ref)]
             (if (#,#'#,(format-id struct-name "~a?" struct-name) ref-res)
                 
                 (let [(query-result
                        (query-rows (get-sourcery-connection)
                                    (format #,(gen-accessor-query-format
                                               (quote-field
                                                (symbol->string (syntax->datum #'#,field)))
                                               (quote-field
                                                (symbol->string (syntax->datum #'#,struct-name))))
                                            (sourcery-ref-id ref-res))))]
                   (if (= (length query-result) 1)
                       (let [(type-translator
                              (fourth (get-type-info #,#,(symbol->string (syntax->datum type)))))]
                         (type-translator (first (vector->list (first query-result)))))
                       (error "sourcery-ref: " "sourcery reference does not exist")))
                 
                 (error #,#,(string-append (id->string accessor-id) ":")
                        (format "expected ~a, given: ~a"
                                #,#,(id->string struct-name)
                                ref-res))))]
          [else (error 'struct-accessor "Expected a single reference in struct accessor")]))))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-load
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-load
;; load all structures from the given id referencing a defineed sourcery-struct
(define-syntax sourcery-load
  (syntax-parser
    [(_) (error 'sourcery-load "expected structure name")]
    [(_ tbl)
     #`(let* [(tbl-string #,(syntax-local-value
                             #'tbl
                             (λ ()
                               (error 'sourcery-load "expected existing structure name got: ~a"
                                      (syntax->datum #'tbl)))))
              (s-s-i (get-sourcery-struct-info tbl-string))]
         (map (λ (r) (sourcery-ref tbl-string (first r)))
              (rows->lists (query-rows (get-sourcery-connection)
                                       (format "SELECT * FROM ~a"
                                               (quote-field tbl-string))))))]
    [else (error 'sourcery-load "expects single structure name")]))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-delete
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-delete
;; delete the given sourcery-struct reference
(define (sourcery-delete ref)
  (if (sourcery-ref? ref)
      (begin (query-exec (get-sourcery-connection)
                         (format "DELETE FROM ~a WHERE ~a = ~a"
                                 (quote-field (sourcery-ref-table ref))
                                 SOURCERY_ID_FIELD_NAME
                                 (sourcery-ref-id ref)))
             #t)
      (error 'sourcery-delete
             (format "Expected sourcery-struct, got: ~a" ref))))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-filter-delete
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-filter-delete
;; delete all sourcery references that don't pass the given predicate
(define (sourcery-filter-delete pred refs)
  (if (and (list? refs) (andmap valid-sourcery-ref? refs))
      (let [(return-list (filter pred refs))
            (to-delete (filter (λ (x) (not (pred x))) refs))]
        (begin
          (map sourcery-delete to-delete)
          return-list))
      (error 'sourcery-filter-delete
             (format "Expected list of sourcery-structs, got: ~a" refs))))


;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-dead-reference?
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-dead-reference
;; determines if a given sourcery-struct is a valid reference
(define (sourcery-dead-reference? x)
  (if (sourcery-ref? x)
      (not (valid-sourcery-ref? x))
      (error 'TODO (format "Expected sourcery-struct, got: ~a" x))))
