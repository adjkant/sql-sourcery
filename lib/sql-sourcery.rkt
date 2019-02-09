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
 (rename-out [safe-struct struct])

 ;; User Testing
 sourcery-test-suite
 run-sourcery-tests
 (all-from-out "user-testing.rkt")
 (all-from-out rackunit)
 )

;; Language Requirements
(require db
         rackunit
         rackunit/text-ui
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
                     "sql.rkt"
                     "utils.rkt"
                     "utils-phase-1.rkt"))


;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-db
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-db: String -> Void
;; create a database connection to the given path and set as current connection
(define (sourcery-db db-path)
  (if (string? db-path)
      (begin
        ;; Create the new connection
        (set-sourcery-connection! db-path)
         
        ;; recreate all tables in new database if needed
        (to-void
         (map
          (λ (s-s-i)
            (let [(table-count (first
                                (first
                                 (rows->lists
                                  (query-rows (get-sourcery-connection)
                                              (format (string-append "SELECT count(*) "
                                                                     "FROM sqlite_master WHERE "
                                                                     "type='table' AND name='~a'")
                                                      (first s-s-i)))))))]
              (if (= table-count 1)
                  (let [(table-info (map (λ (r) (list (second r) (third r)))
                                         (rows->lists
                                          (query-rows (get-sourcery-connection)
                                                      (format "pragma table_info(~a)"
                                                              (quote-field (first s-s-i)))))))
                        (dec-info
                         (cons (list SOURCERY_ID_FIELD_NAME "INTEGER")
                               (map list (second s-s-i) (third s-s-i))))]
                    (if (and (= (length table-info) (length dec-info))
                             (andmap equal? table-info dec-info))
                        (void)
                        (error 'sourcery-db
                               (format (string-append "changed over to database with "
                                                      "incompatible table definition: ~a")
                                       (first s-s-i)))))
                  ;; Create the table
                  (begin
                    (let
                        [(creation-string (table-creation-string (first s-s-i)
                                                                 (second s-s-i)
                                                                 (third s-s-i)))]
                      (query-exec (get-sourcery-connection) creation-string))
                    (void)))))
          sourcery-struct-info))
        (void))
      (error 'sourcery-db "sourcery-db must take in a single string")))


;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; safe-struct
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

(define-syntax safe-struct
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
;; sourcery-struct
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-struct
;; Create sourcery-struct database table create, constructors, updator, and accessors
(define-syntax sourcery-struct
  (syntax-parser
    [(_ struct-name:id [(field:id type:id) ...])
     #:with name-create   (format-id #'struct-name "~a-create" #'struct-name)
     #:with name-pred     (format-id #'struct-name "~a?" #'struct-name)
     #:with name-update   (format-id #'struct-name "~a-update" #'struct-name)
     #:with name-unmapped (format-id #'struct-name "~a-unmapped" #'struct-name)
     #:with name-map      (format-id #'struct-name "~a-map" #'struct-name)
     #:with num-fields    (length (syntax->list #`(field ...)))
     (let* [(struct-arg-checker  (create-arg-type-checker #'struct-name #'(field ...) #'(type ...)))
            (struct-name-string (id->string #'struct-name))
            (table-count (syn-table-count struct-name-string #'(field ...) #'(type ...)))]
       
       
       (begin
         
         ;; Check field names are valid
         (map field-valid? (map id->string (syntax->list #'(field ...))))
         
         #`(begin
             ;; Create a syntax object for the struct-name to be used for arrows
             (define-syntax struct-name #,(id->string #'struct-name))
           
             ;; Check sourcery-struct does not already exist      
             #,(when (sourcery-struct-exists? struct-name-string)
                 (error (string-append struct-name-string ":")
                        "multiple sourcery-struct definitions"))

             ;; Check sourcery-struct does not share a name with a struct
             #,(when (struct-defined? struct-name-string)
                 (error (string-append struct-name-string ":")
                        "existing structure with same name previously defined"))
             ;; when / unless
             ;; Check struct has at least one field
             (when (zero? (syntax->datum #'num-fields))
               (error #,(string-append struct-name-string ":")
                      "sourcery-struct must have at least one field"))

          
             ;; Check types of fields in structure defition
             (let [(res #,(first-failing (compose validate-type symbol->string syntax->datum)
                                         (compose symbol->string syntax->datum)
                                         (syntax->list #'(type ...))))]
               (unless (equal? #false res)
                 (error #,(string-append struct-name-string ":")
                        (format "bad type given in sourcery-struct definition ~a: ~a"
                                #,struct-name-string res))))

             ;; Check structure definition does not overwrite previous declaration in database
             #,table-count 
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
                                                  "[(field type) (field type) ...])"))]))
                                                          

(define-for-syntax (syn-table-count struct-name-string field* type*)
  (with-syntax ([(field ...) field*]
                [(type ...)  type*])
    #`(let [(table-count (first
                          (first
                           (rows->lists
                            (query-rows (get-sourcery-connection)
                                        (format (string-append "SELECT count(*) "
                                                               "FROM sqlite_master WHERE "
                                                               "type='table' AND name='~a'")
                                                #,struct-name-string))))))]
        (if (= table-count 1)
            (let [(table-info (map (λ (r) (list (second r) (third r)))
                                   (rows->lists
                                    (query-rows (get-sourcery-connection)
                                                (format "pragma table_info(~a)"
                                                        #,(quote-field struct-name-string))))))
                  (dec-info
                   (cons (list SOURCERY_ID_FIELD_NAME "INTEGER")
                         (map list
                              (list (symbol->string (syntax->datum #'field)) ...)
                              (list (symbol->string (syntax->datum #'type)) ...))))]
              (unless (and (= (length table-info) (length dec-info))
                           (andmap equal? table-info dec-info))
                (error #,(string-append struct-name-string ":")
                       (format (string-append "defition does not match database table: "
                                              "expects (sourcery-struct ~a ~a")
                               #,struct-name-string
                               (rest table-info)))))
            (void
             ;; Create the table
             #,(let
                   [(creation-string (table-creation-string-syntax #'struct-name
                                                                   #'(field ...)
                                                                   #'(type ...)))]
                 #`(query-exec (get-sourcery-connection) #,creation-string)))))))
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


;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; Sourcery Testing Suite
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; All tests to run
(define sourcery-tests '())

;; Any -> Void
;; Add the given test suite to sourcery-tests
(define (add-to-tests t)
  (begin
    (set! sourcery-tests (cons t sourcery-tests))
    (void)))

;; Syntax -> Void
;; Add a test suite to the sourcery-tests
(define-syntax sourcery-test-suite
  (syntax-parser
    [(_ pieces ...)
     #'(add-to-tests (test-suite pieces ...))]))

;; -> Void
;; Run all sourcery-tests
(define (run-sourcery-tests test-db-path end-db-path)
  (if (and (string? test-db-path) (string? end-db-path))
      (begin
        (sourcery-db test-db-path)
        (to-void (run-tests (make-test-suite "SQLSourcery Tests" sourcery-tests)))
        (sourcery-db end-db-path)
        (void))
      (error 'run-sourcery-tests (format "Given paths must be strings: got ~a and ~a"
                                         test-db-path end-db-path))))

(module+ test
  
  (define PROD_DB "prod.db")
  (define TEST_DB "test.db")

  ;; Database connection and structure definition
  (sourcery-db TEST_DB)
  (sourcery-struct student [(name STRING) (grade INTEGER) (failing BOOLEAN)])
  (sourcery-struct prof [(name STRING)])

  ;; Using testing library with test connection
  (require "../test/sql-sourcery-test-lib.rkt")
  (stest-conn TEST_DB)

  ;; Defile Display/Print/Write files
  (define display-file (gen-output-file "outputs/display.txt"))
  (define dead-ref-display-file (gen-output-file "outputs/display-dead-ref.txt"))

  ;; -----------------------------------------------------------------------
  ;; Results, Setup, and Teardown Library

  ;; Results
  (declare-test-vars bob ben steve bobby sourcery-load-results)

  ;; Setup Functions
  (define-action su-create-bob
    (set-test-var! bob (student-create "Bob Smith" (* 45 2) #false)))

  (define-action su-create-ben
    (set-test-var! ben (prof-create "Ben")))

  (define-action su-create-steve
    (set-test-var! steve (student-create "Steve Steve" 20 #true)))

  (define-action su-update-bob-create-bobby
    (set-test-var! bobby (student-update bob "Bobby Smith" (+ (student-grade bob) 1) #true)))

  (define-action su-update-while-create-joe-john
    (student-update (student-create "Temp Student" -1 #false)
                    "Joe"
                    (student-grade (student-create "John" 100 #f))
                    #t))

  (define-action su-create-joe-delete
    (sourcery-delete (student-create "No Name" (student-grade (student-create "Joe" 1 #true)) #false)))

  (define-action printer-bob-display
    (displayln bob display-file))

  (define-action printer-bob-dead-ref
    (displayln bob dead-ref-display-file))

  (define-action su-delete-bob
    (sourcery-delete bob))

  (define-composed-action su-create-all [su-create-bob su-create-ben su-create-steve])

  (define-action su-load-students
    (set-test-var! sourcery-load-results (sourcery-load student)))

  (define-action su-none)

  ;; Teardown Functions
  (define-action td-clear-all-tables
    (clear-sourcery-structs student prof))

  (define-action td-unset-vars
    (clear-test-vars! bob ben steve bobby sourcery-load-results))

  (define-composed-action td-complete [td-clear-all-tables td-unset-vars])

  (define td-none su-none)

  ;; -----------------------------------------------------------------------
  ;; Language Tests

  (sourcery-test-suite
   "Table Validity"
   #:before su-none
   #:after  td-none
   (check-true (stest-table? "student"))
   (check-equal? (stest-fields "student") 4)
   (check-equal? (stest-field "student" SOURCERY_ID_FIELD_NAME) "INTEGER")
   (check-equal? (stest-field "student" "name") "STRING")
   (check-equal? (stest-field "student" "grade") "INTEGER")
   (check-equal? (stest-field "student" "failing") "BOOLEAN")
   (check-equal? (stest-field "student" "eh") void))


  (sourcery-test-suite
   "Structure Definition Errors"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail? (λ () (begin (sourcery-struct blank []) 1))
              "sourcery-struct must have at least one field"))

  (sourcery-test-suite
   "Structure Definition Compile Time Errors"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail?
              (λ () (compile #'(begin 
                                 (sourcery-struct student [(name STRING)])
                                 (sourcery-struct student [(name STRING)])))))
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-struct))))
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-struct err [(name INVALIDTYPE)]))))
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-struct err_2 [(name)])))))

  (sourcery-test-suite
   "Structure Creation"
   #:before su-create-all
   #:after  td-complete
   (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 1)
   (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "1"))
                 (list 1 "Bob Smith" 90 "FALSE"))
   (check-exn exn:fail? (λ () (student-create 90 90 #false))
              "expected type STRING for name: got 90"))

  (sourcery-test-suite
   "Structure Create Compile Time Errors"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail?
              (λ () (compile #'(student-create))))
   (check-exn exn:fail?
              (λ () (compile #'(student-create "too many" 1 #t "arguments")))))

  (sourcery-test-suite
   "Structure Predicates"
   #:before su-create-all
   #:after  td-complete
   (check-true (prof? ben))
   (check-true (student? bob))
   (check-false (prof? bob))
   (check-false (student? ben))
   (check-false (prof? 1)))

  (sourcery-test-suite
   "Structure Access"
   #:before su-create-all
   #:after  td-complete
   (check-equal? (student-name bob) "Bob Smith")
   (check-equal? (student-grade bob) 90)
   (check-equal? (student-failing bob) #f)
   (check-exn exn:fail? (λ () (student-name ben)) "expected student, given:"))

  (sourcery-test-suite
   "Structure Access Compile Time Errors"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail?
              (λ () (compile #'(student-name))))
   (check-exn exn:fail?
              (λ () (compile #'(student-name bob "extra arguments")))))

  (sourcery-test-suite
   "Structure Update"
   #:before (action-compose su-create-all su-update-bob-create-bobby)
   #:after  td-complete
   (check-equal? bobby bob)
   (check-equal? (student-name bob) "Bobby Smith")
   (check-equal? (student-grade bob) 91)
   (check-equal? (student-failing bob) #true))

  (sourcery-test-suite
   "Structure Creation Within Update"
   #:before su-update-while-create-joe-john
   #:after  td-complete
   (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 1)
   (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) (list 1 "Joe" 100 "TRUE"))
   (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "2")) (list 2 "John" 100 "FALSE")))

  (sourcery-test-suite
   "Structure Update Compile Time Errors"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail?
              (λ () (compile #'(student-update))))
   (check-exn exn:fail?
              (λ () (compile #'(student-update bob "too many" 1 #t "arguments")))))

  (sourcery-test-suite
   "Structure Printer"
   #:before (action-compose su-create-all printer-bob-display)
   #:after  td-complete
   (check-equal? (read-from-file 50 "outputs/display.txt") "#<student: Bob Smith 90 #f>"))

  (sourcery-test-suite
   "Structure Deletion"
   #:before (action-compose su-create-all su-update-bob-create-bobby su-delete-bob printer-bob-dead-ref)
   #:after  td-complete
   (check-exn exn:fail? (λ () (sourcery-delete 1)) "Expected sourcery-struct, got: 1")
   (check-exn exn:fail? (λ () (student-name bob)) "sourcery reference does not exist")
   (check-exn exn:fail? (λ () (student-name bobby)) "sourcery reference does not exist")
   (check-false (student? bob))
   (check-equal? (read-from-file 50 "outputs/display-dead-ref.txt") "#<student: dead-reference>")
   (check-true (sourcery-delete bob))
   (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 0))

  (sourcery-test-suite
   "Creation Within Deletion"
   #:before su-create-joe-delete
   #:after  td-complete
   (check-equal? (length (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) 1)
   (check-equal? (first (stest-rows "student" SOURCERY_ID_FIELD_NAME "1")) (list 1 "Joe" 1 "TRUE")))

  (sourcery-test-suite
   "sourcery-load"
   #:before (action-compose su-create-all su-load-students)
   #:after  td-complete
   (check-equal? (length sourcery-load-results) 2)
   (check-equal? (student-name (first sourcery-load-results)) "Bob Smith")
   (check-equal? (student-name (second sourcery-load-results)) "Steve Steve"))

  (sourcery-test-suite
   "Structure Load Compile Time Errors"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-load))))
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-load "too many" "arguments"))))
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-load not-a-real-struct)))))

  (sourcery-test-suite
   "sourcery-filter-delete None"
   #:before su-create-all
   #:after  td-complete
   (check-equal? (length (sourcery-filter-delete (λ (s) #t) (sourcery-load student))) 2)
   (check-equal? (student-name (first (sourcery-load student))) "Bob Smith")
   (check-equal? (student-name (second (sourcery-load student))) "Steve Steve"))

  (sourcery-test-suite
   "sourcery-filter-delete All"
   #:before su-create-all
   #:after  td-complete
   (check-equal? (length (sourcery-filter-delete (λ (s) #f) (sourcery-load student))) 0))

  (sourcery-test-suite
   "sourcery-filter-delete Some"
   #:before su-create-all
   #:after  td-complete
   (check-equal? (length (sourcery-filter-delete (λ (s) (string=? (student-name s) "Steve Steve"))
                                                 (sourcery-load student)))
                 1)
   (check-equal? (student-name (first (sourcery-load student))) "Steve Steve"))

  (sourcery-test-suite
   "sourcery-dead-reference?"
   #:before (action-compose su-create-all su-delete-bob)
   #:after  td-complete
   (check-true  (sourcery-dead-reference? bob))
   (check-false (sourcery-dead-reference? steve))
   (check-exn exn:fail? (λ () (sourcery-dead-reference? 1)) "Expected sourcery-struct, got: 1"))

  (sourcery-test-suite
   "Struct/sourcery-struct Creation with same names"
   #:before su-none
   #:after  td-none
   (check-exn exn:fail?
              (λ () (compile #'(begin
                                 (struct a [])
                                 (sourcery-struct a [(name STRING)])))))
   (check-exn exn:fail?
              (λ () (compile #'(begin
                                 (sourcery-struct b [(x STRING)])
                                 (struct b [])))))
   (check-exn exn:fail?
              (λ () (compile #'(sourcery-struct x [(__field STRING)]))))
   (check-exn exn:fail?
              (λ () (compile #'((sourcery-struct y [(create INTEGER)
                                                    (update INTEGER)
                                                    (map INTEGER)
                                                    (unmapped INTEGER)]))))))

  (sourcery-test-suite
   ""
   #:before su-create-all
   #:after  td-complete
   )

  ;; Run all tests
  (run-sourcery-tests TEST_DB PROD_DB)

  ;; Delete databases
  (stest-teardown TEST_DB)
  (stest-teardown PROD_DB)

  )