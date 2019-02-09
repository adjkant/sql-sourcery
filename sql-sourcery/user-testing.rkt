#lang racket

(provide 
         ;; Action Creation and Composition
         action
         define-action
         action-compose
         define-composed-action

         ;; Side Effect Testing
         declare-test-vars
         set-test-var!
         clear-test-vars!
         
         ;; Database Teardown
         clear-sourcery-structs)

(require db
         "sourcery-connection.rkt"
         (for-syntax racket
                     syntax/parse
                     "utils.rkt"))


;; -----------------------------------------------------------------------
;; Action Creation and Composition

;; An Action is a thunk lambda that returns void

;; [List-of Action] -> Action
;; Compose a list of actions together into a single action with each given action executing in order
(define (action-compose . actions)
  (λ ()
    (begin
      (map (λ (a) (a)) actions)
      (void))))

;; Any ... -> Action
;; create an action
(define-syntax action
  (syntax-parser
    [(_ expr ...)
     #`(λ () (begin
               expr ...
               (void)))]))

;; Any ... -> Void
;; define an action with the given name
(define-syntax define-action
  (syntax-parser
    [(_ name:id exprs ...)
     #`(define name (action exprs ...))]))

;; Id Action -> Void
;; define a composed action with the given name and comprised of the given actions
(define-syntax define-composed-action
  (syntax-parser
    [(_ name:id [a ...])
     #'(define name (action-compose a ...))]
    [else (error 'define-composed-action "Invalid arguments to define-composed-action")]))


;; -----------------------------------------------------------------------
;; Side Effect Testing

;; All sourcery testing variables
(define sourcery-test-vars '())

;; Add to sourcery-test-vars
;; String -> Void
(define (add-test-vars name)
  (begin
    (set! sourcery-test-vars (append name sourcery-test-vars))
    (void)))

;; Delcare the given names as test variables
;; Id ... -> Void
(define-syntax declare-test-vars
  (syntax-parser
    [(_ name:id ...)
     #`(begin (define name #f) ...
              (add-test-vars (map symbol->string '(name ...))))]
    [else #`(error 'declare-test-vars "Invalid test variable declaration")]))

;; Set a given test variable Id to the given value
;; Id Any -> Void
(define-syntax set-test-var!
  (syntax-parser
    [(_ var:id value)
     #`(if (member #,(id->string #' var) sourcery-test-vars)
           (set! var value)
           (error 'set-test-var! (format "Invalid test variable: ~a" #,(id->string #' var))))]
    [else #`(error 'set-test-var! "Invalid test variable modification delcaration form")]))

;; Clear the values of the given test var Id's
;; Id ... -> Void
(define-syntax clear-test-vars!
  (syntax-parser
    [(_ var:id ...)
     #`(begin (set-test-var! var #f) ...)]))

;; Id -> Void
;; clear all given sourcery-struct's
(define-syntax clear-sourcery-structs
  (syntax-parser
    [(_ struct-name:id ...)
     #`(begin (clear-sourcery-struct struct-name) ...)]
    [else #`(error 'clear-sourcery-structs "Invalid clearing of sourcery structures")]))

;; Id -> Void
;; delete all records from a given table and reset the autoincrement
(define-syntax clear-sourcery-struct
  (syntax-parser
    [(_ struct-name:id)
     (let [(struct-string (id->string #'struct-name))]
       #`(if (stest-table? #,struct-string)
             (begin
               (query-exec (get-sourcery-connection)
                           (format "DELETE FROM ~a"
                                   #,struct-string))
               (query-exec (get-sourcery-connection)
                           (format "UPDATE SQLITE_SEQUENCE SET SEQ=0 WHERE NAME='~a'"
                                   #,struct-string)))
             (error 'clear-sourcery-struct
                    (format "sourcery-struct does not exist: ~a" #,struct-string))))]
    [else #`(error 'clear-sourcery-structs "Invalid clearing of sourcery structures")]))

;; String -> Boolean
;; verify a table exists in the database
(define (stest-table? table-name)
  (= 1
     (length (query-rows (get-sourcery-connection)
                         (format "SELECT name FROM sqlite_master WHERE type='table' AND name='~a'"
                                 table-name)))))



