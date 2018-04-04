#lang racket

(provide check-equal?
         check-eq?
         check-not-equal?
         check-exn
         exn:fail?
         check-true
         check-false

         ;; Test Suite Generation and Execution
         sourcery-test-suite
         run-sourcery-tests

         ;; Action Creation and Composition
         action
         define-action
         action-compose

         ;; Side Effect Testing
         declare-test-vars
         set-test-var!
         clear-test-vars

         ;; Database Teardown
         clear-sourcery-structs)

(require rackunit
         rackunit/text-ui
         db
         "sourcery-connection.rkt"
         (for-syntax racket
                     syntax/parse
                     "utils.rkt"))


;; -----------------------------------------------------------------------
;; Automatically Run Test Suite

(define-syntax sourcery-test-suite
  (syntax-parser
    [(_ pieces ...)
     #'(add-to-tests (test-suite pieces ...))]))

(define-syntax run-sourcery-tests
  (syntax-parser
    [(_)
     #'(begin
         (define unsuccessful-tests (run-tests (make-test-suite "SQLSourcery Tests" all-tests)))
         (void))]))

;; Runtime Test Suite Library
(define all-tests '())

(define (add-to-tests t)
  (set! all-tests (cons t all-tests)))

;; -----------------------------------------------------------------------
;; Action Creation and Composition

(define (action-compose . actions)
  (λ () (begin
          (map (λ (a) (a)) actions)
          (void))))

(define-syntax action
  (syntax-parser
    [(_ actions ...)
     #`(λ () (begin
               actions ...
               (void)))]))

(define-syntax define-action
  (syntax-parser
    [(_ name:id actions ...)
     #`(define name
         (λ () (begin
                 actions ...
                 (void))))]))


;; -----------------------------------------------------------------------
;; Side Effect Testing
(define test-vars '())

(define (add-test-vars name)
  (set! test-vars (append name test-vars)))

(define-syntax declare-test-vars
  (syntax-parser
    [(_ name:id ...)
     #`(begin (define name #f) ...
              (add-test-vars (map symbol->string '(name ...))))]
    [else #`(error 'define-test-var "Invalid test variable declaration")]))

(define-syntax set-test-var!
  (syntax-parser
    [(_ var:id value)
     #`(if (member #,(id->string #' var) test-vars)
           (set! var value)
           (error 'modify-test-var! (format "Invalid test variable: ~a" #,(id->string #' var))))]
    [else #`(error 'define-test-var "Invalid test variable modification")]))

(define-syntax clear-test-vars
  (syntax-parser
    [(_ var:id ...)
     #`(begin (set-test-var! var #f) ...)]))

;; String -> Boolean
;; delete all records from a given table and reset the autoincrement
(define-syntax clear-sourcery-structs
  (syntax-parser
    [(_ struct-name:id ...)
     #`(begin (clear-sourcery-struct struct-name) ...)]
    [else #`(error 'clear-sourcery-structs "Invalid clearing of sourcery structures")]))

(define-syntax clear-sourcery-struct
  (syntax-parser
    [(_ struct-name:id)
     (let [(struct-string (id->string #'struct-name))]
       #`(if (stest-table? #,struct-string)
             (begin
               (query-exec sourcery-connection
                           (format "DELETE FROM ~a"
                                   #,struct-string))
               (query-exec sourcery-connection
                           (format "UPDATE SQLITE_SEQUENCE SET SEQ=0 WHERE NAME='~a'"
                                   #,struct-string)))
             (error 'clear-sourcery-struct
                    (format "sourcery-struct does not exist: ~a" #,struct-string))))]
    [else #`(error 'clear-sourcery-structs "Invalid clearing of sourcery structures")]))

;; String -> Boolean
;; verify a table exists in the database
(define (stest-table? table-name)
  (= 1
     (length (query-rows sourcery-connection
                         (format "SELECT name FROM sqlite_master WHERE type='table' AND name='~a'"
                                 table-name)))))



