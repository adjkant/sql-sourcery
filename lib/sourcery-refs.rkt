#lang racket

(provide struct-field-values
         sourcery-struct-info
         update-sourcery-struct-info
         (struct-out sourcery-ref))

(require "sourcery-connection.rkt"
         "type-support.rkt"
         "utils.rkt"
         db
         racket/struct
         (for-syntax syntax/parse))

;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------
;; sourcery-ref
;; -----------------------------------------------------------------------
;; -----------------------------------------------------------------------

;; sourcery-ref: structure to represent table rows and print them as structures
(struct sourcery-ref [table id]
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (sourcery-ref-table obj))
      (lambda (obj)
        (append
         (struct-field-values (sourcery-ref-table obj) (sourcery-ref-id obj))))))])


;; -----------------------------------------------------------------------
;; Sourcery References

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

;; -----------------------------------------------------------------------
;; SQLSourcery Struct Info

;; A SQLSourceryStructInfo is a:
;; - (list String [List-of String] [List-of SQLTypeName])
;; Interpretation for list:
;; - the first item is the name of the struct
;; - the second item is the list of field names
;; - the third item is the list of types for the given fields

;; [List-of SQLSourceryStructInfo]
;; the sorucery structs defined at runtime
(define sourcery-struct-info '())

;; SQLSourceryStructInfo -> Void
;; add the given struct info to the runtime environment
(define (update-sourcery-struct-info to-add)
  (set! sourcery-struct-info (cons to-add sourcery-struct-info)))

