#lang racket

(provide struct-field-values
         sourcery-struct-info
         update-sourcery-struct-info
         get-sourcery-struct-info
         sourcery-struct-exists?
         (struct-out sourcery-ref)
         valid-sourcery-ref?

         add-defined-struct
         struct-defined?)

(require "type-support.rkt"
         "sql.rkt"
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
        (struct-field-values (sourcery-ref-table obj) (sourcery-ref-id obj)))))])


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
                            sourcery-struct-info)))
            (row (get-row table id))]
         (if row
             (translate-types row (third sourcery-struct-i))
             (list 'dead-reference)))]))

;; Any -> Boolean
;; determine if something is a valid sourcery ref
(define (valid-sourcery-ref? x)
  (and (sourcery-ref? x)
       (get-row (sourcery-ref-table x) (sourcery-ref-id x))))
  

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

;; String -> Boolean
;; check if the given name is an existing sourcery struct
(define (sourcery-struct-exists? name)
  (let [(s-s-i (filter (λ (x) (string=? name (first x))) sourcery-struct-info))]
    (= 1 (length s-s-i))))

;; String -> SQLSourceryStructInfo
;; get the given struct info by name
(define (get-sourcery-struct-info name)
  (let [(s-s-i (filter (λ (x) (string=? name (first x))) sourcery-struct-info))]
    (if (= 1 (length s-s-i))
        (first s-s-i)
        (error 'sourcery-struct (format "struct does not exist: ~a" name)))))


;; -----------------------------------------------------------------------
;; Struct Name Tracking

;; A list of strings representing the names of all defined structures
(define defined-structs '())

;; String -> Void
;; Add the given name to the defined structs
(define (add-defined-struct name)
  (set! defined-structs (cons name defined-structs)))

;; String -> Boolean
;; Does this name exist as a previously defined structure?
(define (struct-defined? name)
  (member name defined-structs))

