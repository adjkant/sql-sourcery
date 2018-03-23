#lang racket

(provide translate-types
         get-type-info
         format-sql-type
         (for-syntax validate-type
                     type->predicate
                     create-arg-type-checker))

(require "types.rkt"
         (for-syntax racket
                     "types.rkt"
                     "utils.rkt"))

;; -----------------------------------------------------------------------
;; Type Checking Generation

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


;; -----------------------------------------------------------------------
;; Basic Type Validation and Information Retreival

;; String -> Boolean
;; Validate that a given string is a SQLTypeName
(define-for-syntax (validate-type type)
  (ormap (λ (t) (string=? (first t) type)) TYPES))

;; SQLTypeName -> [Any -> Boolean]
;; Turn a given type name into its matching predicate
(define-for-syntax (type->predicate name)
  (let [(type-info (get-type-info name))]
    (second type-info)))

;; -----------------------------------------------------------------------
;; Type Formatting

;; Syntax -> SQLData
;; Format a piece of syntax that maps to a SupportedStructType into its matching format
;; by type as determined by type predicate
(define (format-sql-type stx)
  (let* [(data (syntax->datum stx))
         (stx-type (filter (λ (t) ((second t) data)) TYPES))]
    (if (= (length stx-type) 1)
        ((third (first stx-type)) data)
        (error 'sourcery-struct (format "Invalid type: ~a" data)))))


;; -----------------------------------------------------------------------
;; Type Translation

;; [List-of SQLData] [List-of SQLTypeName] -> [List-of SupportedStructType]
;; translate a value to the given type as used in structures
(define (translate-types values types)
  (map translate-type values types))

;; SQLData SQLTypeName -> SupportedStructType
;; translate the given SQLData  into it's SQLSourcery data form given its type
(define (translate-type value type)
  (let* [(t-info (get-type-info type))]
    ((fourth t-info) value)))

  



