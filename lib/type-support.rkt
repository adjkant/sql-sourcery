#lang racket

(provide translate-types
         get-type-info
 
         (for-syntax validate-type
                     type->predicate
                     format-sql-types
                     create-arg-type-checker))

(require "types.rkt"
         (for-syntax racket
                     "types.rkt"
                     "utils-phase-1.rkt"))

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

;; SQLTypeName -> SQLSourceryTypeInfo
;; Get the type info for a SQLTypeName
(define (get-type-info name)
  (let [(type (filter (λ (ti) (string=? (first ti) name)) TYPES))]
    (if (= 1 (length type))
        (first type)
        (error 'sourcery-struct (format "Invalid type: ~a" name)))))

;; String -> Boolean
;; Validate that a given string is a SQLTypeName
(define-for-syntax (validate-type type)
  (ormap (λ (t) (string=? (first t) type)) TYPES))

;; SQLTypeName -> [Any -> Boolean]
;; Turn a given type name into its matching predicate
(define-for-syntax (type->predicate name)
  (let [(name-type (filter (λ (t) (string=? (first t) name)) TYPES))]
    (if (= (length name-type) 1)
        (second (first name-type))
        (error 'sourcery-struct (format "Invalid type: ~a" name)))))

;; -----------------------------------------------------------------------
;; Type Formatting

;; Syntax -> SQLData
;; Format a piece of syntax that maps to a SupportedStructType into its matching format
;; by type as determined by type predicate
(define-for-syntax (format-sql-types stx)
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
(define (translate-type value type)
  (let* [(t-info (filter (λ (t) (string=? type (first t))) TYPES))]
    (if (= (length t-info) 1)
        ((fourth (first t-info)) value)
        (error 'sourcery-struct (format "Invalid translation type: ~a" type)))))

  



