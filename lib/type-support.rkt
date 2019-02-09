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

;; Syntax Syntax Syntax -> [Syntax-of [[List-of Any] String -> Void]]
;; create function to check arguments for structure creation
(define-for-syntax (create-arg-type-checker struct-name fields types)
  (let* [(field-strings (map symbol->string (map syntax->datum (syntax->list fields))))
         (type-strings (map symbol->string (map syntax->datum (syntax->list types))))
         (type-preds (map type->predicate type-strings))
         (struct-name-string (id->string struct-name))]
    #`(lambda (args call)
      (begin
        (map (λ (f t pred a)
               (if (pred a)
                   (void)
                   (error (string-append #,struct-name-string "-" call ":")
                          (format "expected type ~a for ~a: got ~v"
                                  t f a))))
             #,(cons #'list field-strings)
             #,(cons #'list type-strings)
             #,(cons #'list type-preds)
             args)
        (void)))))


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

;; SupportedStructType -> SQLData
;; Format a piece of syntax that maps to a SupportedStructType into its matching format
;; by type as determined by type predicate
(define (format-sql-type data)
  (let* [(stx-type (filter (λ (t) ((second t) data)) TYPES))]
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

  



