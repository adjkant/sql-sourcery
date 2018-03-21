#lang racket

(provide TYPES)  

;; An SQLData is one of:
;; - String
;; - Number

;; A SQLTypeName is one of:
;; - "STRING"
;; - "INTEGER"
;; - "BOOLEAN"

;; A SupportedStructType is one of:
;; - String
;; - Integer
;; - Boolean

;; A SQLSourceryTypeInfo is a:
;; (list SQLTypeName
;;       [Any -> Boolean]
;;       [SupportedStructType -> SQLData]
;;       [SQLData -> SupportedStructType])
;; Interpretation for list:
;; - SQLTypeName is the name of the data
;; - predicate for the SupportedStructType
;; - translator from SupportedStructType to SQLData
;; - translator from SQLData to SupportedStructType



;; [Listof SQLSourceryTypeInfo]
;; All valid SQLSourcery types
(define TYPES
  (list (list "INTEGER"
              integer?
              identity
              identity)
        
        (list "STRING"
              string?
              (λ (s) (format "\"~a\"" s))
              identity)
        
        (list "BOOLEAN"
              boolean?
              (λ (b) (if b "\"TRUE\"" "\"FALSE\""))
              (λ (b) (if (string=? "TRUE" b) #t #f)))))
