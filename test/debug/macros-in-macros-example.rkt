#lang racket

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define (checked-constructor-transformer pred?)
    (syntax-parser
      [(_ arg)
       (if (pred? #'arg)
           #'(f arg)
           (error 'foo "bar"))]))
  (define (syntax-number? arg)
    (number? (syntax-e arg))))
    
(define-syntax define-checked-constructor
  (syntax-parser
    #:literals (lambda)
    [(_ name (lambda (v) b))
     #'(begin
         (define (f v)
           b)
         (define-syntax name (checked-constructor-transformer syntax-number?)))]))

(define-checked-constructor foo (lambda (x) x))

(foo 5)
(foo (+ 1 2))