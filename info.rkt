#lang info

(define collection "sql-sourcery")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("doc/sql-sourcery.scrbl" ())))
(define pkg-desc "An ORM for mapping structures in Racket to database entries in a SQL database. All versions 0.1.X are unstable Alpha releases.")
(define version "0.1.0")
(define pkg-authors '(adjkant))
