#lang scribble/manual

@;{------------------------------------------------------------------------------------------------}
@;{Requirements} 

@(require scribble/example
          racket/sandbox)
@(require "../lib/sql-sourcery.rkt")

@;{------------------------------------------------------------------------------------------------}
@;{Sandbox Setup} 

@(define sourcery-eval
   (parameterize ([sandbox-output 'string]
                  [sandbox-error-output 'string]
                  [sandbox-memory-limit 50])
     (sandbox-path-permissions (list (list 'write "../")))
     (make-evaluator 'racket
                     #:requires (list "../lib/sql-sourcery.rkt")
                     #:allow-for-require (list "../lib/sql-sourcery.rkt"))))

@examples[#:eval sourcery-eval
          #:hidden
          (sourcery-db "docs.db")
          (sourcery-struct spell [(name STRING) (power INTEGER) (deadly? BOOLEAN)])]


@;{------------------------------------------------------------------------------------------------}
@;{Documentation Start} 


@title{SQLSourcery}

@author{Adrian Kant and Taylor Murphy}

Database backed structs for functional programmers.
A SQLSourcery programmer is a sourcerer.

@;{------------------------------------------------------------------------------------------------}
@section{Motivation}

TODO.

@;{------------------------------------------------------------------------------------------------}
@section{Grammar}

@(racketgrammar*

  [we
   (can n) put ...]

  [stuff
   (here id expr)
   (and here)])

@;{------------------------------------------------------------------------------------------------}
@section{Database Connection}

SQLSourcery programs must first connect to a SQLite database. It is typical to set the database at the
top of a file or module. Test modules typically use a different database than programs. When a
database is changed, all loaded sourcery structures currently in existence are subject to error. Any
operations besides sourcery structure declarations will throw an error if a database is not set.

@defproc[(sourcery-db [db-file-path string?])
         void]{
 Creates a connection to a SQLite database at the given path location that to be used for all
 SQLSourcery operations.
 
 Example:
 @codeblock{(sourcery-db "spells.db")}
}


@;{------------------------------------------------------------------------------------------------}
@section{Sourcery Structures}


@;{-------------------------------------------------------------}
@subsection{Structure Definition}

A sourcery-struct acts as a typical structure with added database persistence.

@defform[(sourcery-struct struct-name [(field-name type) ...])
         #:contracts([struct-name id?]
                     [field-name id?]
                     [type acceptable-struct-type?])
         ]{
 @(racketgrammar*
   [type
    STRING
    INTEGER
    BOOLEAN])
}

Example:
@codeblock{(sourcery-struct spell [(name STRING) (power INTEGER) (deadly? BOOLEAN)])}

This will create a new structure source or connect to an existing structure source with an identical
definition. If an existing sourcery-struct deinition with the same name already exists in the database
but the definition is not identical, an error will be raised.

No field name identifier can start with the characters "__", nor can they be one of the
following reserved fields:

@itemlist[@item{update}
          @item{create}
          @item{map}
          @item{unmapped}]

A sourcery-struct definition will create the following functions:

@defproc[(struct-name? [x any?])
         boolean?]{
 A predicate for a structure. Will return true only when its argument is of the type struct-name.
}

@defproc[(struct-name-create [field any?] ...)
         struct-name?]{
 Creates a new instance of the structure in both the program and the database. The number of
 arguments and the type of each argument must match the sourcery-struct definition.

 Example:
 @codeblock{(spell-create "Summon Bees" 100 #true)}
 Result:
 @(racketblock (spell "Summon Bees" 100 #true))
}

@defproc[(struct-name-update [ref-struct struct-name?] [field any?] ...)
         struct-name?]{
 Updates the given structure in the database to have the given field values and return the
 newly created structure in the program. All references to the given structure will also be changed.

 Example:
 @examples[#:eval sourcery-eval
           #:label #f
           #:no-prompt
           (define bees     (spell-create "Summon Bees" 100 #true))
           (define bees-new (spell-update bees "Summon Bees" 200 #true))]

 @examples[#:eval sourcery-eval
           #:label #f
           (spell-power bees-new)
           (spell-power bees)]

 Here both bees and new-bees reference the same spell.
}

@defproc[(struct-name-field [s struct-name?])
         any?]{
                                
 Creates accessors for each field are generated as the racket
 @(hyperlink "https://docs.racket-lang.org/reference/define-struct.html?q=struct#%28form._%28%28lib.
              _racket%2Fprivate%2Fbase..rkt%29._struct%29%29"
             "struct")
 construct does.
                                
 @examples[#:eval sourcery-eval
           #:label "Examples:"
           (spell-name bees)
           (spell-power bees)
           (spell-deadly? bees)]                          
}


@;{-------------------------------------------------------------}
@subsection{Loading Structures}

@defproc[(sourcery-load [struct-name id?])
         (listof struct-name)]{
 Return a list of all of the structures currently in existence in the current database.

 @examples[#:eval sourcery-eval
           #:hidden
           (sourcery-filter-delete (λ (x) #f) (sourcery-load spell))] 
 
 @examples[#:eval sourcery-eval
           #:label "Example:"
           #:no-result
           (spell-create "Expecto" 10 #false)
           (spell-create "Patronum" 100 #true)
           (sourcery-load spell)]

 Result:
 @(racketblock (list (spell "Expecto" 10 #false) (spell "Patronum" 100 #true)))
}


@;{-------------------------------------------------------------}
@subsection{Deleting Structures}

@defproc[(sourcery-delete [sourcery-struct sourcery-struct?])
         void]{
 Delete the given sourcery-struct from the database. Any existing references to the sourcery-struct
 will become @(seclink "DEAD" "dead references"). 

 @examples[#:eval sourcery-eval
           #:label "Example:"
           #:no-result
           (define bees (spell-create "Summon Bees" 100 #true))
           (sourcery-delete bees)]

 Attempting to operate on @italic{bees} will result in an error.

 Attempting to display @italic{bees} will result in:
 @(racketblock (spell 'dead-reference))
}

@defproc[(sourcery-filter-delete [predicate (-> sourcery-struct? boolean?)]
                                 [refs (list-of sourcery-struct)])
         (list-of sourcery-struct)]{
 Return all sourcery-struct's which match the predicate and sourcery-delete the structures that
 fail the predicate.

 @examples[#:eval sourcery-eval
           #:hidden
           (sourcery-filter-delete (λ (x) #f) (sourcery-load spell))] 

 @examples[#:eval sourcery-eval
           #:label "Example:"
           #:no-result
           (spell-create "Expecto" 10 #false)
           (spell-create "Patronum" 100 #true)
           (sourcery-filter-delete (λ (s) (spell-deadly? s)) (sourcery-load spell))]

 Result:
 @(racketblock (list (spell "Patronum" 100 #true)))

 The example above deletes all spells that are not deadly.
}

@;{------------------------------------------------------------------------------------------------}
@(section "Programming with SQLSourcery")

@;{-------------------------------------------------------------}
@subsection{SQLSourcery, Side Effects, and Mutation}

By nature, SQLSourcery is at odds with one of the often noted rules and advantages of functional
programming - that mutation and side effects should be avoided when possible.

However, this does not mean that programmers should be forced to change the way they write their
functional code, only that a few additional considerations must be made in order to get the advantages
of database backing. This section is dedicated to explaining how programmers can think about these
issues while still writing idiomatic code.

@;{-------------------------------------------------------------}
@subsection{Under The Hood}

At its core, SQLSourcery simply translates all operations with structures into SQL queries. It then
introduces unique numeric idenifiers that are tied to each structure. Instances of structures are in
fact simply references to these identifiers. Programming with sourcery structures is thus working
with references, not values. While SQLSourcery allows for multiple references to the same value, it
is designed so that there is no need use the feature, and one can write fully functional code, which
then tracks its state with the connected database.

@;{-------------------------------------------------------------}
@(subsection #:tag "DEAD" "Dead References")

Since sourcery structures at their core are references, and the values that a reference points to can
be modified or deleted, it is then possible to have an invalid or "dead" reference. When
sourcery-delete is used, any remaining references then become dead references. If a sourcerer chooses
to use multiple references to the same value, it is the responsibility of the sourcerer to keep track
of the status of references.


@;{-------------------------------------------------------------}
@subsection{SQLSourcery Paradigms}

There are a few main paradigms that are suggested for SQLSourcery programs:

@subsubsection{Complete Integration}

TODO

@subsubsection{Loading and Saving}

TODO

@subsubsection{Task Scripts}

TODO

@subsubsection{Relational Mapping}

TODO


