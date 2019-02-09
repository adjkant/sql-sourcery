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
          (delete-file "docs.db")
          (sourcery-db "docs.db")
          (sourcery-struct spell [(name STRING) (power INTEGER) (deadly? BOOLEAN)])]


@;{------------------------------------------------------------------------------------------------}
@;{Documentation Start} 


@title{SQLSourcery}

@author{Adrian Kant and Taylor Murphy}

Database backed structs for functional programmers.
A SQLSourcery programmer is a sourcerer.

@(hyperlink "https://github.com/adjkant/sql-sourcery"
             "Github Repo")

@;{------------------------------------------------------------------------------------------------}
@section{Motivation}

Racket programs require lots of boilerplate for any sort of basic I/O persistency, and getting this
I/O to fit into a functional style causes many additional problems. SQLSourcery attempts to allow a
sourcerer to easily spin up state persistency of their structures that can fit into their coding
paradigms with minimal adaptation.

@;{------------------------------------------------------------------------------------------------}
@section{Database Connection}

SQLSourcery programs must first connect to a SQLite database. It is typical to set the database at the
top of a file or module. Test modules typically use a different database than programs. When a
database is changed, all loaded sourcery structures currently in existence are subject to error. Any
operations besides sourcery structure declarations will throw an error if a database is not set.

@defproc[(sourcery-db [db-file-path string?])
         void]{
 Creates a connection to a SQLite database at the given path location to be used for all
 SQLSourcery operations until the database is changed. Ensure tables exist for all previously defined
 structures in the program.
 
 Example:
 @codeblock{(sourcery-db "spells.db")}
}


@;{------------------------------------------------------------------------------------------------}
@section{Sourcery Structures}


@;{-------------------------------------------------------------}
@subsection{Structure Definition}

A sourcery-struct acts as a typical structure with added database persistence.

@defform[(sourcery-struct struct-name
                          [(field-name type) (field-name type) ...])
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
@section{Testing}

SQLSourcery programs must test mutation and be able to easily write setup and teardown. While possible
with racket, the library can be combersome to use. SQLSourcery comes with a testing library.

@subsection{Testing Philosophy}

There are a few main concepts that come with SQLSourcery's testing library:
@itemlist[@item{@(seclink "TESTING_VARIABLES" "Testing Variables")}
          @item{@(seclink "TESTING_ACTIONS" "Testing Actions")}
          @item{@(seclink "TESTING_SUITES" "Sourcery Test Suites")}]

Sourcery Test Suites uses the racket
@(hyperlink "https://docs.racket-lang.org/rackunit/api.html?q=test-suite#%28form._%28%28lib.
_rackunit%2Fmain..rkt%29._test-suite%29%29"
            "test-suite")
and adds the tests to a module level global lists of tests that can be run with a single command. At
its core, these are most useful for their before and after clauses, which take in
@(seclink "TESTING_ACTIONS" "actions").

In order to take advantage of before and after clauses in test suites, varaibles must be avaiable
in the before, during, and after stages, as well as being able to be modified. While this can be done
with set!, the testing library allows easy declaration, setting, and clearing of testing varaibles
through a simple API that clearly signifies their status for testing and will prevent accidental
mutation of variables not used in testing.

Before and after clauses in
@(hyperlink "https://docs.racket-lang.org/rackunit/api.html?q=test-suite#%28form._%28%28lib.
_rackunit%2Fmain..rkt%29._test-suite%29%29"
            "test-suite")
are thunks that return void. To simplify writing these thunks, the testing library introduces the
concept of actions that can perform multiple operations at once and can be composed together in an
intuitive order for the context of testing.

Testing will use the currently set sourcery-db. It is customary to test by using a test module and
using a testing database via a call to sourcery-db at the start of the module


@;{-------------------------------------------------------------}
@(subsection #:tag "TESTING_VARIABLES" "Testing Variables")

@defform[(declare-test-vars [var-id id?])]{
 Define the given ids to be test variables with the initial value of #f using the racket define.

 @examples[#:eval sourcery-eval
           #:label "Example:"
           (declare-test-vars a b c)
           a
           b
           c]
}

@defproc[(set-test-var! [var-id id?] [value any?]) void]{
 Set the given test varaible to the given value. Will error if the given id is not a test varaible.
 
 @examples[#:eval sourcery-eval
           #:label "Example:"
           (define d #f)
           a
           b
           c
           d
           (set-test-var! a 1)
           a
           (eval:error (set-test-var! d 1))]
}

@defproc[(clear-test-vars! [var-id id?] ...)
         void]{
 Set the given test variable ids to #false. Error if given an invalid test id. If error occurs, all
 ids listed before the invalid id will be set.

 @examples[#:eval sourcery-eval
           #:label "Example:"
           (set-test-var! a 2)
           (set-test-var! b 3)
           (set-test-var! c 3)
           a
           b
           c
           (clear-test-vars! a b)
           a
           b
           c]
}

@;{-------------------------------------------------------------}
@(subsection #:tag "TESTING_ACTIONS" "Testing Actions")

@defproc[(action [expr ...])
         thunk?]{
 Create an action (thunk) that runs all expressions inside a begin and then returns void.

 @examples[#:eval sourcery-eval
           #:label "Example using set!:"
           (define x 1)
           ((action (set! x 2)))
           x]

 @examples[#:eval sourcery-eval
           #:label "Example using test variables:"
           a
           ((action (set-test-var! a 3)))
           a]
}

@defform[(define-action [name id?] [expr any?] ...)]{
 Define an action with the given name.

 Shortcut for:
 @(racketblock (define name (action expr ...)))

 @examples[#:eval sourcery-eval
           #:label "Example using set!:"
           (define x 1)
           (define-action action-1 (set! x 2))
           (action-1)
           x]

 @examples[#:eval sourcery-eval
           #:label "Example using test variables:"
           a
           (define-action action-2 (set-test-var! a 4))
           (action-2)
           a]
}

@defproc[(action-compose [action thunk?] ...)
         thunk?]{
 Create a single action that executes the given actions from left to right.

 @examples[#:eval sourcery-eval
           #:label "Example:"
           (declare-test-vars v1 v2)
           (define-action v-action-1 (set-test-var! v1 1))
           (define-action v-action-2 (set-test-var! v2 2))
           (define-action v-action-3 (set-test-var! v1 3))
           (define v-action-4 (action-compose v-action-1 v-action-2 v-action-3))
           v1
           v2
           (v-action-4)
           v1
           v2]
}

@defform[(define-composed-action [name id?] [[action action?] ...])]{
 Define an action with the given name by composing the given actions.

 @examples[#:eval sourcery-eval
           #:hidden
           (set-test-var! v1 #f)
           (set-test-var! v2 #f)] 

 @examples[#:eval sourcery-eval
           #:label ":Example"
           (define-composed-action v-action-4-easier [v-action-1 v-action-2 v-action-3])
           v1
           v2
           (v-action-4-easier)
           v1
           v2]
}
 

@;{-------------------------------------------------------------}
@(subsection #:tag "TESTING_CLEANUP" "Testing Cleanup")

@defproc[(clear-sourcery-structs [struct-name struct-name?] ...)
         void]{
 sourcery-delete all existing sourcery-structs of the type of the given names. Will possibly
 create @(seclink "DEAD" "dead references").

 @examples[#:eval sourcery-eval
           #:hidden
           (sourcery-filter-delete (λ (x) #f) (sourcery-load spell))] 

 @examples[#:eval sourcery-eval
           #:label ":Example"
           (define alakazam (spell-create "Alakazam" 100 #false))
           (clear-sourcery-structs spell)
           (sourcery-load spell)]
 
 Accessesing @italic{alakazam} will result in:
 @(racketblock (spell 'dead-reference))
}


@;{-------------------------------------------------------------}
@(subsection #:tag "TESTING_SUITES" "Sourcery Test Suites")

@defproc[(sourcery-test-suite [name-expr string?]
                              [maybe-before before-action]
                              [maybe-after after-action]
                              [test test?] ...)
         void]{
                                
 Acts the same as
 @(hyperlink "https://docs.racket-lang.org/rackunit/api.html?q=test-suite#%28form._%28%28lib.
_rackunit%2Fmain..rkt%29._test-suite%29%29"
             "test-suite")
 except that it will add the suite to the global tests.
                                
 Example:
 @(racketblock
   (sourcery-test-suite
    "A sourcery-test-suite using fictional actions"
    #:before su-create-all ;; create spells and load into sourcery-load-results
    #:after  td-complete ;; remove spells and clear all testing varaibles
    (check-equal? (spell-name (first sourcery-load-results)) "Summon Bees")
    (check-equal? (spell-name (second sourcery-load-results)) "Create Pig Tail On A Dursley")))

}

@defproc[(run-sourcery-tests [test-db-path string?] [end-db-path string?]) void]{
 Set the database using sourcery-db to the given test-db-path and run all of the sourcery-test-suites
 defined before the call using
 @(hyperlink "https://docs.racket-lang.org/rackunit/api.html?q=test-suite#%28def._%28%28lib.
_rackunit%2Ftext-ui..rkt%29._run-tests%29%29"
             "run-tests")
 , setting the database using sourcery-db to the given end-db-path after completion.
 
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


@;{
 @;{-------------------------------------------------------------}
 @section{SQLSourcery Paradigms}

 There are a few main paradigms that are suggested for SQLSourcery programs:

 @subsection{Complete Integration}

 TODO

 @subsection{Loading and Saving}

 TODO

 @subsection{Task Scripts}

 TODO

 @subsection{Relational Mapping}

 TODO
}

