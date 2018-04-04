#lang scribble/manual


@title{SQL-Sourcery}

Database backed structs for functional programmers. 

@section{Grammar}

@(racketgrammar*

  [we
   (can n) put ...]

  [stuff
   (here id expr)
   (and here)])


@section{Connecting to a DB}

The first top level expression in a sql-sourcery program must be a soucery-db statement.
If the database file does not already exist it will be created.

@defform[(sourcery-db db-file)
         #:contracts([db-file string?])]{
 Summons a connection to a SQLite database with the name db-file
}

Example:
@codeblock{(sourcery-db "spells.db")}

@section{sourcery-structs}

Here we get to the magic of SQL-sourcery: database backed structs.

@defform[(sql-sourcery name [(field type) ...])
         #:contracts([name id?]
                     [field id?]
                     [type acceptable-struct-type?])]{
 Create a new structure definition with the given fields and types
 where:
 @(racketgrammar*
   [type
    STRING
    INTEGER
    BOOLEAN])
}
This will allow a sourcerer to use the defined structure in the rest of the program.
If the db backed table does not already exist, it will be created.
If the table already exists and the sourcerer has incorrectly conjured up the struct's
field and types, an error will be raised.

Example:
@codeblock{(sourcery-struct spell [(name STRING) (power INTEGER) (deadly? BOOLEAN)])}

This will create several functions for a sourcerer to use, outlined below

@defform[(struct-name-create field ...)]{
 Where struct-name matches a previously defined sourcery-struct, and
 the types of field... match the types outlined in it's definition.
}

This will summon a new instance of the struct.

@codeblock{(define not-the-bees (spell-create "SUMMON BEES" 1 #false))}


@defform[(struct-name-update struct field ...)]{
 This will update struct s to have the values field ...
}

This will update an existing struct

@codeblock{(spell-update not-the-bees "SUMMON BEES" 1000 #t)}

Structs are also generated with accessors for all of their fields


@defform[(struct-name-field s)]{
 This return the field value of struct s
}

we should probably make this interactive or smthn, idk??
@codeblock{(spell-name not-the-bees)}

@section{Loading Structures}
@defform[(sourcery-load struct-name)]{
 This will return all structs of struct-name that have been created (and not deleted afterwards)
}

@codeblock{(define tome (sourcery-load spell))}

@section{Deleting things}
@defform[(sourcery-delete struct)]{
 This will delete struct from the database
}

@codeblock{(sourcery-delete not-the-bees)}

@defform[(sourcery-filter-delete pref refs)]{
 This will return all references which match the predicate and delete all from the database that don't
}

@codeblock{(define all-bees (λ (s) (spell-deadly? s)) (sourcery-load spell))}

This will delete all spells that aren't deadly

@section{Dead References}

If you delete a refence, all previous references to that object will become dead references,
which can't be used for anything, and I don't know how to better explain them.
