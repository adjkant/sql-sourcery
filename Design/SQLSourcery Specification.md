SQLSourcery Vocabulary

A Name or a Variable is a sequence of characters, not including a space or one of the following: " , ' ` ( ) [ ] { } | ; #:

A Variable is a Name without preassigned meaning.

A Type is one of Boolean, String, or Number.

A Primitive is a predefined Name with a meaning.

A Primitive is one of:
❏	Number Primitives
❏	=
❏	+
❏	-
❏	*
❏	/
❏	modulo
❏	String Primitives
❏	string=?
❏	string-length
❏	substr
❏	string-append
❏	List Primitives
❏	cons
❏	first
❏	rest
❏	empty
❏	length
❏	append
❏	SQLSourcery Primitives
❏	sourcery-filter
❏	sourcery-map

Primitives will be assigned meaning matching their meaning in BSL.

Values
A Boolean is one of #true or #false.

A Number is an integer or floating point number.

A String is a sequence of characters enclosed by double quotes with backslashes used to escape literal double quotes.

A SourceryStruct is a racket-like structure that allows creation and access to store multiple pieces of data together.

SQLSourcery Grammar

SQLSourceryProgram = Database (Definition | SourceryStruct | Expression)…

Database        = (sourcery-db Variable String)

Definition 	    = (define Variable Expression) 
    | (define (Variable Variable…) Expression)

SourceryStruct  = (sourcery-struct Variable [(Variable Type)…])

Expression 	    = Variable 
   	    | Value
    | (Primitive Expression...)
    | (Variable Expression...) 
    | (cond [Expression Expression]... [Expression Expression])
                | (cond [Expression Expression]... [else Expression])

SQLSourcery Scoping Rules

SQLSourcery Scoping will use the BSL scoping rules as defined in this document.

