### SQLSourcery Summary
We wish to build the language *SQLSourcery*. *SQLSourcery* will map structures in a racket-like language to database entries in a SQL database. Structure definitions will map to tables and instances of each structure definition will map to a row in the respective table. The language will allow changes in the structures in the program to be propagated back to the database in various methods and approaches. Any *SQLSourcery* program will be able to specify a database to use or load from an already created *SQLSourcery* database into the program.


### SQLSourcery Planned Milestones

#### 1. Language Basics
The basic language will aim to properly import a basic functionality similar to BSL, allow single level structure types (no structures within structures) to be defined and linked to a SQLite database (both saved and loaded), and allow for changes in the program to be propagated to the database according to basic user specifications.

#### 2. Semantic Extension
Once successful, we aim to extend the language with helpful constructs to allow programmers to write semantic programs with syntax and paradigms matching classic lisp conventions while maintaining the mapping and mutability of the database structures.

#### 3. Improving Usability and Increasing Complexity
Once this has been accomplished, we will consider developing the following features:
- [ ] Allowing classical structure use within programs for complex operations
- [ ] Supporting mappings for structures within structures
- [ ] Allow automated testing of database mapping and functionality
- [ ] Expand the language features to closer match Racket than BSL

#### 4. Additional Ambitions

If time permits, we will attempt to add the following:
- [ ] Supporting multiple database types beyond SQLite
- [ ] Give historical rollback ability of structures within the program
- [ ] Support multiple programs using a single SQLSourcery database at once
- [ ] Life fulfillment

