### SQLSourcery Summary
*SQLSourcery* will map structures in a racket-like language to database entries in a SQL database. Structure definitions will map to tables and instances of each structure definition will map to a row in the respective table. The language will allow changes in the structures in the program to be propagated back to the database in various methods and approaches. Any *SQLSourcery* program will be able to specify a database to use or load from an already created *SQLSourcery* database into the program.


### SQLSourcery Planned Milestones

#### 1. Language Basics
The basic language will aim to properly import a basic functionality similar to BSL, allow single level structure types (no structures within structures) to be defined and linked to a SQLite database (both saved and loaded), and allow for changes in the program to be propagated to the database according to basic user specifications.

##### Current Roadmap
- [x] Basic DB connectivity and spin-up
- [x] Table creation on structure definition
- [x] Add sourcery-id's to tables
- [x] add basic language constructs
- [X] translate mapped structures to sourcery-ref
- [X] structure create
- [X] proper structure printing
- [X] structure accessors
- [X] structure predicates
- [X] structure update
- [ ] structure delete
- [ ] sourcery-map and sourcery-filter
- [ ] sourcery-load

##### Unfinished:
- [ ] verify all sourcery-structs are at the top level / start of file
- [ ] check existing table declarations match current declarations
- [ ] check field name validity and translate valid struct names (no sourcery-id, no underscore, no question mark, etc)

#### 2. Semantic Extension
Once successful, we aim to extend the language with helpful constructs to allow programmers to write semantic programs with syntax and paradigms matching classic lisp conventions while maintaining the mapping and mutability of the database structures. Possible extensions include:
- [ ] add user testing library
- [ ] unmapped structures and abilty to easily map as needed
- [ ] Allow database modification to be reset on error to avoid script errors causing database duplication or data vailidy corruption

#### 3. Improving Usability and Increasing Complexity
Once this has been accomplished, we will consider developing the following features:
- [ ] Allowing classical structure use within programs for complex operations
- [ ] Allowing user-defined atomic types
- [ ] Supporting mappings for structures within structures
- [ ] Expand the language features to closer match Racket than BSL

#### 4. Additional Ambitions

If time permits, we will attempt to add the following:
- [ ] Supporting multiple database types beyond SQLite
- [ ] Give historical rollback ability of structures within the program / allow batching
- [ ] Support multiple programs using a single SQLSourcery database at once
- [ ] Life fulfillment


