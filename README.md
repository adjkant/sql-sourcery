# SQLSourcery

### Overview
SQLSourcery is an ORM for mapping structures in Racket to database entries in a SQL database. Structure definitions map to tables and instances of each structure definition map to a row in the respective table. Changes are propagated through functional constructs in Racket.

### Current Status

SQLSourcery is still in a prototype state under active development in conjunction with [WebSourcery](https://github.com/adjkant/sql-sourcery). Currently the basic structure mapping is complete along with basic tools for working with SQLSourcery structures. The language also includes testing tooling.

### Roadmap

##### Upcoming Features
- [ ] unmapped structures + tooling support (unmap, map, mapped?)
- [ ] Allowing atomic user-defined types
- [ ] Structures within structures support
- [ ] List Support
- [ ] Cache layer for references to avoid disc reads / SQL execution
- [ ] SQL error handling support

##### Programmer Tooling

- [ ] Create if exists structure create option
- [ ] easy sourcery-struct equality check
- [ ] Edit sourcery-load to include filtering during loading
- [ ] reference-equal?

##### Code improvement
- [ ] Breaking up monolithic constructs using previously unused racket tooling
- [ ] Better use of the db and sql libraries
