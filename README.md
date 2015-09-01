pyDatalog adds the logic programming paradigm to Python's toolbox, in a pythonic way.  
You can now run logic queries on databases or Python objects, and use logic clauses to define python classes.

Datalog is a truly declarative language derived from Prolog, with strong academic foundations.  It complements Python very well for:

* managing complex sets of related information (e.g. in data integration or the semantic web).  
* simulating intelligent behavior (e.g. in games), 
* performing recursive algorithms (e.g. in network protocol, code and graph analysis, parsing)
* solving discrete constraint problems. 

In particular, pyDatalog can be used as a query language: 

* it can perform multi-database queries (from memory datastore, 11 relational databases, and noSQL database with appropriate connectors)
* it is more expressive than SQL, with a cleaner syntax; 
* it facilitates re-use of SQL code snippet (e.g. for frequent joins or formula); 

Datalog excels at accelerated development : Datalog programs are often shorter than their Python equivalent, 
and Datalog statements can be specified in any order, as simply as formula in a spreadsheet.