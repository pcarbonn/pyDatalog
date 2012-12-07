pyDatalog adds the logic programming paradigm to python's toolbox, in a pythonic way.  
You can now run logic queries on databases or python objects, and use logic clauses to define python classes.

Datalog is a truly declarative subset of prolog that is best at

* managing large sets of related information (e.g. in data integration or the semantic web).  
* simulating intelligent behavior (e.g. in games), 
* performing recursive algorithms (e.g. in network protocol, code and graph analysis)
* solving discrete constraint problems. 

In particular, pyDatalog can be used for object-relational mapping: 

* it can perform multi-database queries (from memory datastore,  relational databases, and noSQL database with appropriate connectors)
* it is more expressive than SQL, with a cleaner syntax; 
* it facilitates re-use of SQL code snippet (e.g. for frequent joins or formula); 
* it offloads the database server by performing joins on the application tier. 

Datalog excels at accelerated development : Datalog programs are often shorter than their python equivalent, 
and Datalog statements can be specified in any order, as simply as formula in a spreadsheet.

See pyDatalog's home page : https://bitbucket.org/pcarbonn/pydatalog/wiki/Home
