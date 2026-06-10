# Datalog logic programming in python

Logic programming in Python

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_symbols('factorial, N')
>>> factorial[N] = N*factorial[N-1]
>>> factorial[1] = 1
>>> print(factorial[3]==N)
N
-
6

```

* **Easy to learn**: Based on programmer-friendly Python.
* **Be productive**: Write short, declarative programs.
* **Batteries included**: With full access to the extensive Python library.
* **Be readable**: Order your statements to be [literate](https://en.wikipedia.org/wiki/Literate_programming).
* **Query 11 SQL databases**: and noSQL, and Python classes.
* **Use AI**: to implement expert systems and complex algorithms.
pyDatalog adds the [logic programming](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FLogic_programming&sa=D&sntz=1&usg=AOvVaw38aTM4tWs-A3dCIxE5BScf) paradigm to [Python](http://www.google.com/url?q=http%3A%2F%2Fwww.python.org%2Fabout%2F&sa=D&sntz=1&usg=AOvVaw1ksRrV0rAqLBwu7RUVFWCb)'s [extensive toolbox](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FPython_%28programming_language%29%23Standard_library&sa=D&sntz=1&usg=AOvVaw3oRAqMQ1pCoEhSk8m4zKH_), in a pythonic way.

Logic programmers can now use the extensive standard library of Python, and Python programmers can now express complex algorithms quickly.

[Datalog ](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FDatalog&sa=D&sntz=1&usg=AOvVaw0fGTkk76owoeDYDBEcTmjb)is a truly declarative language derived from Prolog, with strong [academic foundations](http://scholar.google.be/scholar?hl=en&q=datalog&btnG=&as_sdt=1%2C5&as_sdtp=). Datalog excels at managing complexity. Datalog programs are shorter than their Python equivalent, and Datalog statements can be specified in any order, as simply as formula in a spreadsheet.

pyDatalog can be used for:

  * simulating intelligent behavior (for games or expert systems),
  * querying complex sets of related information (e.g. in data integration or Natural Language Processing),
  * performing recursive algorithms (e.g. on hierarchical data structure)

pyDatalog is derived from [previous work by John D. Ramsdell](http://www.google.com/url?q=http%3A%2F%2Fwww.ccs.neu.edu%2Fhome%2Framsdell%2Ftools%2Fdatalog%2F&sa=D&sntz=1&usg=AOvVaw04Hlic6GgHqGeOid-4DV2_). It is an open-source project (LGPL) lead by Pierre Carbonnelle (in Belgium). It is inspired by [LogicBlox](http://www.google.com/url?q=http%3A%2F%2Fwww.datalog20.org%2Fslides%2Faref.pdf&sa=D&sntz=1&usg=AOvVaw0kTbpEsZsu_cJb8O8boB9O).

## Core technology

pyDatalog uses a tabled resolution algorithm based on SLG resolution, simplified to support negation as failure on stratified programs only.

Core technology in pyDatalog |  Benefits |  Sample applications
  ---|---|---
The resolution engine determines the best sequence of clauses to use to reach a goal | Spreadsheet-style programming : faster development; fewer bugs, easier to read afterwards |  Rule-based models with many input and output, e.g. expert system for [price calculation](https://www.google.be/search?q=expert+system+pricing+) or [tax planning](https://www.google.be/search?q=expert+system+tax+) , [access right management](https://www.google.be/search?q=logic+programming+access+control+policy) , [robot control](https://www.google.be/search?q=logic+programming+robot) , [intelligent agent](https://www.google.be/search?q=+intelligent+agent+logic+programming) in games or automated assistants
The resolution engine can resolve recursive algorithm | Easy to write queries on hierarchical structure | Reporting with hierarchical reference data such as organisational structure
The same clause can solve various mixes of known and unknown parameters | Maximize code reuse : shorter program, simpler query language than SQL |  Cross-database queries, [data integration](https://www.google.be/search?q=logic+programming+data+integration)
Intermediate results are [memoized](https://en.wikipedia.org/wiki/Memoization) .  | Improved speed by avoiding duplicate work. |  Business logic in [3-tier architecture](https://en.wikipedia.org/wiki/Multitier_architecture) .


## Features

pyDatalog embeds logic programming in Python :

  * you can assert facts in a datalog knowledge base, and query it.
  * you can use logic clauses to query any relational database via [SQLAlchemy](http://www.google.com/url?q=http%3A%2F%2Fsqlalchemy.org%2F&sa=D&sntz=1&usg=AOvVaw2JHoX4dozKo_5Ar3AE5XOu), the [data access layer](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FData_access_layer&sa=D&sntz=1&usg=AOvVaw1kbmV8gACA1MeBh723tXA-) for Python which supports [11 dialects of SQL](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Fdialects%2Findex.html&sa=D&sntz=1&usg=AOvVaw1R11TbTcvUqt_09ICbvfqo).
  * you can define attributes of Python classes through [logic clauses](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FClause_%28logic%29&sa=D&sntz=1&usg=AOvVaw2qBGXZrdK4pHv7sErPTC9L), and use logic queries on Python objects.

More specifically:

  * you can use [aggregate functions](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FAggregate_function&sa=D&sntz=1&usg=AOvVaw1gI8iVTehuIhBqGA7G9_Oo) :` len_, sum_, concat_, min_, max_, tuple_, rank_, running_sum_, mean_, linear_regression_`
  * you can define logic functions (e.g. `p[X]==Y`) , i.e. logic predicate with unicity
  * variables can represent nested lists that you can index and slice
  * you can prefix literals and functions with a Python class name (e.g. `Employee.name[X]==Y`): their first argument refers to instances of the class.
  * prefixed literals and functions can be inherited from parent classes.
  * you can use arithmetic literals `(X == Y+1)`. An expression can contain Python functions and [lambda expressions](http://www.google.com/url?q=http%3A%2F%2Fwww.secnetix.de%2Folli%2FPython%2Flambda_functions.hawk&sa=D&sntz=1&usg=AOvVaw0SKVdXRHmllnIH32LT9Sup)
  * you can negate an atom in the body of a clause : `~p(X)`

pyDatalog is a fast and lightweight datalog interpreter written in Python:

  * it can solve the [8-queen problem](http://www.google.com/url?q=http%3A%2F%2Fen.wikipedia.org%2Fwiki%2FEight_queens_puzzle&sa=D&sntz=1&usg=AOvVaw1cCR1YjdlvHRnEfoMT3TO8) in 0.07 seconds on a PC
  * it is thread safe
  * it uses indexes for the database of datalog facts
  * it can run on [pypy](http://www.google.com/url?q=http%3A%2F%2Fpypy.org&sa=D&sntz=1&usg=AOvVaw1rFgGtsjIxzn3EFUx3O7Sb) (Python with Just-In-Time compiler)
  * it has hooks to let you rewrite performance-critical clauses in pure Python
  * it has less than 2 K lines of executable code.

The depth of recursion is not limited by the stack size.

## Testimonials

pyDatalog is cited in [several scientific papers](https://scholar.google.com/scholar?hl=fr&as_sdt=0%2C5&q=pydatalog&btnG=) !

"Very neat.  Datalog rocks" - Marck Carter.  He has written a nice blog entry on unit conversion with Datalog.

"Congratulations for your wonderful work done in pyDatalog project" - Karamajit Kaur, researcher in India

You are also using pyDatalog ? [Let us know](mailto:pierre.carbonnelle@gmail.com).

