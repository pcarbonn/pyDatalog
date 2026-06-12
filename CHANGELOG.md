# Change log

In bold : changes that may impact existing pyDatalog programs

  Latest

  * Fix "object is not subscriptable" error
  * improve thread safety

  0.22.3 - Jun 12, 2026

  Performance improvements

  0.21.0 - Jun 10, 2026

* **B(X) <= None now clears the predicate without deleting it from Db, returning [] on query instead of raising AttributeError. This change may impact existing pyDatalog programs that expect AttributeError for cleared predicates.** (#31)
* promote `create_symbols` instead of incorrectly named `create_terms`
* fix #4: fix precedence of rules for functions
* fix #19: predicates without assertions in body of rules
* fix #22: support for mod operator (%)
* fix #25: 8-digit precision for floating point calculations
* fix #30: improve Python resolvers
* fix #31: erases rules and facts of a predicate
* fix #36: allow "p, X = create_terms("p, X")"
* add Python typing annotations


  0.20.8 - Jun 2026

* fix #20
* upgrade dependencies
* new documentation site
* new deployment pipeline

  0.17.1 - 24 Jan 2016

* fix 'in' operator in 3.5 by releasing from master branch.

  0.17 - 20 Jan 2016

* promote previous release candidate

  0.17.0rc1 - 14 Dec 2015

* improve documentation, examples
* wheels for Python 3.5 on Windows
* accept `logging.isEnabledFor` to control logging

  0.16.0 - 5 July 2015

* upstage the release candidate version

  0.16.0rc1 - 26 May 2015

* significant rewrite of the resolution engine:
* improve speed of recursive literal. Hashtag_optimized solved under 6 seconds
* improve speed of aggregate functions and clauses with negation, by reusing memoized intermediate results
* enforce order of iterative functions
* fix issue [#7(https://bitbucket.org/pcarbonn/pydatalog/issue/7/hashtag_optimized-finds-a-path-but-not-the) : hashtag_optimized finds a path, but not the shortest one
* fix issue [#14(https://bitbucket.org/pcarbonn/pydatalog/issue/14/incorrect-result-in-special-case-of-a) : incorrect result in special case of a literal with 2 identical variables

  0.15.2 - 1 Mar 2015

* fix [issue #13(https://bitbucket.org/pcarbonn/pydatalog/issue/13/pypi-package-wrong-case-in-setuppy) : PyPi package wrong case in setup.py

  0.15.0 - 14 December 2014

* reset a predicate, for use in interactive consoles : `p(X) <= None`

  0.15.0c1 - 5 October 2014

* add .`sort()` for in-line queries
* improve performance by compiling the resolution engine with [Cython(http://cython.org/)
* [issue #12(https://bitbucket.org/pcarbonn/pydatalog/issue/12/please-stop-printing-version-on-import) : don't show version number on `import pyDatalog`
* [issue #11(https://bitbucket.org/pcarbonn/pydatalog/issue/11/error-in-y-1-x) : error in (Y == 1/X)
* [issue #10(https://bitbucket.org/pcarbonn/pydatalog/issue/10/typeerror-when-using-a-prefixed-resolver) : TypeError when using a prefixed resolver
* [issue #9(https://bitbucket.org/pcarbonn/pydatalog/issue/9/create_terms-for-dynamic-queries) : terms created by create_terms are now recognized by pyDatalog.load
* [issue #8(https://bitbucket.org/pcarbonn/pydatalog/issue/8/unintuitive-errors-on-nested-queries) : support for nested queries (a resolver in python can ask another query)
* add `mean_` and `linear regression_` aggregate function

  0.14.6 - 19 August 2014

* Several fixes, including:
* [issue #4(https://bitbucket.org/pcarbonn/pydatalog/issue/4/variables-leak-information-when-used-in) : max_ aggregate function had side effects on other clauses
* [issue #6(https://bitbucket.org/pcarbonn/pydatalog/issue/6/evaluation-of-body-not-short-circuit) : exceptions were raised in expressions that should not be evaluated
* changeset [59cd787(https://bitbucket.org/pcarbonn/pydatalog/commits/59cd7870c39a2fea7d5324d27aef219a1d6522f9): avoid confusion between similar heads of a functional clause
* add support for slices in pyDatalog programs and dynamic queries
* various code, performance and documentation improvements

  0.14.5 - 23 March 2014

* added support for unicode
* `rank, running_sum`now use `group_by` instead of `for_each`. Improved speed of aggregation
* literals can now have keyword arguments (e.g. `parent(of=X, is_=Y) `)

  0.14.1 - 23 February 2014

* hot fixes :
* fix the plan of resolution of functional clauses, so that the first, most general rule is always evaluated last
* fix initialization bug in Logic()
* support for python classes (e.g. `str()` ) in logic clauses. Allow logic functions `f[X]` in the argument of python calls within logic clauses
* Examples:
* add graph.py to illustrate graph algorithms
* simplify queens_N.py
* illustrate how to interact with users in the tutorial

  0.14.0 - 27 December 2013

* support for calls to standard library in expressions (with bound variables)
* make queries thread-safe
* support for expert systems, throughordered evaluation of the clauses defining a function (resolution stops after the first match)
* support for multiple logic models, through `Logic()`
* `create_atoms()`is replaced by ` create_terms()`
  support for the power operator (e.g.` X*2)`
* `del f[constant] `removes any fact for f[constant]
* `del f[X] `removes any clauses for f[X]
* remove "six" dependency at installation
* small fixes and speed improvements:
* True and 1 should be 2 different constants
* the value extracted by `>= X` should be the same as `X.v()`for queries returning `True`
* add `format_` atom

  0.13.0 - 17 April 2013 ([Documentation](https://sites.google.com/site/pydatalog/reference/reference-0-13))

* support for SQLAlchemy 8.0
* support for (nested) list data structure:
* unification of a variable to a nested list
* unification of 2 nested lists
* support for indexing and slicing of a list
* `len_, range_, tuple_` function
* add generic N-queen example
* support for "existential" literals
* asserting `pos(X, Y) <= (X>0),` then `Pos(1,Y) `will return True
* negated literal in a clause do not bind unbound variables anymore
* add hashtag.py example
* consistent handling of results of both in-line queries and pyDatalog.ask():
* return `[]` if no result found for in-line query (ask() returns `None `in this case)
* return `[()]` if there is no variable in a query that has a result
* return `True `if a variable is left unbound in a query that has a result
* return one column for each variable, in their order of appearance in the query
* pretty print the result of in-line queries
* redesign of the expression engine:
* full support of function of a function, or function of an operation
* support unary operators (+, -)
* evaluate a sub-expression as soon as its variables are bound
* performance improvement
* stop searching when a (sub-)goal is reached
* improve speed of long running queries
* other improvements:
* support for float, decimal
* add `format_(string, value1, value2,..)` for string formatting
* use logging module in Debug and Trace mode
* add arity to name of Python resolver
* align function names to PEP8 standard
* simplify code, improve documentation of source code (see doc.py)

  0.12.0 - 3 Januay 2013 ([documentation](https://sites.google.com/site/pydatalog/reference/reference-0-12))

* add pyDatalog.create_atoms() to support unprefixed inline queries and clauses
* use the `_` prefix for `_len, _sum, _min, _max` to avoid name conflict with Python
* use `._in` and `._not_in` (instead of `in `and `not in`) to avoid name conflict with Python
* update examples to use create_atoms()
* various refactoring and bug fixes

  0.11.2 - 2 Novembre 2012 ([documentation](https://sites.google.com/site/pydatalog/reference/reference-0-11-2))

* add example for mongoDB support
* fix issue with unreferenced pyDatalog.Mixin instances, by running garbage collector before accessing them
* fix "subscript out of range" error with some in-line conjunctive queries
* fix typo in console.py, which also broke datalog.py example
  31 October 2012

rename version 8.1 and 9.0 to 0.8.1 and 0.9.0 on pypi. Build using these specific versions will need to be updated.

  0.11.1 - 26 Septembre 2012

Include "Examples" directory in distribution

  0.11 - 25 September 2012

Prefixed clauses can now be inherited from parent classes.

Intermediate variables are not included in answers anymore.

Improve trace messages in Debug mode

Grammar enhancements:

* support expressions in predicate, e.g. `odd(N-1)`
* accept function, lambda as arguments of functions, e.g. `f[f[X]]==Y`
* negated literal can now have unbound variables. These variables are left in an undefined state, and should not be used further in the clause or query
* support for negation of conjunctions : `~(p(X) & q(X))`
* support for clause retraction, using '`-`', e.g. `-(head <= body)`
* support for `not in` operator, e.g. `X not in (1,2)`

Python resolvers:

* add arity to the name of python resolvers (e.g. `pyD_x _1`)
* use WeakSet for the registry of pyDatalog.Mixin instances
* support for generic resolvers, `cls._PyD_query`

Other:

* test.py now covers 98% of code in pyDatalog.py, pyParser.py, pyEngine.py
* improve queens solution
* move the examples to a separate directory
* fix rare KeyError when creating interned objects
* fix possible stack overflow after resolving an aggregate clause

  0.10.0 - 5 September 2012

`None`, `0` and `""`:

* `p[X]==None`can now return values.
* fix : `p[X]==0` and `p[X]==""` now return values

Syntax enhancements:

* support for `p[X]<Y+Z`, `a.p[X]<Y+Z`, and `y==f[N-1]`. Query results will have extra columns though.
* support for `X in Y+Z`, `p[X] in Y+Z`, `a.p[X] in Y+Z. ` Query results will have extra columns though.

Other:

* `pyDatalog.variables(n)` is a convenience function that creates n pyDatalog Variables (for use in in-line queries)
* raise an error if the right hand side of a comparison is unbound
* fix the issue with version number when importing pyDatalog
* fix: names starting with a '`_`' are now considered as Variables in pyDatalog program
* fix syntax error in '_example datalog.py" with python 3.2

  9.0 - 30 August 2012

Aggregate function:

* support of `rank(for_each=X, order_by=Y)`
* support of `running_sum(N,` for_each=X, order_by=Y)
* replace `key `by more meaningful names : `sum(X, for_each=Y), concat(X, order_by=Y, sep=Z), min(X, order_by=Y), max(X, order_by=Y)`

In-line queries:

* support for conjunctive queries : `E.p(X) & E.q(X,Y)`
* convenient access to the first variable value : `X.v()` and `E.p(X) >= X`

Performance:

* 20 ... 25 % speed improvement
* support for predicate resolvers written in python

Other:

* detect more syntax errors in clauses and queries
* improved solution to the 8-queens problem

  8.1 - 24 July 2012 ([Documentation](https://sites.google.com/site/pydatalog/roadmap-and-change/documentation-of-version-81))

* add hook for other database connectors
* print Datalog error nicely
* fix rare "string indices must be integers, not Symbol" error
* accept "(X==Y)" where both are unbound
* accept empty string as constant
* pyDatalog._version_ contains the version number

  0.8.0 - 16 July 2012

    allow function within expressions; support direct formula :`a[X] == b[X]2`

* support 'X in range(n)' literals
* improve documentation
* interactive datalog console
* virtual attributes to python classes (e.g. Mary.salary_class)
* allow customisation of Answer class
* simplify code by eliminating lua engine

  0.7.0 - 10 July 2012

* support of function, i.e. predicate with unicity (e.g. `father[x] == v` is equivalent to father(x,v) with unicity per x)
* support for aggregation predicate (len, sum, concat, min, max)
* properly handle python queries with repeated variables (`Employee.manager(X,X)`)
* fix for pyDatalog.clear()

[0.6.0(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.6) - 30 June 2012 ([documentation(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.6))

* use the just-in-time compiler of pypy instead of lupa's. Stop development on lua engine
* support python objects in literals (python engine only)
* support query of relational database via SQLAlchemy
* support trace mode
* Do not accept python variables (starting with _) in pyDatalog.program anymore
* Equality predicate now accepts that both sides are bound
* Accept upper case for predicate initials
* new assert_fact() accepts python objects for terms

[0.5.0(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.5) - 20 June 2012: ([Documentation(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.5))

* support for negation

[0.4.0(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.4) - 3 May 2012: ([Documentation(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.4))

* support multi-line strings in pyDatalog.load()
* go beyond 4 operators in expressions : use lambda

  0.3.2 - 29 Apr 2012:

* index the database of facts for improved performance
* make pyDatalog.clear() work

[0.3.0(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.3) - 25 Apr 2012: ([Documentation(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.3))

* add the datalog engine written in python. Use it by default
* use numeric type in results (instead of string types)
* tested with python 2.7 and 3.2

[0.2.1(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.2) : 16 Apr 2012 ([Documentation(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.2))

* rename "execute()" into "load()"
* avoid stack overflow due to deep recursion
* propose default datalog engine in the decorator, and add pyDatalog.ask(code),

  0.1.1 : 13 Apr 2012

* add links to download in Pypi

[0.1.0(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.1) : first release on Pypi ([Documentation(https://bitbucket.org/pcarbonn/pydatalog/wiki/Documentation%20for%200.1))
