Features
--------

pyDatalog is `Datalog <http://en.wikipedia.org/wiki/Datalog/>`_ embedded in python :

* you can assert and retract facts: ``+ p(a)`` 
* you can assert logic clauses of the form ``head <= body``, where head is a single predicate with one or more variables, and body is a list of predicates separated by '&' : ``p(X) <= q(X) & R(X)`` 
* each variable in the head must also appear in the body.  The body may include equality and comparison predicates : ``N1 == N-1``, or ``N > 0``
* the argument of a predicate cannot be another predicate : ``p(q(a))`` is not allowed
* a predicate cannot be negated : ``not(p(X))`` is not allowed

pyDatalog is fast and lightweight:

* it uses the `datalog engine <http://www.ccs.neu.edu/home/ramsdell/tools/datalog/>`_ developed in `lua <http://en.wikipedia.org/wiki/Lua_(programming_language)>`_ by `John D. Ramsdell <http://www.ccs.neu.edu/home/ramsdell/papers/index.html>`_.  Lua is well-known for its fast, lightweight virtual machine.
* the engine is executed by `luajit <http://luajit.org/install.html>`_, a JIT compiler for lua, wrapped for python by `lupa <http://pypi.python.org/pypi/lupa/>`_
* it uses `memoization <http://en.wikipedia.org/wiki/Memoization>`_
* it has less than 2 K lines of code.

pyDatalog is open-source (LGPL).

Tutorial
========
First, create an instance of the pyDatalog engine:: 

 #instantiate the engine
 from pyDatalog import pyDatalog
 datalog_engine = pyDatalog.Datalog_engine()

The engine can store facts and clauses.  The following statement inserts the fact that Bill is the parent of John Adams::

 datalog_engine.execute("+ parent(bill,'John Adams')")

Note that the unquoted names must start with lower cases, and that quotes must be used for string with spaces or starting with an uppercase letter.  

You can now query our database of facts ::

 print datalog_engine.ask('parent(bill,X)')

It will return a set containing one element: the ('bill', 'John Adams') tuple.  Note that variables in the query start with an upper case, as is customary in prolog.

Logic clauses make the engine smarter.  The following program explains recursively when X is an ancestor of Y : either X is the parent of Y, or X is the parent of a person Z who is an ancestor of Y.  Although it could be entered with the execute function we just used, it is often easier to create a pyDatalog program, as follows::

 # specify what an ancestor is
 @pyDatalog.program(datalog_engine)
 def _():
    ancestor(X,Y) <= parent(X,Y)
    ancestor(X,Y) <= parent(X,Z) & ancestor(Z,Y)

The first line says that the next function is a datalog program : it activates the pyDatalog syntax.  The argument in that first line specifies that the program will be run by the datalog engine that we have created.  

A query of ancestor(bill, X) will now return a set containing one element: the ('bill', 'John Adams') tuple.

You can use python variables in the datalog program, e.g. to bulk-load facts::

 # bulk-load parents
 _parents = (('edward', 'albert'), ('edward', 'victoria'))
 
 @pyDatalog.program(datalog_engine)
 def _(): 
    for _parent in _parents:
        + parent(_parent[0], _parent[1])

Note that python variables must be prefixed with _, to distinguish them from datalog symbols.

The following example illustrates the use of formula::

 # use expressions and recursion to evaluate factorials
 @pyDatalog.program(datalog_engine)
 def _(): 
    factorial(N, F) <= (N < 2) & (F==1)
    factorial(N, F) <= (N > 1) & (N1 == N-1) & factorial(N1, F1) & (F == N*F1)

Note that equality/comparison predicates must be placed between parenthesis, and that the left hand side of the equality/comparison must be a variable.

Installation
============
pyDatalog is based on `C extensions of python <http://docs.python.org/install/index.html>`_.  

* install a C compiler for your platform 

  * for Windows, Visual C++ `Express Edition <http://www.microsoft.com/Express/VC/>`_ is recommended, others include `Windows SDK <http://msdn.microsoft.com/en-us/windowsserver/bb980924.aspx>`_, `minGW <http://www.mingw.org/wiki/InstallationHOWTOforMinGW>`_ or `CygWin <http://www.cygwin.com/install.html>`_

* download and unpack `lupa <http://pypi.python.org/pypi/lupa/>`_
* download and unpack `luajit <http://luajit.org/install.html>`_ in a subdirectory of lupa, and build it
* build and install lupa
* finally, download pyDatalog

I found these resources helpful in resolving problems on Windows :

* `luaJIT binaries <http://sourceforge.net/p/safelua/wiki/LuaJIT%20binaries/>`_ (at your own risk)
* `a library of binaries <http://www.lfd.uci.edu/~gohlke/pythonlibs/>`_.  (luajit is not yet available, though)
* `a solution to a Vcvarsall problem <http://blog.eddsn.com/2010/05/unable-to-find-vcvarsall-bat/>`_
* `hack when lupa can't find luajit2 <http://www.blitzbasic.com/Community/posts.php?topic=89186>`_