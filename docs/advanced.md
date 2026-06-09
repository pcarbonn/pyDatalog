# 4 - Advanced topics

## Input, output, Logging

You can use the Python `input()` function to request data from the user :

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X,Y, quacks, input')
>>> (quacks(X) <= (input('Does a '+X+' quack ? (Y/N) ')=='Y'))

```

Please note that the prompt string contains a variable, X. Without it, e.g. in ` (X==input('Enter your name : ')`, the user would be prompted when the clause is declared, not when it is used during query evaluation.

You can use the `print()` function to show a result on the console:

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('print, ok')
>>> (ok(X) <= (0 < X) & (Y==print(X)))

```

To get a full trace of pyDatalog's reasoning, add the following instructions before a query:

```python
>>> import logging
>>> from pyDatalog import pyEngine
>>> pyEngine.Logging = True
>>> logging.basicConfig(level=logging.INFO)

```

## Thread safety and multi-models

A Python program may start several threads. Each thread should have these statements to initialize pyDatalog :


```python
>>> from pyDatalog import pyDatalog, Logic
>>> Logic() # initializes the pyDatalog engine

```




Each thread can then define its own set of clauses, and run queries against them. See the [ThreadSafe.py](https://www.google.com/url?q=https%3A%2F%2Fbitbucket.org%2Fpcarbonn%2Fpydatalog%2Fsrc%2F0880c4bd92e1b5d42075e46b0e140f6dc8eaa1f5%2FpyDatalog%2Fexamples%2FthreadSafe.py%3Fat%3Ddefault&sa=D&sntz=1&usg=AOvVaw1YwxZpl-2x5CtvBLyJbV89) example.

Alternatively, threads can share the same set of clauses, by following these steps :

  1. the initial thread defines the set of clauses as usual
  2. the initial thread passes the `Logic(True) `object to the threads it creates.
  3. the new thread executes `Logic(arg)`, where `arg `is the `Logic(True) `object it received from the calling thread.

Please note that, once passed, the set of clauses should not be changed (such changes are not thread safe).

Finally, a program may switch from one set of clauses to another :

```python
>>> Logic() # creates an empty set of clauses for use in the current thread
>>> # add first set of clauses here
>>> first = Logic(True) # save the current set of clauses in variable 'first'
>>> Logic() # first is not affected by this statement
>>> # define the second set of clauses here
>>> second = Logic(True) # save it for later use
>>> Logic(first) # now use first in the current thread
>>> # queries will now run against the first set of rules

```

See the [Multi-Model.py](https://www.google.com/url?q=https%3A%2F%2Fbitbucket.org%2Fpcarbonn%2Fpydatalog%2Fsrc%2F0880c4bd92e1b5d42075e46b0e140f6dc8eaa1f5%2FpyDatalog%2Fexamples%2FmultiModel.py%3Fat%3Ddefault&sa=D&sntz=1&usg=AOvVaw3HOpgJM7MbLVtMI_GbEFGh) example for illustration.

## Dynamic datalog statements

Some applications need to construct datalog statements dynamically, i.e. at run-time. The following interface can then be used.

A Python program can assert fact in the in-memory Datalog database using `assert_fact()`. The first argument is the name of the predicate :

```python
>>> from pyDatalog.pyDatalog import assert_fact, load, ask
>>> # + parent(bill, 'John Adams')
>>> assert_fact('parent', 'bill','John Adams')

```

A python program calls the load function to add dynamic clauses :

```python
>>> # specify what an ancestor is
>>> load("""
...     ancestor(X,Y) <= parent(X,Y)
...     ancestor(X,Y) <= parent(X,Z) & ancestor(Z,Y)
... """)

```

It can now query the database of facts :

```python
>>> # prints a set with one element : the ('bill', 'John Adams') tuple
>>> print(ask('parent(bill,X)'))
{('John Adams',)}

```

## Predicate Resolvers written in pure python

A predicate such as` p(X,Y)` or `(a.p[X]==Y)` can be resolved using a custom-made python method, instead of using logical clauses. Such python resolvers can be used to implement connectors to non-relational databases, or to improve the speed of specific clauses. It has precedence over (and replaces) any clauses in the pyDatalog knowledge base.

### Unprefixed predicates

A decorated function can be used to resolve an unprefixed predicate or function. It is defined using the `@pyDatalog.predicate()` decorator:


```python
>>> @pyDatalog.predicate()
... def p(X,Y):
...     yield (1,2)
...     yield (2,3)
...
>>> print(pyDatalog.ask('p(1,Y)'))
{(2,)}

>>> # inline query
>>> print((p(1, Y) & p(1, Y)).ask())
[(2,)]

>>> # inline rule
>>> pyDatalog.create_terms('f_inline')
>>> f_inline(X) <= p(1, X) & p(1, X)
>>> print(f_inline(X).ask())
[(2,)]
```

Note that this resolver also resolves `p[X] == Y`.

This simple resolver returns all possible results, which pyDatalog further filters to satisfy the query.

Often, the resolver will use the value of its X and Y arguments, if any, to return a smaller, (if possible exact) set of results. Each argument has a `.is_const()` method : it returns `True `if it is a constant, and `False `if it is an unbound variable. If it is a constant, the value can be obtained with `.id` (e.g. `X.id`).

## Prefixed predicates

The resolver for a prefixed predicate is defined by a method in a class. The following resolver is equivalent, and replaces, Employee.salary_class[X] = Employee.salary[X]//1000.


```python
>>> class Employee(pyDatalog.Mixin):
...     @classmethod
...     def _pyD_salary_class2(cls, X, Y):
...         if X.is_const():
...             yield (X.id, X.id.salary//1000)
...         else:
...             for X in pyDatalog.metaMixin._refs_[cls]:
...                 Y1 = X.salary // 1000
...                 if not Y.is_const() or Y1==Y.id:
...                     yield (X, Y1)

```

The method `_pyD_salary_class2` will be called to resolve `Employee.salary_class(X,Y)` and `(Employee.salary_class[X]==Y)` queries. Note that the method name is suffixed with the arity of the predicate, here, 2 for `X` and `Y`.

The list of instances in a class `cls `is available in pyDatalog.metaMixin._refs_[cls] . For SQLAlchemy classes, queries can be run on `cls.session`.

A class can also have (or inherit) a generic resolver, called `_pyD_query`. Its arguments are the predicate name and its arguments.


```python
>>> # generic resolver
>>> class Employee(pyDatalog.Mixin):
...     @classmethod
...     def _pyD_query(cls, pred_name, args):
...         if pred_name == "Employee.salary_class":
...             if args[0].is_const():
...                 yield (args[0].id, args[0].id.salary//1000)
...             else:
...                 for X in pyDatalog.metaMixin._refs_[cls]:
...                     Y1 = X.salary // 1000
...                     yield (X,Y1)
...         else:
...             raise AttributeError

```

## Performance

The best performance is obtained with pypy, the JIT compiler of python. Please note that:

  * pypy needs a few seconds to run pyDatalog at top performance;
  * performance of pypy is significantly better when executed from the comand line / shell than in debug mode (e.g. in IDE or Eclipse/pyDev);
  * you should not measure performance of pypy with cProfile, as it prevents the JIT compiler to be effective.

A pyDatalog program will never beat the same program written in pure python, but:

  * a pyDatalog program run with pypy will have approximately the same performance as a program interpreted (uncompiled) by CPython;
  * there is a wide class of applications where speed of development is more important than speed of execution;
  * there is a wide class of application where other software components will be the real bottleneck (e.g. I/O or remote databases);
  * thanks to increasing speed of computers, pyDatalog will run fast enough in many cases;
  * you can first prototype a program in pyDatalog, then rewrite the few performance-critical clauses in pure python, using python resolvers.

Performance tip:

  * The results of the evaluation of clauses in a query are memoized only for the duration of the query. For better performance, combine consecutive queries into one using the "&" operator, or create a clause for the combined result.

## Floating Point Precision

In Datalog, tabled logic programming (memoization) is used to prevent infinite loops when evaluating recursive, cyclic rules. However, when these cyclic rules involve floating-point arithmetic (e.g. `X = Y * 0.5`), floating-point inaccuracies could cause the engine to derive values that differ slightly (e.g., `1.0` vs `1.0000000000000002`). Without intervention, PyDatalog would interpret these as distinct facts and fail to terminate, leading to infinite loops.

To circumvent this, PyDatalog provides a configuration variable to enforce canonicalization of floating-point numbers during evaluation. By default, it restricts comparisons to 8 significant digits to ensure that cyclic paths converge and terminate. Alternatively, fractions can be used.

The following Python statements can be used to control this behavior.

* `pyEngine.Float_Precision = N`: (Default: `8`) PyDatalog will aggressively round all floats to `N` significant digits. This breaks the infinite loop by correctly grouping slightly drifted floats into the same memoization bucket. Note that precision beyond `N` digits is destructively truncated. Best for general use, especially when dealing with messy real-world measurements or mathematically inconsistent hardcoded decimals.
* `pyEngine.Float_Precision = 'Fraction'`: PyDatalog will automatically convert all floats to exact rational `Fraction` objects (`fractions.Fraction`). This preserves perfect accuracy and solves infinite loops without truncating precision. This is highly useful for financial calculations or exact algebraic domains. However, if your starting facts are mathematically inconsistent (e.g. providing slightly rounded conversion rates that don't perfectly cycle back to 1), exact fraction math will faithfully reproduce that inconsistency infinitely and fail to terminate.
* `pyEngine.Float_Precision = None`: Disables floating-point canonicalization. Floats are handled exactly as Python evaluates them, which may lead to infinite loops in cyclic numeric rules.
