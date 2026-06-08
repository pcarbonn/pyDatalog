# Datalog tutorial

pyDatalog is a powerful language with very few syntactic elements, mostly coming from Python : this makes it easy to learn ! In this tutorial, we'll review:

* Variables and expressions
* Loops
* Facts
* Logic Functions and dictionaries
* Aggregate functions
* Literals and sets
* Tree, graphs and recursive algorithms
* 8-queen problem

We'll see that pyDatalog statements are declarative : they describe the result we want, leaving to the computer the task of finding the appropriate solutions. We'll start with trivial problems to show the basics of the language, and progressively address more complex problems, to show how simply they can be expressed. We'll finish with an efficient solution to the 8-queen problem.

The first step is to import pyDatalog:

```python
>>> from pyDatalog import pyDatalog

```

## Variables and expressions

The next step is to declare the variables we'll use. They must start with an upper-case letter:

```python
>>> pyDatalog.create_terms('X,Y')

```

Variables appear in logic queries, which return a printable result

```python
>>> # give me all the X so that X is 1
>>> print(X==1)
X
-
1

```

Queries can contain several variables and several criteria ('&' is read 'and'):


```python
>>> print((X==True) & (Y==False))
X    | Y
-----|------
True | False

```

Note the parenthesis around each equality: they are required to avoid confusion with (X==(True & Y)==False).

Some queries return an empty result :


```python
>>> # give me all the X that are both True and False
>>> print((X==True) & (X==False))
[]

```

Besides numbers and booleans, variables can represent strings. Furthermore, queries can contain python expressions:

```python
>>> # give me all the X and Y so that X is a name and Y is 'Hello ' followed by the first letter of X
>>> print((X==input('Please enter your name : ')) & (Y=='Hello ' + X[0]))
Please enter your name : World
X     | Y
------|--------
World | Hello W

```

In the second equality, X is said to be bound by the first equality, i.e. the first equality gives it a value, making it possible to evaluate the expression in the second equality.

pyDatalog has no symbolic resolver (yet) ! If a variable in an expression is not bound, the query returns an empty solution :

```python
>>> # give me all the X and Y so that Y is 1 and Y is X+1
>>> print((Y==1) & (Y==X+1))
[]

```

Variables can also represent (nested) [tuples](http://www.google.com/url?q=http%3A%2F%2Fdocs.python.org%2F2%2Ftutorial%2Fdatastructures.html%23tuples-and-sequences&sa=D&sntz=1&usg=AOvVaw2qkwzJvKkjTYB43TLfG7IZ), which can participate in an expression and be [sliced](http://www.google.com/url?q=http%3A%2F%2Fstackoverflow.com%2Fquestions%2F509211%2Fpythons-slice-notation&sa=D&sntz=1&usg=AOvVaw1EpNnALvmfyONYZTT0nTfE) ([0-based](https://www.google.com/url?q=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FZero-based_numbering&sa=D&sntz=1&usg=AOvVaw119eanDBSPv2KO4seLKGYe)).

```python
>>> print((X==(1,2)+(3,)) & (Y==X[2]))
X         | Y
----------|--
(1, 2, 3) | 3
```

To use your own functions in logic expressions, define them in Python, then ask pyDatalog to create logical terms for them:

```python
>>> def twice(a):
...     return a+a
...
>>> pyDatalog.create_terms('twice')
>>> print((X==1) & (Y==twice(X)))

X | Y
--|--
1 | 2

```

Note that X must be bound before calling the function.

Similarly, pyDatalog variables can be passed to functions in the Python standard library:

```python
>>> # give me all the X and Y so that X is 2 and Y is the square root of X
>>> import math
>>> pyDatalog.create_terms('math')
>>> print((X==2) & (Y==math.sqrt(X)))

X | Y
--|--------------
2 | 1.41421356237

```

This way, pyDatalog has access to [an extensive toolbox](http://www.google.com/url?q=http%3A%2F%2Fdocs.python.org%2F2%2Flibrary%2F&sa=D&sntz=1&usg=AOvVaw1nOOfUqoujdd7SIrt5oUe1) !

## Loops

Let's first declare the Variables we'll need:

A loop can be created by using the .in() method (we'll see that there are other ways to create loops later):

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X,Y,Z')

>>> # give me all the X so that X is in the range 0..4
>>> print(X.in_((0,1,2,3,4)))
X
-
0
1
3
2
4

```

Here is the procedural equivalent

```python
>>> for x in range(5):
...     print(x)
...
0
1
2
3
4

```

The result of a query is a set of its possible solutions, in random order. Each solution has 1 value for each variable in the query. The .data attribute gives access to the result.

```python
>>> print(X.in_(range(5)).data)
[(0,), (1,), (2,), (3,), (4,)]
>>> print(X.in_(range(5)) == set([(0,), (1,), (2,), (3,), (4,)]))
True

```


Similarly, after a query, a variable contains a tuple of all its possible values. They can be accessed with these methods :

```python
>>> print("Data : ",X.data)
('Data : ', [1, 0, 4, 2, 3])

>>> print("First value : ", X.v())
('First value : ', 1)

```

Below, ' >=' is a variable extraction operator

```python
>>> print("Extraction of first value of X: ", X.in_(range(5)) >= X)
('Extraction of first value of X: ', 2)

```

The '&' operator can be used to filter the result.

```python
>>> # give me all X in range 0..4 that are below 2
>>> print(X.in_(range(5)) & (X<2))
X
-
1
0

```

Loops can easily be nested. Indentation helps reading them :

```python
>>> # give me all X, Y and Z so that X and Y are in 0..4, Z is their sum, and Z is below 3
>>> print(X.in_(range(5))
...           & Y.in_(range(5))
...               & (Z==X+Y) & (Z<3))
X | Y | Z
--|---|--
0 | 1 | 1
1 | 1 | 2
1 | 0 | 1
0 | 2 | 2
0 | 0 | 0
2 | 0 | 2

```

## Logic Functions and dictionaries

As an example, we'll calculate the net salary of employee foo and bar.

Notice that logic function names, such as salary, starts with a lower case.

A function defines one value for a given argument. It is similar to a python dictionary.

A function can be queried.

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X,Y,Z, salary, tax_rate, tax_rate_for_salary_above, net_salary')

>>> salary['foo'] = 60
>>> salary['bar'] = 110

>>> # give me all the X and Y so that the salary of X is Y
>>> print(salary[X]==Y)
X   | Y
----|----
bar | 110
foo | 60

>>> # python equivalent:
>>> _salary = dict()
>>> _salary['foo'] = 60
>>> _salary['bar'] = 110
>>> print(_salary.items())
[('foo', 60), ('bar', 110)]

```


A function has only one value for a given argument.

```python
>>> # foo now has a salary of 70
>>> salary['foo'] = 70
>>> print(salary['foo']==Y)
Y
--
70

>>> # Python equivalent
>>> _salary['foo'] = 70
>>> print('foo --> ' + str(_salary['foo']))
foo --> 70

```


A function can also be queried by value. The following statement is shorter than its Python equivalent :

```python
>>> # give me all the X that have a salary of 110
>>> print(salary[X]==110)
X
---
bar

>>> # procedural equivalent in python
>>> for i, j in _salary.items():
...     if j==110:
...         print(i, '-->', j)
...
bar --> 110

```

Notice that there is a implicit loop in the query.

A query can test the negation of a criteria.

```python
>>> print((salary[X]==Y) & ~(Y==110))
X   | Y
----|---
foo | 70

```

Let's now define a global tax rate. We'll use None for the function argument:

```python
>>> # the standard tax rate is 33%
>>> +(tax_rate[None]==0.33)

```

A function can be called in a formula :

```python
>>> # give me the net salary for all X
>>> print((Z==salary[X]*(1-tax_rate[None])))
X   | Z
----|-----
foo | 46.9
bar | 73.7

```

In this case, X is bound by salary[X], so the expression can be evaluated.

A function can also be defined by a clause. Here is a simple example:

```python
>>> # the net salary of X is Y if Y is the salary of X, reduced by the tax rate
>>> net_salary[X] = salary[X]*(1-tax_rate[None])

>>> # give me all the X and Y so that Y is the net salary of X
>>> print(net_salary[X]==Y)
X   | Y
----|-----
bar | 73.7
foo | 46.9

>>> # procedural equivalent in Python
>>> for i,j in _salary.items():
...     k = j*(1-0.33)
...     print(i, k)
...
foo 46.9
bar 73.7

```

Again, such a function can be queried by value. As an exercise, you are invited to write the procedural equivalent of these queries.

```python
>>> # give me the net salary of foo
>>> print(net_salary['foo']==Y)
Y
----
46.9

>>> print(net_salary[Y]<50)
Y
---
foo

```

Let's now define a progressive tax system: the tax rate is 33 % by default, but 50% for salaries above 100.

```python
>>> # the tax rate for salaries above 0 is 33%, and above 100 is 50 %
>>> (tax_rate_for_salary_above[X] == 0.33) <= (0 <= X)
>>> (tax_rate_for_salary_above[X] == 0.50) <= (100 <= X)

>>> print(tax_rate_for_salary_above[70]==Y)
Y
----
0.33

>>> print(tax_rate_for_salary_above[150]==Y)
Y
---
0.5

```

The '<=' is the important token in the statements above : it is read 'if'.

The most general definition of the function is given first. When searching for possible answers, pyDatalog begins with the last rule defined, i.e. the more specific, and stops as soon as a valid answer is found for the function. So, even though the 2 rules seem to apply for a salary of 150, the second one is actually used to obtain 50 %

Let's now redefine net salary. Before we do, we need to retract our initial definition:

```python
>>> # retract our previous definition of net_salary
>>> del net_salary[X]

```

Here is the new definition;
```python
>>> net_salary[X] = salary[X]*(1-tax_rate_for_salary_above[salary[X]])

>>> # give me all X and Y so that Y is the net salary of X
>>> print(net_salary[X]==Y)
X   | Y
----|-----
foo | 46.9
bar | 55.0

```

Please note that we used f[X]=<expr> above, as a shorter notation for (f[X]==Y) <= (Y==expr)

This short notation, together with the fact that functions can be defined in any order, makes writing a pyDatalog program as easy as creating a spreadsheet.

To illustrate the point, this definition of Factorial cannot be any clearer !

```python
>>> pyDatalog.create_terms('factorial, N')
>>> factorial[N] = N*factorial[N-1]
>>> factorial[1] = 1
>>> print(factorial[3]==N)

N
-
6

```

##  Aggregate functions

Aggregate functions are a special type of functions. Let's first create the data we need to illustrate them.

A basic aggregation is to count the number of results, using len_.

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X,Y,manager, count_of_direct_reports')

>>> # the manager of Mary is John
>>> +(manager['Mary'] == 'John')
>>> +(manager['Sam'] == 'Mary')
>>> +(manager['Tom'] == 'Mary')
>>> (count_of_direct_reports[X]==len_(Y)) <= (manager[Y]==X)

>>> print(count_of_direct_reports['Mary']==Z)
Z
-
2

```

pyDatalog searches all possible solutions for manager[Y]=='Mary', then counts the number of Y.

The aggregate functions are:

* len_ (P[X]==len_(Y)) <= body : P[X] is the count of values of Y (associated to X by the body of the clause)
* sum_ (P[X]==sum_(Y, for_each=Z)) <= body : P[X] is the sum of Y for each Z. (Z is used to distinguish possibly identical Y values)
* min_ , max_ (P[X]==min_(Y, order_by=Z)) <= body : P[X] is the minimum (or maximum) of Y sorted by Z.
* tuple_ (P[X]==tuple_(Y, order_by=Z)) <= body : P[X] is a tuple containing all values of Y sorted by Z.
* concat_ (P[X]==concat_(Y, order_by=Z, sep=',')) <= body : same as 'sum' but for string. The strings are sorted by Z, and separated by ','.
* rank_ (P[X]==rank_(group_by=Y, order_by=Z)) <= body : P[X] is the sequence number of X in the list of Y values when the list is sorted by Z.
* running_sum_ (P[X]==running_sum_(N, group_by=Y, order_by=Z)) <= body : P[X] is the sum of the values of N, for each Y that are before or equal to X when Y's are sorted by Z.
* mean_ and linear_regression : see [our reference](reference.md)

##  Literals and sets

Just as pyDatalog functions behave like Python dictionaries, pyDatalog literals behave like Python sets.

Here is how you add facts to the set.

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X,Y,Z, works_in, department_size, manager, indirect_manager, count_of_indirect_reports')

>>> # Mary works in Production
>>> + works_in('Mary', 'Production')
>>> + works_in('Sam', 'Marketing')
>>> + works_in('John', 'Production')
>>> + works_in('John', 'Marketing')

>>> # procedural equivalent in Python
>>> _works_in = set()
>>> _works_in.add(('Mary', 'Production'))
>>> _works_in.add(('Sam', 'Marketing'))
>>> _works_in.add(('John', 'Production'))
>>> _works_in.add(('John', 'Marketing'))

```

Again, literals can be queried by value, in a way that is shorter than their Python equivalent.

```python
>>> # give me all the X that work in Marketing
>>> print(works_in(X, 'Marketing'))
X
----
John
Sam
>>> # procedural equivalent in Python
>>> for i in _works_in:
...     if i[1]=='Marketing':
...         print(i[0])
...
Sam
John

```

Notice again that there is an implicit loop in the query.

Literals can also be defined by clauses.

```python
>>> # one of the indirect manager of X is Y, if the (direct) manager of X is Y
>>> indirect_manager(X,Y) <= (manager[X] == Y)
>>> # another indirect manager of X is Y, if there is a Z so that the manager of X is Z, # and an indirect manager of Z is Y
>>> indirect_manager(X,Y) <= (manager[X] == Z) & indirect_manager(Z,Y)

>>> print(indirect_manager('Sam',X))
X
----
Mary
John

```

Notice that the use of 2 separate clauses implements an implicit 'or'.

When resolving queries, pyDatalog remembers intermediate results, by a process called [memoization](https://www.google.com/url?q=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FMemoization&sa=D&sntz=1&usg=AOvVaw33Bf4asQhfDfAob9BQwWhQ). This makes resolution faster, but it also helps deal with infinite loops !

```python
>>> # the manager of John is Mary (whose manager is John !)
>>> manager['John'] = 'Mary'

>>> print(indirect_manager('John',X))
X
----
John
Mary

```

This makes pyDatalog a great tool to implement recursive algorithms on complex data structures, e.g. representing networks.

It's also possible to remove facts:

```python
>>> # John does not work in Production anymore
>>> - works_in('John', 'Production')

```

Aggregate functions can be defined on literals too :

```python
>>> (count_of_indirect_reports[X]==len_(Y)) <= indirect_manager(Y,X)

>>> print(count_of_indirect_reports['John']==Y)
Y
-
4

```

##  Tree, graphs and recursive algorithms

Trees and graphs can be represented by the links between their nodes :

```python
>>> pyDatalog.create_terms('link, can_reach')

>>> # there is a link between node 1 and node 2
>>> +link(1,2)
>>> +link(2,3)
>>> +link(2,4)
>>> +link(2,5)
>>> +link(5,6)
>>> +link(6,7)
>>> +link(7,2)

```

This clause specifies that links are bidirectional:

```python
>>> # links are bi-directional
>>> link(X,Y) <= link(Y,X)

```

The following 2 clauses explain how to determine if Y can be reached from X, using recursion.

```python
>>> # can Y be reached from X ?
>>> can_reach(X,Y) <= link(X,Y) # direct link
>>> can_reach(X,Y) <= link(X,Z) & can_reach(Z,Y) & (X!=Y) # via Z_
>>> print(can_reach(1,Y))
Y
-
4
5
6
7
3
2

```

Please note that pyDatalog is smart enough to resolve the query despite the facts that there are loops in the graph.

More example of graph algorithms are available in [this example](https://www.google.com/url?q=https%3A%2F%2Fgithub.com%2Fpcarbonn%2FpyDatalog%2Fblob%2Fmaster%2FpyDatalog%2Fexamples%2Fgraph.py&sa=D&sntz=1&usg=AOvVaw21zU8nc7HfqmdBc0lZRi8E).

## 8-queen puzzle

By combining what we have seen so far, one can program the solution of complex problems in a declarative way, and let the computer find the procedure to solve them.

As an example, let's program an efficient solution to the [8-queen puzzle](https://www.google.com/url?q=https%3A%2F%2Fen.wikipedia.org%2Fwiki%2FEight_queens_puzzle&sa=D&sntz=1&usg=AOvVaw1tVrS-f-Q1XxJbgze3Kwhj). A shorter solution for any N can be found [here](https://www.google.com/url?q=https%3A%2F%2Fgithub.com%2Fpcarbonn%2FpyDatalog%2Fblob%2Fmaster%2FpyDatalog%2Fexamples%2Fqueens_N.py&sa=D&sntz=1&usg=AOvVaw29vZqpr_sRPYQEEiXgx_GH).

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('N,X0,X1,X2,X3,X4,X5,X6,X7')
>>> pyDatalog.create_terms('ok,queens,next_queen')

>>> # the queen in the first column can be in any row
>>> queens(X0) <= (X0._in(range(8)))
>>> # to find the queens in the first 2 columns, find the first one first, then find a second one
>>> queens(X0,X1) <= queens(X0) & next_queen(X0,X1)
>>> # repeat for the following queens
>>> queens(X0,X1,X2) <= queens(X0,X1) & next_queen(X0,X1,X2)
>>> queens(X0,X1,X2,X3) <= queens(X0,X1,X2) & next_queen(X0,X1,X2,X3)
>>> queens(X0,X1,X2,X3,X4) <= queens(X0,X1,X2,X3) & next_queen(X0,X1,X2,X3,X4)
>>> queens(X0,X1,X2,X3,X4,X5) <= queens(X0,X1,X2,X3,X4) & next_queen(X0,X1,X2,X3,X4,X5)
>>> queens(X0,X1,X2,X3,X4,X5,X6) <= queens(X0,X1,X2,X3,X4,X5) & next_queen(X0,X1,X2,X3,X4,X5,X6)
>>> queens(X0,X1,X2,X3,X4,X5,X6,X7) <= queens(X0,X1,X2,X3,X4,X5,X6) & next_queen(X0,X1,X2,X3,X4,X5,X6,X7)
>>> # the second queen can be in any row, provided it is compatible with the first one
>>> next_queen(X0,X1) <= queens(X1) & ok(X0,1,X1)
>>> # to find the third queen, first find a queen compatible with the second one, then with the first# re-use the previous clause for maximum speed, thanks to memoization
>>> next_queen(X0,X1,X2) <= next_queen(X1,X2) & ok(X0,2,X2)
>>> # repeat for all queens
>>> next_queen(X0,X1,X2,X3) <= next_queen(X1,X2,X3) & ok(X0,3,X3)
>>> next_queen(X0,X1,X2,X3,X4) <= next_queen(X1,X2,X3,X4) & ok(X0,4,X4)
>>> next_queen(X0,X1,X2,X3,X4,X5) <= next_queen(X1,X2,X3,X4,X5) & ok(X0,5,X5)
>>> next_queen(X0,X1,X2,X3,X4,X5,X6) <= next_queen(X1,X2,X3,X4,X5,X6) & ok(X0,6,X6)
>>> next_queen(X0,X1,X2,X3,X4,X5,X6,X7) <= next_queen(X1,X2,X3,X4,X5,X6,X7) & ok(X0,7,X7)
>>> # it's ok to have one queen in row X1 and another in row X2 if they are separated by N columns
>>> ok(X1, N, X2) <= (X1 != X2) & (X1 != X2+N) & (X1 != X2-N)
>>> # give me one solution to the 8-queen puzzle
>>> print(queens(X0,X1,X2,X3,X4,X5,X6,X7).data[0])
(4, 2, 0, 6, 1, 7, 5, 3)

```