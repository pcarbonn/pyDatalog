# 2 Object-oriented Datalog

Python objects can be used in logic facts and clauses, and those clauses can be invoked from simple python statements.

We'll use the following class definition of Employee.

```python
>>> from pyDatalog import pyDatalog, Logic
>>> Logic()  # initialize the pyDatalog engine
>>> class Employee(pyDatalog.Mixin): # --> Employee inherits the pyDatalog capability
...     def __init__(self, name, manager, salary):
...         # call the initialization method of the Mixin class
...         super(Employee, self).__init__()
...         self.name = name
...         self.manager = manager # direct manager of the employee, or None
...         self.salary = salary # monthly salary of the employee
...     def __repr__(self): # specifies how to display an Employee
...         return self.name
...
>>> John = Employee('John', None, 6800)
>>> Mary = Employee('Mary', John, 6300)
>>> Sam = Employee('Sam', Mary, 5900)

```

Let's assert the fact that Mary has a car:

```python
>>> pyDatalog.create_terms('has_car, X, Y, Z')
>>> + has_car(Mary)
>>> print(has_car(X))
X
----
Mary

```

This fact is stored in pyDatalog's knowledge base (not in the Employee object). All the principles explained in [Tutorial 1](https://sites.google.com/site/pydatalog/Online-datalog-tutorial) apply to such predicates whose terms are python objects.

Instead of asserting facts in the pyDatalog knowledge base, it is often more efficient for logic predicates to directly refer to the attribute of an object. For example, we may want to have a clause concerning the salary attribute of Employee instances. This requires 2 things:

1. that the class inherits from pyDatalog.Mixin
2. that the predicate is prefixed by the class nam e, e.g. ( Employee.salary[X] == Y ).

```python
>>> # which employees have a salary of 6300 ?
>>> print(Employee.salary[X]==6300)
X
----
Mary
>>> print(X)
X
>>> print(X.v()) # 'v()' is a convenience function to get the first value of X
Mary

```

Prefixed predicates can appear in a logic clause and logic equality. Such clauses are implicitly called when accessing the corresponding class attribute. For example:


```python
>>> # all the indirect managers Y of X are derived from his manager, recursively
>>> (Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y) & (Y != None))
>>> (Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y) & (Y != None))
>>> # the salary class N of employee X is a function of his/her salary
>>> # this statement is a logic equality, not an assignment !
>>> Employee.salary_class[X] = Employee.salary[X]//1000
>>> # What is the salary class of John ?
>>> print(John.salary_class)
6

```

A logic clause can also redefine an existing python class attribute. For example, the Employee class can have a salary_class attribute, in addition to the logic clause above. In that case, Mary.salary_class refers to the attribute, while (Employee.salary_class[Mary] == Y) uses the logic clause. This example shows how to update a calculated attribute in one line:


```python
>>> # calculated attribute
>>> Mary.salary_class = ((Employee.salary_class[Mary]==X) >= X)

```

The pyDatalog definition of a class can be inherited. For example, if Employee inherits from Person, and if a Person has a clause defining the age of the person, both Person.age[X]and Employee.age[X] are legitimate. Employee.age[X] will first look for a clause in parent classes; if it does not find any, it will try to access the class attribute.

In-line queries can use the "&" operator to create conjunctive queries.

```python
>>> # Who are the employees of John with a salary class of 5 ?
>>> result = (Employee.salary_class[X] == 5) & Employee.indirect_manager(X, John)
>>> print(result)
X
---
Sam

```

It can be convenient to use aliases for class names , as shown below:

```python
>>> E = Employee # defines an alias for Employee
>>> Employee.salary_class[X] = E.salary[X]//1000

```

The pyDatalog Mixin also simplifies the creation of clauses for a class, using an anonymous function :

```python
>>> class Employee(pyDatalog.Mixin): # inherits pyDatalog capability
...     # <same definition of Employee as above>
...     pass
...
>>> @pyDatalog.program() # the following function contains pyDatalog clauses
... def _():
...     Employee.salary_class[X] = E.salary[X]//1000

```

The Employee class can be persisted using Python's [pickle ](http://www.google.com/url?q=http%3A%2F%2Fdocs.python.org%2F2%2Flibrary%2Fpickle.html&sa=D&sntz=1&usg=AOvVaw0ghADYpTbHXfdAPguywkgb)or [other persistence mechanisms](http://www.google.com/url?q=http%3A%2F%2Fstackoverflow.com%2Fquestions%2F1047318%2Feasiest-way-to-persist-a-data-structure-to-a-file-in-python&sa=D&sntz=1&usg=AOvVaw1h-QYFGy1QzQwBc1oksZeD), or to a relational database using SQLAlchemy , as shown in [this example](https://www.google.com/url?q=https%3A%2F%2Fbitbucket.org%2Fpcarbonn%2Fpydatalog%2Fsrc%2Fc840ef379a5e0dd8e60fcd312fc9b2aa983d208e%2FpyDatalog%2Fexamples%2FSQLAlchemy.py%3Fat%3Ddefault&sa=D&sntz=1&usg=AOvVaw3wjslvNmMWTL8anqEBJAoq).