"""
This file contains the code for the interactive online datalog tutorial.
https://sites.google.com/site/pydatalog/Online-datalog-tutorial
"""

from pyDatalog import pyDatalog
pyDatalog.create_atoms('parent,bill,ancestor,descendents,manager, X,Y,Z,N,F,  factorial,odd,even, _split')

+ parent(bill,'John Adams')

print(parent(bill,X)) # prints [('John Adams',)]

# specify what an ancestor is
ancestor(X,Y) <= parent(X,Y)
ancestor(X,Y) <= parent(X,Z) & ancestor(Z,Y)

print(ancestor(bill, X)) # prints [('John Adams',)]

# the manager of bill is John Adams. Bill has only one manager.
+ (manager[bill]=='John Adams') 

print(manager[bill]==X) # prints [('John Adams',)]

# use expressions and recursion to evaluate factorials
(factorial[N] == F) <= (N < 2) & (F==1)
(factorial[N] == F) <= (N > 1) & (F == N*factorial[N-1])

print(factorial[3]==N) # prints [(6,)]

# aggregate function
# calculate the list of descendents, sorted by their name, and separated by ','
(descendents[X]==concat(Y, order_by=Y, sep=',')) <= ancestor(X,Y) 

print(descendents[bill]==X) # prints [('John Adams',)]

# specify how a string can be split, reversibly
_split(X, Y,Z) <= (X == Y+'-'+Z)
_split(X, Y,Z) <= (Y == (lambda X: X.split('-')[0])) & (Z == (lambda X: X.split('-')[1]))

# prints a set with one tuple : ('a-b', 'a', 'b')
print(_split("a-b",X,Y))

# unlimited depth of recursion
+ even(0)
even(N) <= (N > 0) & odd(N-1)
odd(N) <= (N > 0) & even(N-1)

# prints a set with one element: the ('2000',) tuple
print(even(20)) # prints [()], one result with 0 variable