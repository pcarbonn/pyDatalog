"""
This file contains the code for the interactive online datalog tutorial.
https://sites.google.com/site/pydatalog/Online-datalog-tutorial
"""

from pyDatalog import pyDatalog
pyDatalog.create_terms('parent,bill,ancestor,descendents,manager, X,Y,Z,N,N1,F,  factorial, first_remainder, odd,even, _split')

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
(factorial[N] == F) <= (N < 1) & (F==-factorial[-N])
+ (factorial[1]==1)
(factorial[N] == F) <= (N > 1) & (F == N*factorial[N-1])

print(factorial[3]==F) # prints [(6,)]

# (nested) list
first_remainder(X, Y, Z) <= (Y==X[0]) & (Z==X[1:])
print(first_remainder((1,2), Y, Z)) # Y is 1, Z is (2,)

print((Y==5) & (X==format_('Y is {}', Y))) # X is 'Y is 5'

# aggregate function
# calculate the list of descendents, sorted by their name, and separated by ','
(descendents[X]==concat_(Y, order_by=Y, sep=',')) <= ancestor(X,Y) 

print(descendents[bill]==X) # prints [('John Adams',)]

# specify how a string can be split, reversibly
_split(X, Y,Z) <= (X == Y+'-'+Z)
_split(X, Y,Z) <= (Y == (lambda X: X.split('-')[0])) & (Z == (lambda X: X.split('-')[1]))

print(_split("a-b",X,Y)) # prints [('a', 'b')]

# unlimited depth of recursion
+ even(0)
even(N) <= (N > 0) & odd(N-1)
odd(N) <= (N > 0) & even(N-1)

print(even(20)) # prints [()], i.e. one valid result with 0 variable


############################################ python in Datalog

class Employee(pyDatalog.Mixin): # --> Employee inherits the pyDatalog capability to use logic clauses
    
    def __init__(self, name, manager, salary): # method to initialize Employee instances
        super(Employee, self).__init__() # calls the initialization method of the Mixin class
        self.name = name
        self.manager = manager           # direct manager of the employee, or None
        self.salary = salary             # monthly salary of the employee
    
    def __repr__(self): # specifies how to display an Employee
        return self.name

John = Employee('John', None, 6800)
Mary = Employee('Mary', John, 6300)
Sam = Employee('Sam', Mary, 5900)

pyDatalog.create_terms('has_car, X')
+ has_car(Mary)
print(has_car(X)) # prints [(Mary,)]

print(Employee.salary[X]==6300) # prints [(Mary,)]
print(X) # prints [Mary]
print(X.v()) # 'v()' is a convenience function to get the first value of X (or None)

print((Employee.salary[X]==6300) >= X) # prints the first value of X in the query, in one line

# the salary class N of employee X is computed as a function of his/her salary
# this statement is a logic equality, not an assignment !
Employee.salary_class[X] = Employee.salary[X]//1000


# What is the salary class of John ?
print(John.salary_class) # prints 6

# calculated attribute
Mary.salary_class = ((Employee.salary_class[Mary]==X) >= X) 

# all the indirect managers Y of employee X are derived from his manager, recursively
Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y) & (Y != None)
Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y) & (Y != None)

# Who are the employees of John with a salary class of 5 ?
result = (Employee.salary_class[X] == 5) & Employee.indirect_manager(X, John)
print(result) # prints [(Sam,)]

E = Employee # defines an alias for Employee
Employee.salary_class[X] = E.salary[X]//1000