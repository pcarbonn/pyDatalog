"""
This file shows how to use pyDatalog using facts stored in python objects.

It has 3 parts :
    1. define python class and business rules 
    2. create python objects for 2 employees
    3. Query the objects using the datalog engine
"""

from pyDatalog import pyDatalog

""" 1. define python class and business rules """

class Employee(pyDatalog.Mixin):   # --> Employee inherits the pyDatalog capability to use logic clauses
    
    def __init__(self, name, manager, salary): # method to initialize Employee instances
        super(Employee, self).__init__() # calls the initialization method of the Mixin class
        self.name = name
        self.manager = manager           # direct manager of the employee, or None
        self.salary = salary             # monthly salary of the employee
    
    def __repr__(self): # specifies how to display an Employee
        return self.name

    @pyDatalog.program() # indicates that the following method contains pyDatalog clauses
    def Employee():
        # the salary class N of employee X is computed as a function of his/her salary
        # this statement is a logic equality, not an assignment !
        Employee.salary_class[X] = Employee.salary[X]//1000
        
        # all the indirect managers Y of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y) & (Y != None)
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y) & (Y != None)
        
        # count the number of reports of X
        (Employee.report_count[X] == len(Y)) <= Employee.indirect_manager(Y,X)

""" 2. create python objects for 3 employees """
# John is the manager of Mary, who is the manager of Sam
John = Employee('John', None, 6800)
Mary = Employee('Mary', John, 6300)
Sam = Employee('Sam', Mary, 5900)

""" 3. Query the objects using the datalog engine """
# the following python statements implicitly use the datalog clauses in the class definition

# What is the salary class of John ?
print(John.salary_class) # prints 6

# who has a salary of 6300 ?
X = pyDatalog.Variable()
Employee.salary[X] == 6300 # notice the similarity to a pyDatalog query
print(X) # prints [Mary]
print(X.v()) # prints Mary

# who are the indirect managers of Mary ?
Employee.indirect_manager(Mary, X)
print(X) # prints [John]

# Who are the employees of John with a salary below 6000 ?
result = (Employee.salary[X] < 6000) & Employee.indirect_manager(X, John)
print(result) # prints [(Sam,)]
print(X) # prints [Sam]
print((Employee.salary_class[X] == 5) & Employee.indirect_manager(X, John) >= X) # Sam

# verify that the manager of Mary is John
assert Employee.manager[Mary]==John

# who is his own indirect manager ?
Employee.indirect_manager(X, X)
print(X) # prints []

# who has 2 reports ?
Employee.report_count[X] == 2
print(X) # prints [John]

# what is the total salary of the employees of John ?
# note : it is better to place aggregation clauses in the class definition 
pyDatalog.load("(Employee.budget[X] == sum(N, for_each=Y)) <= (Employee.indirect_manager(Y, X)) & (Employee.salary[Y]==N)")
Employee.budget[John]==X
print(X) # prints [12200]

# who has the lowest salary ?
pyDatalog.load("(lowest[1] == min(X, order_by=N)) <= (Employee.salary[X]==N)")
# must use ask() because inline queries cannot use unprefixed literals 
print(pyDatalog.ask("lowest[1]==X")) # prints set([(1, 'Sam')])
