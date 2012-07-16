"""
This file shows how to use pyDatalog using facts stored in python objects.

It has 3 parts :
    1. define python class and business rules 
    2. create python objects for 2 employees
    3. Query the objects using the datalog engine
"""

import pyDatalog # or: from pyDatalog import pyDatalog

""" 1. define python class and business rules """

import pyDatalog # or: from pyDatalog import pyDatalog

class Employee(pyDatalog.Mixin):   # --> Employee inherits the pyDatalog capability to use logic clauses
    
    def __init__(self, name, manager, salary): # method to initialize Employee instances
        super(Employee, self).__init__() # calls the initialization method of the Mixin class
        self.name = name
        self.manager = manager           # direct manager of the employee, or None
        self.salary = salary             # monthly salary of the employee
    
    def __repr__(self): # specifies how to display an Employee
        return self.name

    @pyDatalog.program() # indicates that the following method contains pyDatalog clauses
    def _():
        # the salary class N of employee X is computed as a function of his/her salary
        # this statement is a logic equality, not an assignment !
        Employee.salary_class[X] = Employee.salary[X]//1000
        
        # all the indirect managers Y of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y)
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y)

""" 2. create python objects for 2 employees """
John = Employee('John', None, 6800)
Mary = Employee('Mary', John, 6300)

""" 3. Query the objects using the datalog engine """
# the following python statements implicitly use the datalog clauses in the class definition

# What is the salary class of Mary ?
print(Mary.salary_class)

# who has a salary of 6300 ?
X = pyDatalog.Variable()
Employee.salary[X] == 6300 # notice the similarity to a pyDatalog query
print(X) # prints (Mary,)

# Who are the employees with a salary class of 6 ?
Employee.salary_class[X] == 6
print(X) # prints (John, Mary)

# who are the indirect managers of Mary ?
Employee.indirect_manager(Mary, X)
print(X) # prints (John,)

assert Employee.manager[Mary]==John
# who is his own  managers ?
Employee.manager[X]==X
print(X) # prints (,)

# who is his own indirect managers ?
Employee.indirect_manager(X, X)
print(X) # prints (,)

# start the datalog console, for interactive querying of employee
import console # or: from pyDatalog import console
console = console.datalogConsole(locals=locals())
console.interact('')
