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
        Employee.salary_class(X,N) <= Employee.salary(X,N1) & (N==N1//1000)
        
        # all the indirect managers Y of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= Employee.manager(X,Y)
        Employee.indirect_manager(X,Y) <= Employee.manager(X,Z) & Employee.indirect_manager(Z,Y)

""" 2. create python objects for 2 employees """
John = Employee('John', None, 6800)
Mary = Employee('Mary', John, 6300)

""" 3. Query the objects using the datalog engine """
# the following python statements implicitly use the datalog clauses in the class definition

# who has a salary of 6300 ?
X = pyDatalog.Variable()
Employee.salary(X, 6300) # notice the similarity to a pyDatalog query
print(X) # prints (Mary,)

# what is the salary class of Mary ?
Employee.salary_class(Mary, X)
print(X) # prints (6,)

# Who are the employees with a salary class of 6 ?
Employee.salary_class(X, 6)
print(X) # prints (John, Mary)

# who are the indirect managers of Mary ?
Employee.indirect_manager(Mary, X)
print(X) # prints (John,)

