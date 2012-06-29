"""
This file shows how to use pyDatalog using facts stored in python objects.

It has 3 parts :
    1. define python class and business rules 
    2. create python objects for 2 employees
    3. Query the objects using the datalog engine
"""

import pyDatalog

""" 1. define python class and business rules """
Base = pyDatalog.Register # --> Base is a class with the pyDatalog capability to use logic clauses

class Employee(Base): # --> Employee inherits from the Base class
    def __init__(self, name, manager, salary): # function to initialize instances
        super(Employee, self).__init__() # calls __init__ in the Base class
        self.name = name
        self.manager = manager # direct manager of the employee, or None
        self.salary = salary # monthly salary of the employee
    def __repr__(self): # specifies how to display the employee
        return self.name

    @pyDatalog.program() # indicates that the following function contains pyDatalog clauses
    def _():
        # the salary class of employee X is computed as a function of his/her salary
        Employee.salary_class(X,N) <= Employee.salary(X,N1) & (N==N1//1000)
        
        # all the indirect managers of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= Employee.manager(X,Y)
        Employee.indirect_manager(X,Y) <= Employee.manager(X,Z) & Employee.indirect_manager(Z,Y)

""" 2. create python objects for 2 employees """
John = Employee('John', None, 6800)
Mary = Employee('Mary', John, 6300)

""" 3. Query the objects using the datalog engine """
# the following python statements implicitly use the datalog clauses

# who has a salary of 6300 ?
X=[]
Employee.salary(X, 6300) # notice the similarity to a pyDatalog query
print(X) # prints Mary

# what is the salary class of Mary ?
Y = []
Employee.salary_class(Mary, Y)
print(Y) # prints 6

# who are the indirect managers of Mary ?
X, Y =[], []
Employee.indirect_manager(Mary, X)
print(X) # prints (John,)

# Who are the employees with a salary class of 6 ?
X=[]
Employee.salary_class(X, 6)
print(X) # prints (John, Mary)

