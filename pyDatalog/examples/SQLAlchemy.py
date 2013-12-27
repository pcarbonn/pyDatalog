"""
This file shows how to use pyDatalog using facts stored in a relational database, 
via SQLAlchemy's Object-Relational Mapping (ORM).

It has 4 parts :
    1. initialize the ORM
    2. define python class and business rules 
    3. create table and rows for 2 employees
    4. Query the objects using the datalog engine
"""

from pyDatalog import pyDatalog

""" 1.initialize the ORM  """
from sqlalchemy import create_engine
from sqlalchemy import Column, Integer, String, ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker

# define a base class with SQLAlchemy and datalog capabilities, to be inherited by the Employee class
Base = declarative_base(cls=pyDatalog.Mixin, metaclass=pyDatalog.sqlMetaMixin)

# open a session on a database, then associate it to the Base class
engine = create_engine('sqlite:///:memory:', echo=False) # create database in memory
Session = sessionmaker(bind=engine)
session = Session()
Base.session = session

""" 2. define python class and business rules """
class Employee(Base): # --> Employee inherits from the Base class
    __tablename__ = 'employee'
    
    name = Column(String, primary_key=True)
    manager_name = Column(String, ForeignKey('employee.name'))
    salary = Column(Integer)
    
    def __init__(self, name, manager_name, salary):
        super(Employee, self).__init__()
        self.name = name
        self.manager_name = manager_name # direct manager of the employee, or None
        self.salary = salary # monthly salary of the employee
    def __repr__(self): # specifies how to display the employee
        return "Employee: %s" % self.name

    @pyDatalog.program() # --> the following function contains pyDatalog clauses
    def Employee():
        (Employee.manager[X]==Y) <= (Employee.manager_name[X]==Z) & (Z==Employee.name[Y])
        # the salary class of employee X is computed as a function of his/her salary
        # this statement is a logic equality, not an assignment !
        Employee.salary_class[X] = Employee.salary[X]//1000
        
        # all the indirect managers of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y) & (Y != None)
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y) & (Y != None)
        
        # count the number of reports of X
        (Employee.report_count[X] == len(Y)) <= Employee.indirect_manager(Y,X)

""" 3. create table and rows for 2 employees """
# create table
Base.metadata.create_all(engine) 

# create rows for 3 employees
# John is the manager of Mary, who is the manager of Sam
John = Employee('John', None, 6800)
Mary = Employee('Mary', 'John', 6300)
Sam = Employee('Sam', 'Mary', 5900)

session.add(John)
session.add(Mary)
session.add(Sam)
session.commit()

""" 4. Query the objects using the datalog engine """
pyDatalog.create_terms('X, Y, N, lowest')

# the following python statements implicitly use the datalog clauses in the class definition

# What is the salary class of John ?
print(John.salary_class) # prints 6

# who has a salary of 6300 ?
Employee.salary[X] == 6300 # notice the similarity to a pyDatalog query
print(X) # prints [Employee: Mary]
print(X.v()) # prints Employee:Mary

# who are the indirect managers of Mary ?
Employee.indirect_manager(Mary, X)
print(X) # prints [Employee: John]

# Who are the employees of John with a salary below 6000 ?
result = (Employee.salary[X] < 6000) & Employee.indirect_manager(X, John)
print(result) # Sam is in the result
print(X) # prints [Employee: Sam]

# verify that the manager of Mary is John
assert Employee.manager[Mary]==John

# who is his own indirect manager ?
Employee.indirect_manager(X, X)
print(X) # prints []

# who has 2 reports ?
Employee.report_count[X] == 2
print(X) # prints [Employee: John]

# what is the total salary of the employees of John ?
# note : it is better to place aggregation clauses in the class definition 
Mary.salary = 6400 # queries use the latest, in-session, data
(Employee.budget[X] == sum_(N, for_each=Y)) <= (Employee.indirect_manager(Y, X)) & (Employee.salary[Y]==N)
Employee.budget[John]==X
print(X) # prints [12300]

# who has the lowest salary ?
(lowest[1] == min_(X, order_by=N)) <= (Employee.salary[X]==N)
# must use ask() because inline queries cannot use unprefixed literals 
print(lowest[1]==X) # Sam is the result
