"""
This file shows how to use pyDatalog using facts stored in a relational database, 
via SQLAlchemy's Object-Relational Mapping (ORM).

It has 4 parts :
    1. initialize the ORM
    2. define python class and business rules 
    3. create table and rows for 2 employees
    4. Query the objects using the datalog engine
"""

import pyDatalog # or: from pyDatalog import pyDatalog

""" 1.initialize the ORM  """
from sqlalchemy import create_engine
from sqlalchemy import Column, Integer, String, ForeignKey
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker, relationship

engine = create_engine('sqlite:///:memory:', echo=True) # create database in memory
Session = sessionmaker(bind=engine)
session = Session()

# define a base class with SQLAlchemy and datalog capabilities, to be inherited by the Employee class
Base = declarative_base(cls=pyDatalog.Mixin, metaclass=pyDatalog.sqlMetaMixin)
Base.session = session

""" 2. define python class and business rules """
class Employee(Base): # --> Employee inherits from the Base class
    __tablename__ = 'employee'
    
    name = Column(String, primary_key=True)
    manager_name = Column(String, ForeignKey('employee.name'))
    salary = Column(Integer)
    
    manager = relationship("Employee", backref="managees", remote_side='Employee.name',)
    
    def __init__(self, name, manager_name, salary):
        super(Employee, self).__init__()
        self.name = name
        self.manager_name = manager_name # direct manager of the employee, or None
        self.salary = salary # monthly salary of the employee
    def __repr__(self): # specifies how to display the employee
        return "Employee: %s" % self.name

    @pyDatalog.program() # --> the following function contains pyDatalog clauses
    def _():
        # the salary class of employee X is computed as a function of his/her salary
        (Employee.salary_class[X]==N) <= (Employee.salary[X]==N1) & (N==N1//1000)
        
        # all the indirect managers of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y)
        Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y)

""" 3. create table and rows for 2 employees """
# create table
Base.metadata.create_all(engine) 

# create rows for 2 employees
John = Employee('John', None, 6800)
Mary = Employee('Mary', 'John', 6300)

session.add(John)
session.add(Mary)
session.commit()

""" 4. Query the objects using the datalog engine """
# the following python statements implicitly use the datalog clauses

# who has a salary of 6300 ?
X = pyDatalog.Variable()
Employee.salary[X] == 6300 # notice the similarity to a pyDatalog query
print(X) # prints (Mary,)

# what is the salary class of Mary ?
Employee.salary_class[Mary] == X
print(X) # prints (6,)

# Who are the employees with a salary class of 6 ?
Employee.salary_class[X] == 6
print(X) # prints (John, Mary)

# who are the indirect managers of Mary ?
Employee.indirect_manager(Mary, X)
print(X) # prints (John,)
