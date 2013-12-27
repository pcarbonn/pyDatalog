"""
This file shows how to use pyDatalog to query 
a relational database and a noSQL database simultaneously.

It has 3 parts :
    1.initialize the Mongo database
    2. define a predicate resolver for Employee.diploma 
    3. query data from both Mongo DB and SQLite
"""

""" 1.initialize the Mongo database  """

from pymongo import Connection
connection = Connection()
db = connection.Employees
profiles = db.profiles

profiles_to_insert = [{"name": "John", "diploma": "MSc."},
                     {"name": "Mary", "diploma": "EE"},
                     {"name": "Sam", "diploma": "MBA"}]

profiles.insert(profiles_to_insert)

""" 2. define a predicate resolver for Employee.diploma """

from SQLAlchemy import Employee # import the SQLAlchemy example

def _pyD_diploma2(cls, employee, diploma):
    global profiles
    if employee.is_const():
        r = profiles.find_one({"name": employee.id.name})
        if r: yield (employee, r["diploma"])
        return
    raise AttributeError

Employee._pyD_diploma2 = classmethod(_pyD_diploma2) # attach the resolver to the Employee class

""" 3. query data from both Mongo DB and SQLite """

from pyDatalog import pyDatalog

print("\n *** combined SQLite and Mongo query :\n")
pyDatalog.create_terms('X, N, Diploma')

# Who has a salary of 6800 and a MSc. diploma
(Employee.salary[X]==6800) & (Employee.diploma[X]=="MSc.")
print(X)

