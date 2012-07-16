"""
This file shows how to use pyDatalog using facts stored in datalog.

It has 3 parts:
    1. create facts for 2 employees in the datalog engine
    2. define business rules
    3. Query the datalog engine
"""

import pyDatalog # or: from pyDatalog import pyDatalog

""" 1. create facts for 2 employees in the datalog engine """
pyDatalog.load("+ (salary['John'] == 6800)")

pyDatalog.load("+ (manager['Mary'] == 'John')")
pyDatalog.load("+ (salary['Mary'] == 6300)")

""" 2. define business rules """
# the salary class of employee X is computed as a function of his/her salary
pyDatalog.load("salary_class[X] = salary[X]//1000")
    
# all the indirect managers of employee X are derived from his manager, recursively
pyDatalog.load("indirect_manager(X,Y) <= (manager[X] == Y)")
pyDatalog.load("indirect_manager(X,Y) <= (manager[X] == Z) & indirect_manager(Z,Y)")

""" 3. Query the datalog engine """

# what is the salary class of John ?
print(pyDatalog.ask("salary_class['John'] == Y")) # prints set([('John', 6)])

# who are the indirect managers of Mary ?
print(pyDatalog.ask("indirect_manager('Mary', X)")) # prints set([('Mary', 'John')])

# Who are the employees with a salary class of 6 ?
print(pyDatalog.ask("salary_class[X] == 6")) # prints set([('Mary', 6), ('John', 6)])

# who is his own direct managers of Mary ?
print(pyDatalog.ask("manager('X', X)")) # prints None

# start the datalog console, for interactive querying 
import console # or: from pyDatalog import console
console = console.datalogConsole(locals=locals())
console.interact('')
