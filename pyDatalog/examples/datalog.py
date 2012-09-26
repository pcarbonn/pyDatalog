"""
This file shows how to use pyDatalog using facts stored in datalog.

It has 3 parts:
    1. create facts for 2 employees in the datalog engine
    2. define business rules
    3. Query the datalog engine
"""

from pyDatalog import pyDatalog

""" 1. create facts for 3 employees in the datalog engine """
# John is the manager of Mary, who is the manager of Sam
pyDatalog.load("+ (salary['John'] == 6800)")

pyDatalog.load("+ (manager['Mary'] == 'John')")
pyDatalog.load("+ (salary['Mary'] == 6300)")

pyDatalog.load("+ (manager['Sam'] == 'Mary')")
pyDatalog.load("+ (salary['Sam'] == 5900)")

""" 2. define business rules """
# the salary class of employee X is computed as a function of his/her salary
pyDatalog.load("salary_class[X] = salary[X]//1000")
    
# all the indirect managers of employee X are derived from his manager, recursively
pyDatalog.load("indirect_manager(X,Y) <= (manager[X] == Y) & (Y != None)")
pyDatalog.load("indirect_manager(X,Y) <= (manager[X] == Z) & indirect_manager(Z,Y) & (Y != None)")

# count the number of reports of X
pyDatalog.load("(report_count[X] == len(Y)) <= indirect_manager(Y,X)")

""" 3. Query the datalog engine """

# what is the salary class of John ?
print(pyDatalog.ask("salary_class['John'] == Y")) # prints set([('John', 6)])

# who has a salary of 6300 ?
print(pyDatalog.ask("salary[X] == 6300")) # prints set([('Mary', 6300)])

# who are the indirect managers of Mary ?
print(pyDatalog.ask("indirect_manager('Mary', X)")) # prints set([('Mary', 'John')])

# Who are the employees of John with a salary below 6000 ?
print(pyDatalog.ask("(salary[X] < 6000) & indirect_manager(X, 'John')")) # prints set([('Sam', 5900)])

# who is his own indirect manager ?
print(pyDatalog.ask("indirect_manager('X', X)")) # prints None

# who has 2 reports ?
print(pyDatalog.ask("report_count[X] == 2")) # prints set([('John', 2)])

# what is the total salary of the employees of John ? 
pyDatalog.load("(Budget[X] == sum(N, for_each=Y)) <= (indirect_manager(Y, X)) & (salary[Y]==N)")
print(pyDatalog.ask("Budget['John']==N")) # prints set([('John', 12200)])

# who has the lowest salary ?
pyDatalog.load("(Lowest[1] == min(X, order_by=N)) <= (salary[X]==N)")
print(pyDatalog.ask("Lowest[1]==N")) # prints set([(1, 'Sam')])

# start the datalog console, for interactive querying 
from pyDatalog import console
console = console.datalogConsole(locals=locals())
console.interact('Type exit() when done.')
