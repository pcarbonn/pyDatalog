"""
This file shows how to use pyDatalog using facts stored in datalog.

It has 3 parts:
    1. create facts for 2 employees in the datalog engine
    2. define business rules
    3. Query the datalog engine
"""
from pyDatalog import pyDatalog

""" 1. create facts for 3 employees in the datalog engine """
pyDatalog.create_atoms('salary', 'manager')

# John is the manager of Mary, who is the manager of Sam
+ (salary['John'] == 6800)

+ (manager['Mary'] == 'John')
+ (salary['Mary'] == 6300)

+ (manager['Sam'] == 'Mary')
+ (salary['Sam'] == 5900)

""" 2. define business rules """
pyDatalog.create_atoms('salary_class', 'indirect_manager', 'report_count', 'budget', 'lowest',
                       'X', 'Y', 'Z', 'N')
# the salary class of employee X is computed as a function of his/her salary
salary_class[X] = salary[X]//1000
    
# all the indirect managers of employee X are derived from his manager, recursively
indirect_manager(X,Y) <= (manager[X] == Y) & (Y != None)
indirect_manager(X,Y) <= (manager[X] == Z) & indirect_manager(Z,Y) & (Y != None)

# count the number of reports of X
(report_count[X] == len_(Y)) <= indirect_manager(Y,X)

""" 3. Query the datalog engine """

# what is the salary class of John ?
print(salary_class['John'] == Y) # Y is 6

# who has a salary of 6300 ?
print(salary[X] == 6300) # X is Mary

# who are the indirect managers of Mary ?
print(indirect_manager('Mary', X)) # X is John

# Who are the employees of John with a salary below 6000 ?
print((salary[X] < 6000) & indirect_manager(X, 'John')) # X is Sam

# who is his own indirect manager ?
print(indirect_manager('X', X)) # prints []

# who has 2 reports ?
print(report_count[X] == 2) # X is John

# what is the total salary of the employees of John ? 
(budget[X] == sum_(N, for_each=Y)) <= (indirect_manager(Y, X)) & (salary[Y]==N)
print(budget['John']==N) # N is 12200

# who has the lowest salary ?
(lowest[1] == min_(X, order_by=N)) <= (salary[X]==N)
print(lowest[1]==X) # X is Sam

# start the datalog console, for interactive querying 
from pyDatalog.examples import console
console = console.datalogConsole(locals=locals())
console.interact('Type exit() when done.')
