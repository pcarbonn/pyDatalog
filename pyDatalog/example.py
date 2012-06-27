
import pyDatalog

class Employee(pyDatalog.Register): # --> Employee inherits pyDatalog capability to use logic clauses
    def __init__(self, name, manager, salary):
        super(Employee, self).__init__()
        self.name = name
        self.manager = manager # direct manager of the employee, or None
        self.salary = salary # monthly salary of the employee
    def __repr__(self): # specifies how to display the employee
        return self.name

    @pyDatalog.program() # --> the following function contains pyDatalog clauses
    def _():
        # the salary class of employee X is computed as a function of his/her salary
        Employee.salary_class(X,N) <= Employee.salary(X,N1) & (N==N1/1000)
        
        # all the indirect managers of employee X are derived from his manager, recursively
        Employee.indirect_manager(X,Y) <= Employee.manager(X,Y)
        Employee.indirect_manager(X,Y) <= Employee.manager(X,Z) & Employee.indirect_manager(Z,Y)

# create 2 employees
John = Employee('John', None, 6800)
Mary = Employee('Mary', John, 6300)

# the following python statements implicitly use the datalog clauses

# what is the salary class of John ?
Y = []
Employee.salary_class(John, Y)
print(Y)

# who are the indirect managers of Mary ?
X, Y =[], []
Employee.indirect_manager(Mary, X) # notice the similarity to a pyDatalog clause
print(X) # prints (John,)

# Who are the employees with a salary class of 6 ?
X=[]
Employee.salary_class(X, 6)
print(X) # prints (John, Mary)

