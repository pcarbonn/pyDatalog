
from pyDatalog import pyDatalog
import time

pyDatalog.create_atoms('N,X0,X1,X2,X3,X4,X5,X6,X7')
pyDatalog.create_atoms('ok,queens0,queens1,queens2,queens3,queens4,queens5,queens6,queens7')

# when is it ok to have a queen in row X1 and another in row X2, separated by N columns
# this is memoized !
#ok(X1, N, X2) <= (X1!=X2) & (X1!= X2+N) & (X1!=X2-N)
@pyDatalog.predicate()
def ok3(X1, N, X2):
    if (X1.id!=X2.id) and (X1.id!= X2.id+N.id) and (X1.id!=X2.id-N.id):
        yield (X1.id, N.id, X2.id)

queens0(X0) <= (X0._in(range(8)))
queens1(X0,X1) <= queens0(X0) & queens0(X1) & ok(X1,1,X0)
queens2(X0,X1,X2) <= queens1(X0,X1) & queens1(X1,X2) & ok(X0,2,X2)
queens3(X0,X1,X2,X3) <= queens2(X0,X1,X2) & queens2(X1,X2,X3) & ok(X0,3,X3)
queens4(X0,X1,X2,X3,X4) <= queens3(X0,X1,X2,X3) & queens3(X1,X2,X3,X4) & ok(X0,4,X4)
queens5(X0,X1,X2,X3,X4,X5) <= queens4(X0,X1,X2,X3,X4) & queens4(X1,X2,X3,X4,X5) & ok(X0,5,X5)
queens6(X0,X1,X2,X3,X4,X5,X6) <= queens5(X0,X1,X2,X3,X4,X5) & queens5(X1,X2,X3,X4,X5,X6) & ok(X0,6,X6)
queens7(X0,X1,X2,X3,X4,X5,X6,X7) <= queens6(X0,X1,X2,X3,X4,X5,X6) & queens6(X1,X2,X3,X4,X5,X6,X7) & ok(X0,7,X7)

# counting is 0-based, so this is actually the 8-queens solution
start_time = time.time()
print(queens7(X0,X1,X2,X3,X4,X5,X6,X7))
print("First datalog run in %f seconds" % (time.time() - start_time))

for i in range(20):  
    # there is a warm-up period for the JIT --> let's compute it again
    start_time = time.time()
    datalog_count = len(queens7(X0,X1,X2,X3,X4,X5,X6,X7))
    datalog_time = (time.time() - start_time)
    print(datalog_time)

# pure python solution found on http://rosettacode.org/wiki/N-Queens#Python, for comparison purposes

from itertools import permutations

n = 8
cols = range(n)
def queens():
    for vec in permutations(cols):
        if n == len(set(vec[i]+i for i in cols)) \
             == len(set(vec[i]-i for i in cols)):
            #print ( vec )
            pass

start_time = time.time()
queens() 
python_time = time.time() - start_time

print("%i solutions by datalog in %f seconds" % (datalog_count, datalog_time))
print("python : %f seconds" % python_time)

# results with pypy 1.9 on Intel Core i7-2820 QM CPU @ 2.3 GHz (run from Command prompt):
# 0.17..0.24 sec for Datalog
# 0.11 sec for python
