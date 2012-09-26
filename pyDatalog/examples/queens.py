
from pyDatalog import pyDatalog
import time

pyDatalog.clear()
@pyDatalog.program()
def _():
    queens0(X0) <= (X0 in range(8))

    # when is it ok to have a queen in row X1 and another in row X2, separated by N columns
    # this is memoized !
    ok(X1, N, X2) <= (X1!=X2) & (X1!= X2+N) & (X1!=X2-N)
    
    queens1(X0,X1) <= queens0(X0) & queens0(X1) & ok(X1,1,X0)

    queens2(X0,X1,X2) <= queens1(X0,X1) & queens1(X1,X2) & ok(X0,2,X2)

    queens3(X0,X1,X2,X3) <= queens2(X0,X1,X2) & queens2(X1,X2,X3) & ok(X0,3,X3)

    queens4(X0,X1,X2,X3,X4) <= queens3(X0,X1,X2,X3) & queens3(X1,X2,X3,X4) & ok(X0,4,X4)

    queens5(X0,X1,X2,X3,X4,X5) <= queens4(X0,X1,X2,X3,X4) & queens4(X1,X2,X3,X4,X5) & ok(X0,5,X5)

    queens6(X0,X1,X2,X3,X4,X5,X6) <= queens5(X0,X1,X2,X3,X4,X5) & queens5(X1,X2,X3,X4,X5,X6) & ok(X0,6,X6)
    # counting is 0-based, so this is actually the 8-queens solution
    queens7(X0,X1,X2,X3,X4,X5,X6,X7) <= queens6(X0,X1,X2,X3,X4,X5,X6) & queens6(X1,X2,X3,X4,X5,X6,X7) & ok(X0,7,X7)

# counting is 0-based, so this is actually the 8-queens solution
print(pyDatalog.ask("queens7(X0,X1,X2,X3,X4,X5,X6,X7)"))
    
# there is a fixed penalty the first time around (JIT, ...), so let's measure performance the second time
start_time = time.time()
datalog_count = len(pyDatalog.ask("queens7(X0,X1,X2,X3,X4,X5,X6,X7)").answers)
datalog_time = (time.time() - start_time)

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
# 0.33 sec for Datalog
# 0.11 sec for python