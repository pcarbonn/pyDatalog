
import pyDatalog # or from pyDatalog import pyDatalog
import time

pyDatalog.clear()
@pyDatalog.program()
def _():
    # when is it ok to have a queen in row X1 and another in row X2, separated by N columns
    ok(X1, X2, N) <= (X1!=X2) & (X1!= X2+N) & (X1!=X2-N)
    
    queens0(X0) <= (X0 in range(8))
    queens1(X0,X1) <= (queens0(X0) & (X1 in range(8)) 
        & ok(X1,X0,1))
    queens2(X0,X1,X2) <= (queens1(X0,X1) & (X2 in range(8)) 
        & ok(X2,X1,1) & ok(X2,X0,2))
    queens3(X0,X1,X2,X3) <= (queens2(X0,X1,X2) & (X3 in range(8)) 
        & ok(X3,X2,1) & ok(X3,X1,2) & ok(X3,X0,3))
    queens4(X0,X1,X2,X3,X4) <= (queens3(X0,X1,X2,X3) & (X4 in range(8)) 
        & ok(X4,X3,1) & ok(X4,X2,2) & ok(X4,X1,3) & ok(X4,X0,4))
    queens5(X0,X1,X2,X3,X4,X5) <= (queens4(X0,X1,X2,X3,X4) & (X5 in range(8)) 
        & ok(X5,X4,1) & ok(X5,X3,2) & ok(X5,X2,3) & ok(X5,X1,4) & ok(X5,X0,5))
    queens6(X0,X1,X2,X3,X4,X5,X6) <= (queens5(X0,X1,X2,X3,X4,X5) & (X6 in range(8)) 
        & ok(X6,X5,1) & ok(X6,X4,2) & ok(X6,X3,3) & ok(X6,X2,4) & ok(X6,X1,5) & ok(X6,X0,6))
    queens7(X0,X1,X2,X3,X4,X5,X6,X7) <= (queens6(X0,X1,X2,X3,X4,X5,X6) & (X7 in range(8)) 
        & ok(X7,X6,1) & ok(X7,X5,2) & ok(X7,X4,3) & ok(X7,X3,4) & ok(X7,X2,5) & ok(X7,X1,6) & ok(X7,X0,7))

# counting is 0-based, so this is actually the 8-queens solution
print(pyDatalog.ask("queens7(X0,X1,X2,X3,X4,X5,X6,X7)"))
    
# there is a fixed penalty the first time around (JIT, ...), so let's measure performance the second time
start_time = time.time()
print(pyDatalog.ask("queens7(X0,X1,X2,X3,X4,X5,X6,X7)"))
datalog_time = (time.time() - start_time)

# pure python solution found on http://rosettacode.org/wiki/N-Queens#Python, for comparison purposes

from itertools import permutations

start_time = time.time()
 
n = 8
cols = range(n)
for vec in permutations(cols):
    if n == len(set(vec[i]+i for i in cols)) \
         == len(set(vec[i]-i for i in cols)):
        #print ( vec )
        pass
python_time = time.time() - start_time

print("datalog : %f seconds" % datalog_time)
print("python : %f seconds" % python_time)
