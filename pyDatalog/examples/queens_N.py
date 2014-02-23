
from pyDatalog import pyDatalog
import time

pyDatalog.create_terms('N,N1, X,Y, X0,X1,X2,X3,X4,X5,X6,X7')
pyDatalog.create_terms('ok,queens, next_queen, pred, pred2')

size=8

# when is it ok to have a queen in row X1 and another in row X2, separated by N columns
# this is memoized !
queens(N, X)    <= (N>1) & queens(N-1, X[:-1]) & next_queen(N, X)
queens(1, X)    <= (X1._in(range(size))) & (X[0]==X1)

next_queen(N, X) <= (N>2) & next_queen(N-1, X[1:]) & ok(X[0], N-1, X[-1]) 
next_queen(2, X) <= queens(1,(X1,)) & ok(X[0], 1, X1) & (X[1] == X1)

ok(X1, N, X2) <= (X1 != X2) & (X1 != X2+N) & (X1 != X2-N)

start_time = time.time()
print(queens(size, (X0,X1,X2,X3,X4,X5,X6,X7)))
print("First datalog run in %f seconds" % (time.time() - start_time))

start = time.time()
for i in range(20):
    # there is a warm-up period for the JIT --> let's compute it again
    start_time = time.time()
    datalog_count = len(queens(size, (X0,X1,X2,X3,X4,X5,X6,X7)).data)
    datalog_time = (time.time() - start_time)
    print(datalog_time)
print("Average : %s" % ((time.time() - start)/20))

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

