
from pyDatalog import pyDatalog
import time
from pyDatalog import pyEngine

pyDatalog.create_atoms('N,N1, X,Y, X0,X1,X2,X3,X4,X5,X6,X7')
pyDatalog.create_atoms('ok,queens, pred')

size=8

# when is it ok to have a queen in row X1 and another in row X2, separated by N columns
# this is memoized !
#ok(X1, N, X2) <= (X1!=X2) & (X1!= X2+N) & (X1!=X2-N)
@pyDatalog.predicate()
def ok3(X1, N, X2):
    if (X1.id!=X2.id) and (X1.id!= X2.id+N.id) and (X1.id!=X2.id-N.id):
        yield (X1.id, N.id, X2.id)

pred(N, N1) <= (N>1) & (N1==N-1)
queens(1, X) <= (X1._in(range(size))) & (X1==X[0])
queens(N, X) <= pred(N, N1) & queens(N1, X[:-1]) & queens(N1, X[1:]) & ok(X[0], N1, X[-1])

start_time = time.time()
print(queens(size, (X0,X1,X2,X3,X4,X5,X6,X7)))
print("First datalog run in %f seconds" % (time.time() - start_time))

for i in range(20):
    # there is a warm-up period for the JIT --> let's compute it again
    start_time = time.time()
    datalog_count = len(queens(size, (X0,X1,X2,X3,X4,X5,X6,X7)).data)
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

