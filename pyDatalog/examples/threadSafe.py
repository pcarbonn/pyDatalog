'''
Created on 3 juin 2013

@author: pcarbonn
'''

from pyDatalog import pyDatalog, Logic
import random
import threading

pyDatalog.create_terms('N,X0,X1,X2,X3,X4,X5,X6,X7')
pyDatalog.create_terms('ok,queens, next_queen')

@pyDatalog.predicate()
def ok3(X1, N, X2):
    if (X1.id!=X2.id) and (X1.id!= X2.id+N.id) and (X1.id!=X2.id-N.id):
        yield (X1.id, N.id, X2.id)

def queen(thread_name):
    n = int(random.random() * 8) + 1 # 1 to 8
    Logic()
    
    queens(X0)                      <= (X0._in(range(n)))
    queens(X0,X1)                   <= queens(X0)                   & next_queen(X0,X1)
    queens(X0,X1,X2)                <= queens(X0,X1)                & next_queen(X0,X1,X2)
    queens(X0,X1,X2,X3)             <= queens(X0,X1,X2)             & next_queen(X0,X1,X2,X3)
    queens(X0,X1,X2,X3,X4)          <= queens(X0,X1,X2,X3)          & next_queen(X0,X1,X2,X3,X4)
    queens(X0,X1,X2,X3,X4,X5)       <= queens(X0,X1,X2,X3,X4)       & next_queen(X0,X1,X2,X3,X4,X5)
    queens(X0,X1,X2,X3,X4,X5,X6)    <= queens(X0,X1,X2,X3,X4,X5)    & next_queen(X0,X1,X2,X3,X4,X5,X6)
    queens(X0,X1,X2,X3,X4,X5,X6,X7) <= queens(X0,X1,X2,X3,X4,X5,X6) & next_queen(X0,X1,X2,X3,X4,X5,X6,X7)
    
    next_queen(X0,X1)                   <= queens(X1)                       & ok(X0,1,X1)
    next_queen(X0,X1,X2)                <= next_queen(X1,X2)                & ok(X0,2,X2)
    next_queen(X0,X1,X2,X3)             <= next_queen(X1,X2,X3)             & ok(X0,3,X3)
    next_queen(X0,X1,X2,X3,X4)          <= next_queen(X1,X2,X3,X4)          & ok(X0,4,X4)
    next_queen(X0,X1,X2,X3,X4,X5)       <= next_queen(X1,X2,X3,X4,X5)       & ok(X0,5,X5)
    next_queen(X0,X1,X2,X3,X4,X5,X6)    <= next_queen(X1,X2,X3,X4,X5,X6)    & ok(X0,6,X6)
    next_queen(X0,X1,X2,X3,X4,X5,X6,X7) <= next_queen(X1,X2,X3,X4,X5,X6,X7) & ok(X0,7,X7)
    
    query = pyDatalog.ask("queens(%s)" % (",".join("X%s" % i for i in range(n))))
    answers = query.answers if query else [] 
    result = "OK" if len(answers) == [1,0,0,2,10,4,40,92][n-1] else "* not OK ! *"
    print("%s : n = %d %s " % (thread_name, n, result))
    
for i in range(20):
    # queen("test %s" % i)
    t = threading.Thread(target=queen,args=("thread %02d" % i,))
    t.start()

