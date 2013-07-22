'''
Created on 3 juin 2013

@author: pcarbonn
'''

from pyDatalog import pyDatalog
import random
import threading

pyDatalog.create_atoms('N,X0,X1,X2,X3,X4,X5,X6,X7')
pyDatalog.create_atoms('ok,queens0,queens1,queens2,queens3,queens4,queens5,queens6,queens7')

@pyDatalog.predicate()
def ok3(X1, N, X2):
    if (X1.id!=X2.id) and (X1.id!= X2.id+N.id) and (X1.id!=X2.id-N.id):
        yield (X1.id, N.id, X2.id)

def queen(thread_name):
    n = int(random.random() * 8) + 1 # 1 to 8
    pyDatalog.clear()
    
    queens0(X0) <= X0._in(range(n))
    queens1(X0,X1) <= queens0(X0) & queens0(X1) & ok(X1,1,X0)
    queens2(X0,X1,X2) <= queens1(X0,X1) & queens1(X1,X2) & ok(X0,2,X2)
    queens3(X0,X1,X2,X3) <= queens2(X0,X1,X2) & queens2(X1,X2,X3) & ok(X0,3,X3)
    queens4(X0,X1,X2,X3,X4) <= queens3(X0,X1,X2,X3) & queens3(X1,X2,X3,X4) & ok(X0,4,X4)
    queens5(X0,X1,X2,X3,X4,X5) <= queens4(X0,X1,X2,X3,X4) & queens4(X1,X2,X3,X4,X5) & ok(X0,5,X5)
    queens6(X0,X1,X2,X3,X4,X5,X6) <= queens5(X0,X1,X2,X3,X4,X5) & queens5(X1,X2,X3,X4,X5,X6) & ok(X0,6,X6)
    queens7(X0,X1,X2,X3,X4,X5,X6,X7) <= queens6(X0,X1,X2,X3,X4,X5,X6) & queens6(X1,X2,X3,X4,X5,X6,X7) & ok(X0,7,X7)
    
    queens = pyDatalog.ask("queens%s(%s)" % (n-1, ",".join("X%s" % i for i in range(n))))
    answers = queens.answers if queens else [] 
    result = "OK" if len(answers) == [1,0,0,2,10,4,40,92][n-1] else "* not OK ! *"
    print("%s : n = %d %s " % (thread_name, n, result))
    
for i in range(20):
    # queen("test %s" % i)
    t = threading.Thread(target=queen,args=("thread %02d" % i,))
    t.start()

