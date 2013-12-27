'''
Created on 7 October 2013

@author: pcarbonn
'''

from pyDatalog import pyDatalog, Logic
import copy
import random
import threading
import time

pyDatalog.create_terms('N,X0,X1,X2,X3,X4,X5,X6,X7')
pyDatalog.create_terms('ok,queens, next_queen')

def add_logic(n):
    ok(X1, N, X2) <= (X1!=X2) & (X1!= X2+N) & (X1!=X2-N)
    
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

def check_logic(thread_name, n):
    query = pyDatalog.ask("queens(%s)" % (",".join("X%s" % i for i in range(n))))
    answers = query.answers if query else [] 
    result = "OK" if len(answers) == [1,0,0,2,10,4,40,92][n-1] else answers
    print("%s : n = %d %s " % (thread_name, n, result))

def queen(thread_name, logic8, logic, n):
    print("start %s" % thread_name)
    Logic(logic8)
    check_logic(thread_name, 8)
    Logic(logic)
    check_logic(thread_name, n)

# create queen resolution logic for N = 1 to 8
logic = []    
for i in range(8):
    logic.append(Logic())
    add_logic(i+1)

Logic(logic[7])
check_logic("Main", 8)

Logic(logic[4])
check_logic("Main", 5)

# start 20 threads, each with a randomly-chosen logic    
for i in range(20):    
    n = int(random.random() * 8) + 1 # 1 to 8
    t = threading.Thread(target=queen,args=("thread %02d" % i, (logic[7]), logic[n-1], n))
    t.start()

