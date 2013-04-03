'''
Created on 2 avr. 2013

The HASHTAG problem was the subject of a contest at PyCon 2013.

Problem description : https://picasaweb.google.com/lh/photo/u_6ZWCrTUNVADkPAyyFvfKbReIcFkkE5l5WZw-QBbLc
Discussion : http://uthcode.appspot.com/2013/03/Hashtag-at-pycon

Below is a solution based on pyDatalog

@author: pcarbonn
'''
from pyDatalog import pyDatalog, pyEngine
import time

pyDatalog.create_atoms('solution,move,X,Y,Z,N,N1,N2')

# valid moves are exchange of 1<->2, 1<->4, ... 
@pyDatalog.predicate()
def move3(X,N,Y):
    for n1,n2 in ((0,1),(0,3),(1,2),(1,4),(2,5),(3,4),(3,6),(4,5),(4,7),(5,8),(6,7),(7,8)):
        if X.id[n1]=='*' or X.id[n2]=='*':
            y = list(X.id)
            y[n1], y[n2] = y[n2], y[n1]
            yield (X.id, '%s-%s,' % (n1+1, n2+1), ''.join(y))


# a solution to go from X to Y is a direct move, 
# or a solution from X to Z, followed by a direct move from Z to Y
(solution[X,Y]==N) <= move(X, N, Y)
(solution[X,Y]==N) <= (solution[X,Z]==N1) & move(Z,N2,Y) & (N==N1+N2)

start_time = time.time()

print((solution['HAAHS*T*G', 'HASHTAG**']==N) >= N)
# prints 5-6,2-5,2-3,3-6,5-6,7-8,5-8,8-9,7-8,
print("Computed in %f seconds" % (time.time() - start_time))

