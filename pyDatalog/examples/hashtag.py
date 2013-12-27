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

pyDatalog.create_terms('star, move,solution,X,Y,N,N1,N2')
pyDatalog.create_terms('X1,X2,X3,X4,X5,X6,X7,X8,X9')
pyDatalog.create_terms('Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9')

# X has a star in position N1 or N2
star(X,N1,N2) <= (X[N1]=='*')
star(X,N1,N2) <= (X[N2]=='*')

# valid moves are exchange of 1<->2, 1<->4, ... 
X19 = (X1,X2,X3,X4,X5,X6,X7,X8,X9) # short hand
move(X19, '1-2,', Y) <= star(X19, 0, 1) & (Y==(X2,X1,X3,X4,X5,X6,X7,X8,X9))
move(X19, '1-4,', Y) <= star(X19, 0, 3) & (Y==(X4,X2,X3,X1,X5,X6,X7,X8,X9))

move(X19, '2-3,', Y) <= star(X19, 1, 2) & (Y==(X1,X3,X2,X4,X5,X6,X7,X8,X9))
move(X19, '2-5,', Y) <= star(X19, 1, 4) & (Y==(X1,X5,X3,X4,X2,X6,X7,X8,X9))

move(X19, '3-6,', Y) <= star(X19, 2, 5) & (Y==(X1,X2,X6,X4,X5,X3,X7,X8,X9))

move(X19, '4-5,', Y) <= star(X19, 3, 4) & (Y==(X1,X2,X3,X5,X4,X6,X7,X8,X9))
move(X19, '4-7,', Y) <= star(X19, 3, 6) & (Y==(X1,X2,X3,X7,X5,X6,X4,X8,X9))

move(X19, '5-6,', Y) <= star(X19, 4, 5) & (Y==(X1,X2,X3,X4,X6,X5,X7,X8,X9))
move(X19, '5-8,', Y) <= star(X19, 4, 7) & (Y==(X1,X2,X3,X4,X8,X6,X7,X5,X9))

move(X19, '6-9,', Y) <= star(X19, 5, 8) & (Y==(X1,X2,X3,X4,X5,X9,X7,X8,X6))

move(X19, '7-8,', Y) <= star(X19, 6, 7) & (Y==(X1,X2,X3,X4,X5,X6,X8,X7,X9))
move(X19, '8-9,', Y) <= star(X19, 7, 8) & (Y==(X1,X2,X3,X4,X5,X6,X7,X9,X8))

# a solution to go from X to Y is a direct move, 
# or a solution from X to Z, followed by a direct move from Z to Y
Z19 = (Z1,Z2,Z3,Z4,Z5,Z6,Z7,Z8,Z9)
(solution[X,Y]==N) <= move(X, N, Y)
(solution[X,Y]==N) <= (solution[X,Z19]==N1) & move(Z19,N2,Y) & (N==N1+N2)

start_time = time.time()

print((solution[list('HAAHS*T*G'), list('HASHTAG**')]==N) >= N)
# prints 5-6,2-5,2-3,3-6,5-6,7-8,5-8,8-9,7-8,
print("Computed in %f seconds" % (time.time() - start_time))

# For better performance, X19 would be a string, and 'move' would be implemented
# with a Python Resolver