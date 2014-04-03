"""
This file illustrates algorithms that can be used with graphs, trees, hierarchies, ...

    * which nodes can be reached from another node
    * what are the possible paths between 2 nodes
    * give me one path between 2 nodes (more efficient)
    * what is the shortest path between 2 nodes, given a cost function
    
"""

from pyDatalog import pyDatalog

pyDatalog.create_terms('link, can_reach, path, all_path, path_with_cost, shortest_path')
pyDatalog.create_terms('X, Y, Z, P, P2, C, C2')

#        4
#       /
#  1 - 2 - 3
#     / \
#    7   5
#     \ /
#      6

+link(1,2)
+link(2,3)
+link(2,4)
+link(2,5)
+link(5,6)
+link(6,7)
+link(7,2)

link(X,Y) <= link(Y,X) # optional : make each link bi-directional

print (link(1,Y))

print("can reach from 1")
can_reach(X,Y) <= link(X,Z) & can_reach(Z,Y) & (X!=Y)
can_reach(X,Y) <= link(X,Y)
print (can_reach(1,Y))

print("all path from/to 1")
all_path(X,Y,P) <= link(X,Z) & all_path(Z,Y,P2) & (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) & (P==[Z]+P2)
all_path(X,Y,P) <= link(X,Y) & (P==[])

print (all_path(1,Y,P))
print (all_path(X,1,P))

print("a path from / to 1")
(path[X,Y]==P) <= link(X,Z) & (path[Z,Y]==P2) & (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) & (P==[Z]+P2) 
(path[X,Y]==P) <= link(X,Y) & (P==[])

print (path[1,Y]==P)
print (path[Y,1]==P)

print ("path with cost from / to 1")
(path_with_cost(X,Y,P,C)) <= link(X,Z) & (path_with_cost(Z,Y,P2,C2)) & (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) & (P==[Z]+P2) & (C==C2+1) 
(path_with_cost(X,Y,P,C)) <= link(X,Y) & (P==[]) & (C==0)

print (path_with_cost(1,Y,P,C))
print (path_with_cost(Y,1,P,C))

print ("shortest path from / to 1")
(shortest_path[X,Y]==min_(P, order_by=C)) <= (path_with_cost(X,Y,P,C))

print (shortest_path[1,Y]==P) 
print (shortest_path[X,1]==P) 