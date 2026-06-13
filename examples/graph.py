"""
This file illustrates algorithms that can be used with graphs, trees, hierarchies, ...

    * which nodes can be reached from another node
    * what are the possible paths between 2 nodes
    * give me one path between 2 nodes (more efficient)
    * what is the shortest path between 2 nodes, given a cost function
    
"""

from pyDatalog import pyDatalog

#        4
#       /
#  1 - 2 - 3
#     / \
#    7   5
#     \ /
#      6       8 - 9

@pyDatalog.program()
def _():
	+link(1,2)
	+link(2,3)
	+link(2,4)
	+link(2,5)
	+link(5,6)
	+link(6,7)
	+link(7,2)
	+link(8,9)

	link(X,Y) <= link(Y,X) # optional : make each link bi-directional

	print (link(1,Y))

	print("can reach from 1")
	can_reach(X,Y) <= can_reach(X,Z) & link(Z,Y) & (X!=Y)
	can_reach(X,Y) <= link(X,Y)
	print (can_reach(1,Y))

	print("can't reach from 1")
	print(link(X,Y) & (~can_reach(1,X)) & (X !=1))

	print("all path from/to 1")
	all_path(X,Y,P) <= all_path(X,Z,P2) & link(Z,Y) & (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) & (P==P2+[Z])
	all_path(X,Y,P) <= link(X,Y) & (P==[])

	print (all_path(X,1,P))

	print("no path from 1")
	print(link(X,Y) & (~all_path(1,X,P)) & (X !=1))

	print("a path from / to 1")
	(path[X,Y]==P) <= ((path[X,Z]==P2) &  link(Z,Y)
					   # dropping next line is an unsafe optimisation. see negation below
					   #& (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) 
					   & (P==P2+[Z])) 
	(path[X,Y]==P) <= link(X,Y) & (P==[])

	print (path[1,Y]==P)

	print("not one path from 1")
	(safe_path[X,Y]==P) <= ((safe_path[X,Z]==P2) &  link(Z,Y)
					   # next line needed to avoid infinite loop in negation
					   & (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) 
					   & (P==P2+[Z])) 
	(safe_path[X,Y]==P) <= link(X,Y) & (P==[])
	print(link(X,Y) & (X !=1) & (~(safe_path[X,1]==P)) )

	print ("path with cost from / to 1")
	(path_with_cost(X,Y,P,C)) <= (path_with_cost(X,Z,P2,C2)) & link(Z,Y) & (X!=Y) & (X._not_in(P2)) & (Y._not_in(P2)) & (P==P2+[Z]) & (C==C2+1) 
	(path_with_cost(X,Y,P,C)) <= link(X,Y) & (P==[]) & (C==0)

	print (path_with_cost(1,Y,P,C))
	print (path_with_cost(Y,1,P,C))

	print ("shortest path from / to 1")
	(shortest_path[X,Y]==min_(P, order_by=C)) <= (path_with_cost(X,Y,P,C))

	print (shortest_path[1,Y]==P) 
	print (shortest_path[X,1]==P)