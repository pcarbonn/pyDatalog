"""
pyDatalog

Copyright (C) 2012 Pierre Carbonnelle

This library is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as
published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.

This library is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc.  51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

"""
import cProfile
import math
import re
import six
import time

import pyDatalog
def test():

    # test of expressions
    pyDatalog.load("""
        + p(a) # p is a proposition
    """)
    assert pyDatalog.ask('p(a)') == set([('a',)])
    
    pyDatalog.assert_fact('p', 'a', 'b')
    assert pyDatalog.ask('p(a, "b")') == set([('a', 'b')])
    pyDatalog.retract_fact('p', 'a', 'b')
    assert pyDatalog.ask('p(a, "b")') == None
    
    """unary facts                                                            """
    
    @pyDatalog.program()
    def unary(): 
        +z()
        assert ask(z()) == set([()])
        
        + p(a) 
        # check that unary queries work
        assert ask(p(a)) == set([('a',)])
        assert ask(p(X)) == set([('a',)])
        assert ask(p(Y)) == set([('a',)])
        assert ask(p(_X)) == set([('a',)])
        assert ask(p(b)) == None
        assert ask(p(a) & p(b)) == None
        
        + p(b)
        assert ask(p(X), _fast=True) == set([('a',), ('b',)])
        
        + p(b) # facts are unique
        assert ask(p(X)) == set([('a',), ('b',)])
        
        - p(b) # retract a unary fact
        assert ask(p(X)) == set([('a',)])
        
        - p(a)
        assert ask(p(X)) == None
        + p(a)
        
        # strings and integers
        + p('c')
        assert ask(p(c)) == set([('c',)])
        
        + p(1)
        assert ask(p(1)) == set([(1,)])
        
        + n(None)
        assert ask(n(X)) == set([(None,)])
        assert ask(n(None)) == set([(None,)])
        
        # spaces and uppercase in strings
        + farmer('Moshe dayan')
        + farmer('omar')
        assert ask(farmer(X)) == set([('Moshe dayan',), ('omar',)])

    # execute queries in a python program
    moshe_is_a_farmer = pyDatalog.ask("farmer('Moshe dayan')")
    assert moshe_is_a_farmer == set([('Moshe dayan',)])

    """ binary facts                                                         """
    
    @pyDatalog.program()
    def binary(): 
        + q(a, b)
        assert ask(q(a, b)) == set([('a', 'b')])
        assert ask(q(X, b)) == set([('a', 'b')])
        assert ask(q(a, Y)) == set([('a', 'b')])
        assert ask(q(a, c)) == None
        assert ask(q(X, Y)) == set([('a', 'b')])
        
        + q(a,c)
        assert ask(q(a, Y)) == set([('a', 'b'), ('a', 'c')])
        
        - q(a,c)
        assert ask(q(a, Y)) == set([('a', 'b')])
        
        assert ask(q(X, X)) == None 
        +q(a, a)
        assert ask(q(X, X)) == set([('a', 'a')])
        -q(a, a) 
        
    """ (in)equality                                             """

    @pyDatalog.program()
    def equality():
        assert ask(X==1) == set([(1,)]) 
        assert ask(X==Y) == None
        assert ask(X==Y+1) == None
        assert ask((X==1) & (Y==1) & (X==Y)) == set([(1,1)])
        assert ask((X==1) & (Y==2) & (X==Y-1)) == set([(1,2)])
        #assert ask((X==1) & (Y==2) & (X+2==Y+1)) == set([(1,2)])
        assert ask((X==2) & (Y==X/2)) == set([(2,1)])
        assert ask((X==2) & (Y==X//2)) == set([(2,1)])
        
        assert ask((X==1) & (Y==1+X)) == set([(1,2)])
        assert ask((X==1) & (Y==1-X)) == set([(1,0)])
        assert ask((X==1) & (Y==2*X)) == set([(1,2)])
        assert ask((X==2) & (Y==2/X)) == set([(2,1)])
        assert ask((X==2) & (Y==2//X)) == set([(2,1)])

    """ Conjunctive queries                                             """

    @pyDatalog.program()
    def conjuctive(): 
        assert ask(q(X, Y) & p(X)) == set([('a', 'b')])

        assert ask(p(X) & p(a)) == set([('a',),('c',),(1,)])
        assert ask(p(X) & p(Y) & (X==Y)) == set([('a', 'a'), ('c', 'c'), (1, 1)])
        assert ask(p(X) & p(Y) & (X==Y) & (Y==a)) == set([('a', 'a')])

        assert ask(q(X, Y)) == set([('a', 'b')])
        assert ask(q(X, Y) & p(X)) == set([('a', 'b')])
    
    @pyDatalog.program()
    def equality2():
        assert ask((X==1) & (X<X+1)) == set([(1,)]) 
        assert ask((X==1) & (Y==X)) == set([(1,1)]) 
        assert ask((X==1) & (Y==X+1)) == set([(1,2)])
        assert ask((X==1) & (Y==X+1) & (X<Y)) == set([(1,2)])
        assert ask((X==1) & (X<1)) == None
        assert ask((X==1) & (X<=1)) == set([(1,)])
        assert ask((X==1) & (X>1)) == None
        assert ask((X==1) & (X>=1)) == set([(1,)])
        assert ask(X in (1,)) == set([(1,)])
        assert ask((X==1) & (X not in (2,))) == set([(1,)])
        assert ask((X==1) & ~(X in (2,))) == set([(1,)])
        assert ask((X==1) & (X not in (1,))) == None
        assert ask((X==1) & ~(X in (1,))) == None

    @pyDatalog.program()
    def equality3():
        # equality (must be between parenthesis):
        s(X) <= (X == a)
        assert ask(s(X)) == set([('a',)])
        s(X) <= (X == 1)
        assert ask(s(X)) == set([(1,), ('a',)])
        
        s(X, Y) <= p(X) & (X == Y)
        assert ask(s(a, a)) == set([('a', 'a')])
        assert ask(s(a, b)) == None
        assert ask(s(X,a)) == set([('a', 'a')])
        assert ask(s(X, Y)) == set([('a', 'a'),('c', 'c'),(1, 1)])

    assert pyDatalog.ask('p(a)') == set([('a',)])

    """ clauses                                                         """
    
    @pyDatalog.program()
    def clauses(): 
    
        p2(X) <= p(X)
        assert ask(p2(a)) == set([('a',)])
        p2(X) <= p(X)
        
        r(X, Y) <= p(X) & p(Y)
        assert ask(r(a, a)) == set([('a', 'a')])
        assert ask(r(a, c)) == set([('a', 'c')])
        r(X, b) <= p(X)
        assert ask(r(a, b)) == set([('a', 'b')])
        
        - (r(X, b) <= p(X))
        assert ask(r(a, b)) == None
        
        # TODO more tests

        # integer variable
        for i in range(10):
            + successor(i+1, i)
        assert ask(successor(2, 1)) == set([(2, 1)])
        
        # built-in
        assert abs(-3)==3
        assert math.sin(3)==math.sin(3)
        
    
    """ in                                                         """
    
    pyDatalog.assert_fact('is_list', (1,2))

    @pyDatalog.program()
    def _in(): 
        assert ((X==1) & (X in (1,2))) == [(1,)]
        _in(X) <= (X in [1,2])
        assert ask(_in(1)) == set([(1,)])
        assert ask(_in(9)) == None
        assert ask(_in(X)) == set([(1,), (2,)])
        
        _in2(X) <= is_list(Y) & (X in Y)
        assert ask(_in2(X)) == set([(1,), (2,)])

        assert ask((Y==(1,2)) & (X==1) & (X in Y)) == set([((1, 2), 1)])
        assert ask((Y==(1,2)) & (X==1) & (X in Y+(3,))) == set([((1, 2), 1)])
                
    """ recursion                                                         """
    
    @pyDatalog.program()
    def recursion(): 
        + even(0)
        even(N) <= successor(N, N1) & odd(N1)
        odd(N) <= ~ even(N)
        assert ask(even(0)) == set([(0,)])
        assert ask(even(X)) == set([(4,), (10,), (6,), (0,), (2,), (8,)])
        assert ask(even(10)) == set([(10,)])
        assert ask(odd(1)) == set([(1,)])
        assert ask(odd(5)) == set([(5,)])
        assert ask(even(5)) == None
    
    """ recursion with expressions                                         """
    # reset the engine
    pyDatalog.clear()
    @pyDatalog.program()
    def recursive_expression(): 
        
        predecessor(X,Y) <= (X==Y-1)
        assert ask(predecessor(X,11)) == set([(10, 11)])
        
        p(X,Z) <= (Y==Z-1) & (X==Y-1)
        assert ask(p(X,11)) == set([(9, 11)])
        
        # odd and even
        + even(0)
        even(N) <= (N > 0) & odd(N-1)
        assert ask(even(0)) == set([(0,)])
        odd(N) <= (N > 0) & ~ even(N)
        assert ask(even(0)) == set([(0,)])
        assert ask(odd(1)) == set([(1,)])
        assert ask(odd(5)) == set([(5,)])
        assert ask(even(5)) == None
        assert ask((X==3) & odd(X+2)) == set([(3,)])
        
    # Factorial
    pyDatalog.clear()
    @pyDatalog.program()
    def factorial(): 
        (factorial[N] == F) <= (N < 2) & (F==1)
        (factorial[N] == F) <= (N > 1) & (F == N*factorial[N-1])
        assert ask(factorial[1] == F) == set([(1, 1)])
        assert ask(factorial[4] == F) == set([(4, 24)])
    
    # Fibonacci
    pyDatalog.clear()
    @pyDatalog.program()
    def fibonacci(): 
        (fibonacci[N] == F) <= (N == 0) & (F==0)
        (fibonacci[N] == F) <= (N == 1) & (F==1)
        (fibonacci[N] == F) <= (N > 1) & (F == fibonacci[N-1]+fibonacci[N-2])
        assert ask(fibonacci[1] == F) == set([(1, 1)])
        assert ask(fibonacci[4] == F) == set([(4, 3)])
        assert ask(fibonacci[18] == F) == set([(18, 2584)])

    # string manipulation
    @pyDatalog.program()
    def _lambda(): 
        split(X, Y,Z) <= (X == Y+'-'+Z)
        assert ask(split(X, 'a', 'b')) == set([('a-b', 'a', 'b')])
        split(X, Y,Z) <= (Y == (lambda X: X.split('-')[0])) & (Z == (lambda X: X.split('-')[1]))
        assert ask(split('a-b', Y, Z)) == set([('a-b', 'a', 'b')])
        assert ask(split(X, 'a', 'b')) == set([('a-b', 'a', 'b')])
        
        (two[X]==Z) <= (Z==X+(lambda X: X))
        assert ask(two['A']==Y) == set([('A','AA')])

    """ negation                                                     """    
    
    @pyDatalog.program()
    def _negation():
        +p(a, b)
        assert ask(~p(X, b)) == None
        assert ask(~p(X, c)) == set([('X', 'c')])

    pyDatalog.load("""
        + even(0)
        even(N) <= (N > 0) & (N1==N-1) & odd(N1)
        odd(N) <= (N2==N+2) & ~ even(N) & (N2>0)
    """)
    assert pyDatalog.ask('~ odd(7)', _fast=True) == None
    assert pyDatalog.ask('~ odd(2)', _fast=True) == set([(2,)])
    assert pyDatalog.ask('odd(3)', _fast=True) == set([(3,)])
    assert pyDatalog.ask('odd(3)'             ) == set([(3,)])
    assert pyDatalog.ask('odd(5)', _fast=True) == set([(5,)])
    assert pyDatalog.ask('odd(5)'            ) == set([(5,)])
    assert pyDatalog.ask('even(5)', _fast=True) == None
    assert pyDatalog.ask('even(5)'            ) == None
    
    """ functions                                                         """
    pyDatalog.clear()
    @pyDatalog.program()
    def function(): 
        + (f[a]==b)
        assert ask(f[X]==Y) == set([('a', 'b')])
        assert ask(f[X]==b) == set([('a', 'b')]) #TODO remove 'b' from result
        assert ask(f[a]==X) == set([('a', 'b')])
        assert ask(f[a]==b) == set([('a', 'b')])
    
        + (f[a]==c)
        assert ask(f[a]==X) == set([('a', 'c')])
        
        + (f[a]==a)
        assert ask(f[f[a]]==X) == set([('a',)])
        - (f[a]==a)
        assert ask(f[f[a]]==X) == None
        + (f[a]==c)

        + (f2[a,x]==b)
        assert ask(f2[a,x]==b) == set([('a', 'x', 'b')])
    
        + (f2[a,x]==c)
        assert ask(f2[a,x]==X) == set([('a', 'x', 'c')])
        
        g[X] = f[X]+f[X]
        assert(ask(g[a]==X)) == set([('a', 'cc')])
        
        h(X,Y) <= (f[X]==Y)
        assert (ask(h(X,'c'))) == set([('a', 'c')])
        assert (ask(h(X,Y))) == set([('a', 'c')])

    @pyDatalog.program()
    def function_comparison(): 
        assert ask(f[X]==Y) == set([('a', 'c')])
        assert ask(f[a]<'d') == set([('c',)])
        assert ask(f[a]>'a') == set([('c',)])
        assert ask(f[a]>='c') == set([('c',)])
        assert ask(f[a]>'c') == None
        assert ask(f[a]<='c') == set([('c',)])
        assert ask(f[a]>'c') == None
        assert ask(f[a] in ['c',]) == set([('c',)])
        
        assert ask((f[X]=='c') & (f[Y]==f[X])) == set([('a', 'a')])
        assert ask((f[X]=='c') & (f[Y]==f[X]+'')) == set([('a', 'a')])
        assert ask((f[X]=='c') & (f[Y]==(lambda X : 'c'))) == set([('a', 'a')])

        assert ask(f[X]==Y+'') == None
        assert ask((Y=='c') &(f[X]==Y+'')) == set([('c', 'a')])
        assert ask((Y=='c') &(f[X]<=Y+'')) == set([('c', 'a')])
        assert ask((Y=='c') &(f[X]<Y+'')) == None
        assert ask((Y=='c') &(f[X]<'d'+Y+'')) == set([('c', 'a')])
        assert ask((Y==('a','c')) & (f[X] in Y)) == set([(('a', 'c'), 'a')])
        assert ask((Y==('a','c')) & (f[X] in (Y+('z',)))) == set([(('a', 'c'), 'a')])

    @pyDatalog.program()
    def function_negation(): 
        assert not(ask(~(f[a]<'d'))) 
        assert not(ask(~(f[X]<'d'))) 
        assert ask(~(f[a] in('d',)))
        
    """ aggregates                                                         """
    
    pyDatalog.clear()
    @pyDatalog.program()
    def sum(): 
        + p(a, c, 1)
        + p(b, b, 4)
        + p(a, b, 1)

        assert(sum(1,2)) == 3
        (a_sum[X] == sum(Y, key=Z)) <= p(X, Z, Y)
        assert ask(a_sum[X]==Y) == set([('a', 2), ('b', 4)])
        assert ask(a_sum[a]==X) == set([('a', 2)])
        assert ask(a_sum[a]==2) == set([('a', 2)])
        assert ask(a_sum[X]==4) == set([('b', 4)])
        assert ask(a_sum[c]==X) == None
        assert ask((a_sum[X]==2) & (p(X, Z, Y))) == set([('a', 'c', 1), ('a', 'b', 1)])

        (a_sum2[X] == sum(Y, for_each=X)) <= p(X, Z, Y)
        assert ask(a_sum2[a]==X) == set([('a', 1)])

        (a_sum3[X] == sum(Y, key=(X,Z))) <= p(X, Z, Y)
        assert ask(a_sum3[X]==Y) == set([('a', 2), ('b', 4)])
        assert ask(a_sum3[a]==X) == set([('a', 2)])

    @pyDatalog.program()
    def len(): 
        assert(len((1,2))) == 2
        (a_len[X] == len(Z)) <= p(X, Z, Y)
        assert ask(a_len[X]==Y) == set([('a', 2), ('b', 1)])
        assert ask(a_len[a]==X) == set([('a', 2)])
        assert ask(a_len[X]==1) == set([('b', 1)])
        assert ask(a_len[X]==5) == None
        
        (a_lenY[X] == len(Y)) <= p(X, Z, Y)
        assert ask(a_lenY[a]==X) == set([('a', 1)])
        assert ask(a_lenY[c]==X) == None
        
        (a_len2[X,Y] == len(Z)) <= p(X, Y, Z)
        assert ask(a_len2[a,b]==X) == set([('a', 'b', 1)])
        assert ask(a_len2[a,X]==Y) == set([('a', 'b', 1), ('a', 'c', 1)])

        + q(a, c, 1)
        + q(a, b, 2)
        + q(b, b, 4)

    @pyDatalog.program()
    def concat(): 
        (a_concat[X] == concat(Y, key=Z, sep='+')) <= q(X, Y, Z)
        assert ask(a_concat[X]==Y) == set([('b', 'b'), ('a', 'c+b')])
        assert ask(a_concat[a]=='c+b') == set([('a', 'c+b')])
        assert ask(a_concat[a]==X) == set([('a', 'c+b')])
        assert ask(a_concat[X]==b) == set([('b', 'b')])

        (a_concat2[X] == concat(Y, order_by=(Z,), sep='+')) <= q(X, Y, Z)
        assert ask(a_concat2[a]==X) == set([('a', 'c+b')])

        (a_concat3[X] == concat(Y, key=(-Z,), sep='-')) <= q(X, Y, Z)
        assert ask(a_concat3[a]==X) == set([('a', 'b-c')])

    @pyDatalog.program()
    def min(): 
        assert min(1,2) == 1
        (a_min[X] == min(Y, key=Z)) <= q(X, Y, Z)
        assert ask(a_min[X]==Y) == set([('b', 'b'), ('a', 'c')])
        assert ask(a_min[a]=='c') == set([('a', 'c')])
        assert ask(a_min[a]==X) == set([('a', 'c')])
        assert ask(a_min[X]=='b') == set([('b', 'b')])
        
        (a_minD[X] == min(Y, order_by=-Z)) <= q(X, Y, Z)
        assert ask(a_minD[a]==X) == set([('a', 'b')])
        
        (a_min2[X, Y] == min(Z, key=(X,Y))) <= q(X, Y, Z)
        assert ask(a_min2[Y, b]==X) == set([('a', 'b', 2),('b', 'b', 4)])
        assert ask(a_min2[Y, Y]==X) == set([('b', 'b', 4)]), "a_min2"
        
        (a_min3[Y] == min(Z, key=(-X,Z))) <= q(X, Y, Z)
        assert ask(a_min3[b]==Y) == set([('b', 4)]), "a_min3"
        
    @pyDatalog.program()
    def max(): 
        assert max(1,2) == 2
        (a_max[X] == max(Y, key=-Z)) <= q(X, Y, Z)
        assert ask(a_max[a]==X) == set([('a', 'c')])
        
        (a_maxD[X] == max(Y, order_by=Z)) <= q(X, Y, Z)
        assert ask(a_maxD[a]==X) == set([('a', 'b')])

    @pyDatalog.program()
    def rank(): 
        (a_rank1[Z] == rank(for_each=Z, order_by=Z)) <= q(X, Y, Z)
        assert ask(a_rank1[X]==Y) == set([(1, 0), (2, 0), (4, 0)])
        assert ask(a_rank1[X]==0) == set([(1, 0), (2, 0), (4, 0)])
        assert ask(a_rank1[1]==X) == set([(1, 0)])
        assert ask(a_rank1[1]==0) == set([(1, 0)])
        assert ask(a_rank1[1]==1) == None

        # rank
        (a_rank[X,Y] == rank(for_each=(X,Y2), order_by=Z2)) <= q(X, Y, Z) & q(X,Y2,Z2)
        assert ask(a_rank[X,Y]==Z) == set([('a', 'b', 1), ('a', 'c', 0), ('b', 'b', 0)])
        assert ask(a_rank[a,b]==1) == set([('a', 'b', 1)])
        assert ask(a_rank[a,b]==Y) == set([('a', 'b', 1)])
        assert ask(a_rank[a,X]==0) == set([('a', 'c', 0)])
        assert ask(a_rank[a,X]==Y) == set([('a', 'b', 1), ('a', 'c', 0)])
        assert ask(a_rank[X,Y]==1) == set([('a', 'b', 1)])
        assert ask(a_rank[a,y]==Y) == None
        # reversed
        (b_rank[X,Y] == rank(for_each=(X,Y2), order_by=-Z2)) <= q(X, Y, Z) & q(X,Y2,Z2)
        assert ask(b_rank[X,Y]==Z) == set([('a', 'b', 0), ('a', 'c', 1), ('b', 'b', 0)])
        assert ask(b_rank[a,b]==0) == set([('a', 'b', 0)])
        assert ask(b_rank[a,b]==Y) == set([('a', 'b', 0)])
        assert ask(b_rank[a,X]==1) == set([('a', 'c', 1)])
        assert ask(b_rank[a,X]==Y) == set([('a', 'b', 0), ('a', 'c', 1)])
        assert ask(b_rank[X,Y]==0) == set([('a', 'b', 0), ('b', 'b', 0)])
        assert ask(b_rank[a,y]==Y) == None

    @pyDatalog.program()
    def running_sum(): 
        # running_sum
        (a_run_sum[X,Y] == running_sum(Z2, for_each=(X,Y2), order_by=Z2)) <= q(X, Y, Z) & q(X,Y2,Z2)
        assert ask(a_run_sum[X,Y]==Z) == set([('a', 'b', 3), ('a', 'c', 1), ('b', 'b', 4)])
        assert ask(a_run_sum[a,b]==3) == set([('a', 'b', 3)])
        assert ask(a_run_sum[a,b]==Y) == set([('a', 'b', 3)])
        assert ask(a_run_sum[a,X]==1) == set([('a', 'c', 1)])
        assert ask(a_run_sum[a,X]==Y) == set([('a', 'b', 3), ('a', 'c', 1)])
        assert ask(a_run_sum[X,Y]==4) == set([('b', 'b', 4)])
        assert ask(a_run_sum[a,y]==Y) == None

        (b_run_sum[X,Y] == running_sum(Z2, for_each=(X,Y2), order_by=-Z2)) <= q(X, Y, Z) & q(X,Y2,Z2)
        assert ask(b_run_sum[X,Y]==Z) == set([('a', 'b', 2), ('a', 'c', 3), ('b', 'b', 4)])
        assert ask(b_run_sum[a,b]==2) == set([('a', 'b', 2)])
        assert ask(b_run_sum[a,b]==Y) == set([('a', 'b', 2)])
        assert ask(b_run_sum[a,X]==3) == set([('a', 'c', 3)])
        assert ask(b_run_sum[a,X]==Y) == set([('a', 'b', 2), ('a', 'c', 3)])
        assert ask(b_run_sum[X,Y]==4) == set([('b', 'b', 4)])
        assert ask(b_run_sum[a,y]==Y) == None

    """ simple in-line queries                                        """
    X = pyDatalog.Variable()
    assert ((X==1) >= X) == 1
    assert ((X==1) & (X!=2) >= X) == 1
    assert set(X._in((1,2))) == set([(1,),(2,)])

    """ interface with python classes                                        """

    class A(pyDatalog.Mixin):
        def __init__(self, b):
            super(A, self).__init__()
            self.b = b
        def __repr__(self):
            return self.b
        @pyDatalog.program() # indicates that the following method contains pyDatalog clauses
        def _():
            (A.c[X]==N) <= (A.b[X]==N)
            (A.len[X]==len(N)) <= (A.b[X]==N)
        @classmethod
        def _pyD_x1(cls, X):
            if X.is_const() and X.id.b == 'za':
                yield (X.id,)
            else:
                for X in pyDatalog.metaMixin.__refs__[cls]:
                    if X.b == 'za':
                        yield (X,)
            
    a = A('a')
    b = A('b')
    assert a.c == 'a'
    X, Y = pyDatalog.variables(2)
    assert (A.c[X]=='a') == [(a,)]
    assert (A.c[X]=='a')[0] == (a,)
    assert list(X) == [a]
    assert X.v() == a
    assert ((A.c[a]==X) >= X) == 'a'
    assert ((A.c[a]==X) & (A.c[a]==X) >= X) == 'a'
    assert ((A.c[a]==X) & (A.c[b]==X) >= X) == None
    (A.c[X]=='b') & (A.b[X]=='a')
    assert list(X) == []
    (A.c[X]=='a') & (A.b[X]=='a')
    assert list(X) == [a]
    result = (A.c[X]=='a') & (A.b[X]=='a')
    assert result == [(a,)]
    assert (A.c[a] == 'a') == [()]
    assert (A.b[a] == 'a') == [()]
    assert (A.c[a]=='a') & (A.b[a]=='a') == [()]
    assert (A.b[a]=='f') == []
    assert ((A.c[a]=='a') & (A.b[a]=='f')) == []
    
    """ filters on python classes                                        """
    assert (A.b[X]!=Y) == [(a, None), (b, None)]
    assert (A.b[X]!='a') == [(b,)]
    assert (A.b[X]!='z') == [(a,), (b,)]
    assert (A.b[a]!='a') == []
    assert list(A.b[b]!='a') == [()]
    assert ((A.b[b]!='a') & (A.b[b]!='z')) == [()]

    assert (A.b[X]<Y) == [(a, None), (b, None)]
    assert (A.b[X]<'a') == []
    assert (A.b[X]<'z') == [(a,), (b,)]
    assert (A.b[a]<'b') == [()]
    assert (A.b[b]<'a') == []
    assert ((A.b[b]<'z') & (A.b[b]!='z')) == [()]

    assert (A.b[X]<='a') == [(a,)]
    assert (A.b[X]<='z') == [(a,), (b,)]
    assert (A.b[a]<='b') == [()]
    assert (A.b[b]<='a') == []
    assert ((A.b[b]<='z') & (A.b[b]!='z')) == [()]

    assert (A.b[X]>'a') == [(b,)]
    assert (A.b[X]>='a') == [(a,), (b,)]

    assert (A.c[X]<='a') == [(a,)]
    assert (A.c[X]<='a'+'') == [(a,)]

    assert (A.c[X]._in(('a',))) == [(a,)]
    assert (A.c[X]._in(('a',)+('z',))) == [(a,)]
    assert ((Y==('a',)) & (A.c[X]._in(Y))) == [(('a',), a)] # TODO make ' in ' work
    
    assert ((Y==('a',)) & (A.c[X]._in(Y+('z',)))) == [(('a',), a)] # TODO make ' in ' work
    assert (A.c[X]._in(('z',))) == []

    # more complex queries
    assert ((Y=='a') & (A.b[X]!=Y)) == [('a', b)] # order of appearance of the variables !
    
    assert (A.len[X]==Y) == [(b, 1), (a, 1)]
    assert (A.len[a]==Y) == [(1,)]

    """ subclass                                              """

    class Z(A):
        def __init__(self, z):
            super(Z, self).__init__(z+'a')
            self.z = z
        def __repr__(self):
            return self.z
        @pyDatalog.program() # indicates that the following method contains pyDatalog clauses
        def _():
            (Z.w[X]==N) <= (Z.z[X]!=N)
        @classmethod
        def _pyD_query(cls, pred_name, args):
            if pred_name == 'Z.pred':
                if args[0].is_const() and args[0].id.b != 'za':
                    yield (args[0].id,)
                else:
                    for X in pyDatalog.metaMixin.__refs__[cls]:
                        if X.b != 'za':
                            yield (X,)
            else:
                raise AttributeError
    
    z = Z('z')
    assert z.z == 'z'
    assert (Z.z[X]=='z') == [(z,)]
    assert ((Z.z[X]=='z') & (Z.z[X]>'a')) == [(z,)]
    assert list(X) == [z]
    try:
        a.z == 'z'
    except Exception as e:
        e_message = e.message if hasattr(e, 'message') else e.args[0]
        if e_message != "Predicate without definition (or error in resolver): A.z[1]==/2":
            print(e_message)
    else:
        assert False
    
    try:
        (Z.z[a] == 'z') == None
    except Exception as e:
        e_message = e.message if hasattr(e, 'message') else e.args[0]
        if e_message != "Object is incompatible with the class that is queried.":
            print(e_message)
    else:
        assert False

    assert (Z.b[X]==Y) == [(z, 'za')]
    assert (Z.c[X]==Y) == [(z, 'za')]
    assert ((Z.c[X]==Y) & (Z.c[X]>'a')) == [(z, 'za')]
    assert (Z.c[X]>'a') == [(z,)]
    assert (z.b) == 'za'
    assert (z.c) == 'za'
    
    w = Z('w')
    w = Z('w') # duplicated to test __refs__[cls]
    assert(Z.x(X)) == [(z,)]
    assert not (~Z.x(z))
    assert ~Z.x(w)
    assert ~ (Z.z[w]=='z')
    assert(Z.pred(X)) == [(w,)]
    assert(Z.pred(X) & ~ (Z.z[X]>='z')) == [(w,)]
    assert(Z.x(X) & ~(Z.pred(X))) == [(z,)]

    assert (Z.len[X]==Y) == [(w, 1), (z, 1)]
    assert (Z.len[z]==Y) == [(1,)]
            
    """ python resolvers                                              """
    
    @pyDatalog.predicate()
    def p(X,Y):
        yield (1,2)
        yield (2,3)
    
    assert pyDatalog.ask('p(X,Y)') == set([(1, 2), (2, 3)])
    assert pyDatalog.ask('p(1,Y)') == set([(1, 2)])
    assert pyDatalog.ask('p(1,2)') == set([(1, 2)])
    
    """ error detection                                              """
    
    @pyDatalog.program()
    def _(): 
        pass
    error = False
    try:
        _()
    except: error = True
    assert error

    def assert_error(code, message='^$'):
        _error = False
        try:
            pyDatalog.load(code)
        except Exception as e:
            e_message = e.message if hasattr(e, 'message') else e.args[0] # python 2 and 3
            if not re.match(message, e_message):
                print(e_message) 
            _error = True
        assert _error
        
    def assert_ask(code, message='^$'):
        _error = False
        try:
            pyDatalog.ask(code)
        except Exception as e: 
            e_message = e.message if hasattr(e, 'message') else e.args[0]
            if not re.match(message, e_message):
                print(e_message) 
            _error = True
        assert _error

    assert_error('ask(z(a),True)', 'Too many arguments for ask \!')
    assert_error('ask(z(a))', 'Predicate without definition \(or error in resolver\): z/1')
    assert_error("+ farmer(farmer(moshe))", "Syntax error: Literals cannot have a literal as argument : farmer\(\[\],\)")
    assert_error("+ manager[Mary]==John", "bad operand type for unary \+: 'Function'. Please consider adding parenthesis")
    assert_error("manager[X]==Y <= (X==Y)", "Syntax error: please verify parenthesis around \(in\)equalities")
    assert_error("p(X) <= (Y==2)", "Can't create clause")
    assert_error("p(X) <= X==1 & X==2", "Syntax error: please verify parenthesis around \(in\)equalities")
    assert_error("p(X) <= (manager[X]== min(X))", "Error: argument missing in aggregate")
    assert_error("p(X) <= (manager[X]== min(X, order_by=X))", "Aggregation cannot appear in the body of a clause")
    assert_error("q(min(X, order_by=X)) <= p(X)", "Syntax error: Incorrect use of aggregation\.")
    assert_error("manager[X]== min(X, order_by=X) <= manager(X)", "Syntax error: please verify parenthesis around \(in\)equalities")
    assert_error("ask(X<1)", 'Error: left hand side of comparison must be bound: =X<1/1')
    assert_error("ask(X<Y)", 'Error: left hand side of comparison must be bound: =X<Y/2')
    assert_error("ask(1<Y)", 'Error: left hand side of comparison must be bound: =Y>1/1')
    assert_error("ask( (A.c[X]==Y) & (Z.c[X]==Y))", "TypeError: First argument of Z.c\[1\]==\('.','.'\) must be a Z, not a A ")
    assert_ask("A.u[X]==Y", "Predicate without definition \(or error in resolver\): A.u\[1\]==/2")
    assert_ask("A.u[X,Y]==Z", "Predicate without definition \(or error in resolver\): A.u\[2\]==/3")
    assert_error('(a_sum[X] == sum(Y, key=Y)) <= p(X, Z, Y)', "Error: Duplicate definition of aggregate function.")
    assert_error('(two(X)==Z) <= (Z==X+(lambda X: X))', 'Syntax error near equality: consider using brackets. two\(X\)')
    assert_error('p(X) <= sum(X, key=X)', 'Invalid body for clause')
    assert_error('ask(- manager[X]==1)', "bad operand type for unary -: 'Function'. Please consider adding parenthesis")
    assert_error("p(X) <= (X=={})", "Syntax error: Symbol or Expression expected")

    """ SQL Alchemy                    """

    from sqlalchemy import create_engine
    from sqlalchemy import Column, Integer, String, ForeignKey
    from sqlalchemy.ext.declarative import declarative_base
    from sqlalchemy.orm import sessionmaker, relationship
    
    engine = create_engine('sqlite:///:memory:', echo=False) # create database in memory
    Session = sessionmaker(bind=engine)
    session = Session()

    Base = declarative_base(cls=pyDatalog.Mixin, metaclass=pyDatalog.sqlMetaMixin)
    Base.session = session
        
    class Employee(Base): # --> Employee inherits from the Base class
        __tablename__ = 'employee'
        
        name = Column(String, primary_key=True)
        manager_name = Column(String, ForeignKey('employee.name'))
        salary = Column(Integer)
        
        def __init__(self, name, manager_name, salary):
            super(Employee, self).__init__()
            self.name = name
            self.manager_name = manager_name # direct manager of the employee, or None
            self.salary = salary # monthly salary of the employee
        def __repr__(self): # specifies how to display the employee
            return "Employee: %s" % self.name
    
        @pyDatalog.program() # --> the following function contains pyDatalog clauses
        def Employee():
            (Employee.manager[X]==Y) <= (Employee.manager_name[X]==Z) & (Z==Employee.name[Y])
            # the salary class of employee X is computed as a function of his/her salary
            # this statement is a logic equality, not an assignment !
            Employee.salary_class[X] = Employee.salary[X]//1000
            
            # all the indirect managers of employee X are derived from his manager, recursively
            Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Y) & (Y != None)
            Employee.indirect_manager(X,Y) <= (Employee.manager[X]==Z) & Employee.indirect_manager(Z,Y) & (Y != None)
            
            # count the number of reports of X
            (Employee.report_count[X] == len(Y)) <= Employee.indirect_manager(Y,X)

    Base.metadata.create_all(engine) 
    
    John = Employee('John', None, 6800)
    Mary = Employee('Mary', 'John', 6300)
    Sam = Employee('Sam', 'Mary', 5900)
    
    session.add(John)
    session.add(Mary)
    session.add(Sam)
    session.commit()
    
    assert (John.salary_class ==6) 
    
    X = pyDatalog.Variable()
    Employee.salary[X] == 6300 # notice the similarity to a pyDatalog query
    assert (X._value() == [Mary,]) # prints [Employee: Mary]
    assert (X.v() == Mary) # prints Employee:Mary

    Employee.indirect_manager(Mary, X)
    assert (X.v() == John) # prints [Employee: John]

    print("Test completed successfully.")

if __name__ == "__main__":
    test()
    #cProfile.runctx('test()', globals(), locals())