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
import time
import six

import pyDatalog
def test():

    # test of expressions
    pyDatalog.load("""
        + p(a) # p is a proposition
    """)
    assert pyDatalog.ask('p(a)') == set([('a',)])
    
    pyDatalog.clear()
    assert pyDatalog.ask('p(a)') == None
    
    pyDatalog.assert_fact('p', 'a', 'b')
    assert pyDatalog.ask('p(a, "b")') == set([('a', 'b')])
    
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
        
    """ (in)equality                                             """

    @pyDatalog.program()
    def equality():
        assert ask(X==1) == set([(1,)]) 
        assert ask(X==Y) == None
        assert ask(X==Y+1) == None

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

    """ clauses                                                         """
    
    @pyDatalog.program()
    def clauses(): 
    
        p2(X) <= p(X)
        assert ask(p2(a)) == set([('a',)])
        
        r(X, Y) <= p(X) & p(Y)
        assert ask(r(a, a)) == set([('a', 'a')])
        assert ask(r(a, c)) == set([('a', 'c')])
        r(X, b) <= p(X)
        assert ask(r(a, b)) == set([('a', 'b')])
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
        # TODO  make this work
        # s <= (X == Y)   
        # assert ask(s(X,Y)) == set([('a', 'a'),('c', 'c'),('1', '1')])

    assert pyDatalog.ask('p(a)') == set([('a',)])
    
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
        even(N) <= (N > 0) & (N1==N-1) & odd(N1)
        assert ask(even(0)) == set([(0,)])
        odd(N) <= (N > 0) & ~ even(N)
        assert ask(even(0)) == set([(0,)])
        assert ask(odd(1)) == set([(1,)])
        assert ask(odd(5)) == set([(5,)])
        assert ask(even(5)) == None
        
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

    """ negation                                                     """    
    
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
        assert ask(f[a]==b) == set([('a', 'b')])
    
        + (f[a]==c)
        assert ask(f[a]==X) == set([('a', 'c')])

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

        assert ask(f[X]==Y+'') == None
        assert ask((Y=='c') &(f[X]==Y+'')) == set([('c', 'a', 'c')])
        assert ask((Y=='c') &(f[X]<=Y+'')) == set([('c', 'a', 'c')])
        assert ask((Y=='c') &(f[X]<Y+'')) == None
        assert ask((Y=='c') &(f[X]<'d'+Y+'')) == set([('c', 'a', 'c')])
        assert ask((Y==('a','c')) & (f[X] in Y)) == set([(('a', 'c'), 'a', 'c')])
        assert ask((Y==('a','c')) & (f[X] in (Y+('z',)))) == set([(('a', 'c'), 'a', 'c')])
        
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
            
    a = A('a')
    b = A('b')
    assert a.c == 'a'
    X, Y = pyDatalog.variables(2)
    assert (A.c[X]=='a') == [(a, 'a')]
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
    assert (A.c[a]=='a') & (A.b[a]=='a')
    assert not ((A.c[a]=='a') & (A.b[a]=='f'))
    
    """ filters on python classes                                        """
    assert (A.b[X]!=Y) == [(a, None), (b, None)]
    assert (A.b[X]!='a') == [(b, 'a')]
    assert (A.b[X]!='z') == [(a, 'z'), (b, 'z')]
    assert (A.b[a]!='a') == []
    assert list(A.b[b]!='a') == [(b, 'a')]
    assert ((A.b[b]!='a') & (A.b[b]!='z')) == [()]

    assert (A.b[X]<Y) == [(a, None), (b, None)]
    assert (A.b[X]<'a') == []
    assert (A.b[X]<'z') == [(a, 'z'), (b, 'z')]
    assert (A.b[a]<'b') == [(a, 'b')]
    assert (A.b[b]<'a') == []
    assert ((A.b[b]<'z') & (A.b[b]!='z')) == [()]

    assert (A.b[X]<='a') == [(a, 'a')]
    assert (A.b[X]<='z') == [(a, 'z'), (b, 'z')]
    assert (A.b[a]<='b') == [(a, 'b')]
    assert (A.b[b]<='a') == []
    assert ((A.b[b]<='z') & (A.b[b]!='z')) == [()]

    assert (A.b[X]>'a') == [(b, 'a')]
    assert (A.b[X]>='a') == [(a, 'a'), (b, 'a')]

    assert (A.c[X]<='a') == [(a, 'a')]
    assert (A.c[X]<='a'+'') == [(a, 'a')]

    assert (A.c[X]._in(('a',))) == [(a, 'a')]
    assert (A.c[X]._in(('a',)+('z',))) == [(a, 'a')]
    assert ((Y==('a',)) & (A.c[X]._in(Y))) == [(('a',), a, 'a')] # TODO make ' in ' work
    
    assert ((Y==('a',)) & (A.c[X]._in(Y+('z',)))) == [(('a',), a, 'a')] # TODO make ' in ' work
    assert (A.c[X]._in(('z',))) == []

    # more complex queries
    assert ((Y=='a') & (A.b[X]!=Y)) == [('a', b)] # order of appearance of the variables !

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

    def test_error(code):
        _error = False
        try:
            pyDatalog.load(code, catch_error=False)
        except Exception as e: 
            _error = True
        assert _error
        
    def test_ask(code):
        _error = False
        try:
            pyDatalog.ask(code)
        except: 
            _error = True
        assert _error

    test_error("+ farmer(farmer(moshe))")
    test_error("+ manager[Mary]==John")
    test_error("manager[X]==Y <= (X==Y)")
    test_error("p(X) <= X==1 & X==2")
    test_error("p(X) <= (manager[X]== min(X))")
    test_error("ask(X<1)")
    test_error("ask(X<Y)")
    test_error("ask(1<Y)")
        
    print("Test completed successfully.")

if __name__ == "__main__":
    test()
    #cProfile.runctx('test()', globals(), locals())