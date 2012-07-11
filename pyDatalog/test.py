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

    # instantiate a pyDatalog engine
    datalog_engine = pyDatalog.Datalog_engine()
    
    print("Defining a datalog program in %s..." % pyDatalog.Engine)
        
    # test of expressions
    datalog_engine.load("""
        + p(a) # p is a proposition
    """)
    assert datalog_engine.ask('p(a)') == set([('a',)])
    
    datalog_engine.clear()
    assert datalog_engine.ask('p(a)') == None
    
    pyDatalog.assert_fact('p', 'a', 'b')
    assert pyDatalog.ask('p(a, "b")') == set([('a', 'b')])
    
    """unary facts                                                            """
    
    @pyDatalog.program()
    def _(): 
        
        + p(a) 
        # check that unary queries work
        assert ask(p(a)) == set([('a',)])
        assert ask(p(X)) == set([('a',)])
        assert ask(p(Y)) == set([('a',)])
        assert ask(p(b)) == None
        
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
        
        # spaces and uppercase in strings
        + farmer('Moshe dayan')
        + farmer('omar')
        assert ask(farmer(X)) == set([('Moshe dayan',), ('omar',)])

    # execute queries in a python program
    moshe_is_a_farmer = datalog_engine.ask("farmer('Moshe dayan')")
    assert moshe_is_a_farmer == set([('Moshe dayan',)])

    """ secondary facts                                                         """
    
    @pyDatalog.program()
    def _(): 
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
        
    @pyDatalog.program()
    def _(): 
    
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
        
    """ recursion                                                         """
    
    @pyDatalog.program()
    def _(): 
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
        #print((ask(s(X))))
        assert ask(s(X)) == set([(1,), ('a',)])
        
        s(X, Y) <= p(X) & (X == Y)
        assert ask(s(a, a)) == set([('a', 'a')])
        assert ask(s(a, b)) == None
        assert ask(s(X,a)) == set([('a', 'a')])
        #print((ask(s(X, Y))))
        assert ask(s(X, Y)) == set([('a', 'a'),('c', 'c'),(1, 1)])
        # TODO  make this work
        # s <= (X == Y)   
        # assert ask(s(X,Y)) == set([('a', 'a'),('c', 'c'),('1', '1')])

    assert pyDatalog.ask('p(a)') == set([('a',)])
    
    """ recursion with expressions                                         """
    # reset the engine
    datalog_engine = pyDatalog.Datalog_engine()
    @pyDatalog.program(datalog_engine)
    def _(): 
        
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
    datalog_engine = pyDatalog.Datalog_engine()
    @pyDatalog.program(datalog_engine)
    def _(): 
        factorial(N, F) <= (N < 2) & (F==1)
        factorial(N, F) <= (N > 1) & (N1 == N-1) & factorial(N1, F1) & (F == N*F1)
        assert ask(factorial(1, F)) == set([(1, 1)])
        assert ask(factorial(4, F)) == set([(4, 24)])
    
    # Fibonacci
    datalog_engine = pyDatalog.Datalog_engine()
    @pyDatalog.program(datalog_engine)
    def _(): 
        fibonacci(N, F) <= (N == 0) & (F==0)
        fibonacci(N, F) <= (N == 1) & (F==1)
        fibonacci(N, F) <= (N > 1) & (N1 == N-1) & (N2 == N-2) & fibonacci(N1, X1) & fibonacci(N2, X2)  & (F == X1+X2)
        assert ask(fibonacci(1, F)) == set([(1, 1)])
        assert ask(fibonacci(4, F)) == set([(4, 3)])
        assert ask(fibonacci(18, F)) == set([(18, 2584)])

    # string manipulation
    @pyDatalog.program(datalog_engine)
    def _(): 
        split(X, Y,Z) <= (X == Y+'-'+Z)
        assert ask(split(X, 'a', 'b')) == set([('a-b', 'a', 'b')])
        split(X, Y,Z) <= (Y == (lambda X: X.split('-')[0])) & (Z == (lambda X: X.split('-')[1]))
        assert ask(split('a-b', Y, Z)) == set([('a-b', 'a', 'b')])
        assert ask(split(X, 'a', 'b')) == set([('a-b', 'a', 'b')])

    """ negation                                                     """    
    
    datalog_engine = pyDatalog.Datalog_engine()
    datalog_engine.load("""
        + even(0)
        even(N) <= (N > 0) & (N1==N-1) & odd(N1)
        odd(N) <= (N2==N+2) & ~ even(N) & (N2>0)
    """)
    assert datalog_engine.ask('~ odd(7)', _fast=True) == None
    assert datalog_engine.ask('~ odd(2)', _fast=True) == set([(2,)])
    assert datalog_engine.ask('odd(3)', _fast=True) == set([(3,)])
    assert datalog_engine.ask('odd(3)'             ) == set([(3,)])
    assert datalog_engine.ask('odd(5)', _fast=True) == set([(5,)])
    assert datalog_engine.ask('odd(5)'            ) == set([(5,)])
    assert datalog_engine.ask('even(5)', _fast=True) == None
    assert datalog_engine.ask('even(5)'            ) == None
    
    """ functions                                                         """
    pyDatalog.clear()
    @pyDatalog.program()
    def _(): 
        + (f[a]==b)
        assert ask(f[a]==b) == set([('a', 'b')])
    
        + (f[a]==c)
        assert ask(f[a]==X) == set([('a', 'c')])

        + (f2[a,x]==b)
        assert ask(f2[a,x]==b) == set([('a', 'x', 'b')])
    
        + (f2[a,x]==c)
        assert ask(f2[a,x]==X) == set([('a', 'x', 'c')])
        
    """ aggregates                                                         """
    pyDatalog.clear()
    @pyDatalog.program()
    def aggregate(): 
        + p(a, c, 1)
        + p(b, b, 4)
        + p(a, b, 1)

        # sum
        assert(sum(1,2)) == 3
        (a_sum[X] == sum(Y, key=Z)) <= p(X, Z, Y)
        assert ask(a_sum[a]==X) == set([('a', 2)])
        assert ask(a_sum[a]==2) == set([('a', 2)])
        assert ask(a_sum[X]==4) == set([('b', 4)])
        assert ask(a_sum[c]==X) == None

        (a_sum2[X] == sum(Y, key=X)) <= p(X, Z, Y)
        assert ask(a_sum2[a]==X) == set([('a', 1)])

        (a_sum3[X] == sum(Y, key=(X,Z))) <= p(X, Z, Y)
        assert ask(a_sum3[a]==X) == set([('a', 2)])

        # len
        assert(len((1,2))) == 2
        (a_len[X] == len(Z)) <= p(X, Z, Y)
        assert ask(a_len[a]==X) == set([('a', 2)])
        
        (a_lenY[X] == len(Y)) <= p(X, Z, Y)
        assert ask(a_lenY[a]==X) == set([('a', 1)])
        assert ask(a_lenY[c]==X) == None
        
        (a_len2[X,Y] == len(Z)) <= p(X, Y, Z)
        assert ask(a_len2[a,b]==X) == set([('a', 'b', 1)])
        assert ask(a_len2[a,X]==Y) == set([('a', 'b', 1), ('a', 'c', 1)])

        + q(a, c, '1')
        + q(a, b, '2')
        + q(b, b, '4')

        # concat
        (a_concat[X] == concat(Y, key=Z, sep='+')) <= q(X, Y, Z)
        assert ask(a_concat[a]==X) == set([('a', 'c+b')])

        (a_concat2[X] == concat(Y, key=(Z,), sep='+')) <= q(X, Y, Z)
        assert ask(a_concat2[a]==X) == set([('a', 'c+b')])

        (a_concat3[X] == concat(Y, key=(-Z,), sep='-')) <= q(X, Y, Z)
        assert ask(a_concat3[a]==X) == set([('a', 'b-c')])

        #min
        assert min(1,2) == 1
        (a_min[X] == min(Y, key=Z)) <= q(X, Y, Z)
        assert ask(a_min[a]==X) == set([('a', 'c')])
        
        (a_minD[X] == min(Y, key=-Z)) <= q(X, Y, Z)
        assert ask(a_minD[a]==X) == set([('a', 'b')])
        
        (a_min2[X, Y] == min(Z, key=(X,Y))) <= q(X, Y, Z)
        assert ask(a_min2[Y, b]==X) == set([('a', 'b', '2'),('b', 'b', '4')])
        
        (a_min3[X, Y] == min(Z, key=(X,Y))) <= q(X, Y, Z)
        assert ask(a_min3[Y, Y]==X) == set([('b', 'b', '4')])
        
        #max
        assert max(1,2) == 2
        (a_max[X] == max(Y, key=-Z)) <= q(X, Y, Z)
        assert ask(a_max[a]==X) == set([('a', 'c')])
        
        (a_maxD[X] == max(Y, key=Z)) <= q(X, Y, Z)
        assert ask(a_maxD[a]==X) == set([('a', 'b')])

        # rank
        #(a_rank[X, Y] == rank(group_by=X, key=-Z)) <= q(X, Y, Z)
        #assert ask(rank[a,b]==Y) == set([('a', 'b', 1)])

    """ can't call a pyDatalog program                             """
    
    error = False
    try:
        _()
    except: error = True
    assert error

    """ literal cannot have a literal as argument                      """

    @pyDatalog.program(datalog_engine)
    def _():

        _error = False
        try:
            + farmer(farmer(moshe))
        except: _error = True
        assert _error
        
    print("Done.")

if __name__ == "__main__":
    pyDatalog.default_datalog_engine = pyDatalog.Datalog_engine()
    test()
    #cProfile.runctx('test()', globals(), locals())