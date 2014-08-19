# coding=UTF-8
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
from decimal import Decimal
import math
import re
import sys
import time
import datetime

import sys
if sys.version < '3':
    import codecs
    def u(x):
        return codecs.unicode_escape_decode(x)[0]
else:
    def u(x):
        return x

from pyDatalog import pyDatalog
def test():

    # test of expressions
    pyDatalog.load("""
        + p(a) # p is a proposition
    """)
    assert pyDatalog.ask('p(a)') == set([()])
    
    @pyDatalog.program()
    def unicode_(): 
        +p(a, u('\xf6'))
        assert ask(p(X,Y)) == set([('a', u('\xf6'))])
        assert ask(p(a, X)) == set([(u('\xf6'),)])
        assert ask(p(a, u('\xf6'))) == set([()])
        -p(a, u('\xf6'))
        assert ask(p(a, u('\xf6'))) == None
        
    """unary facts                                                            """
    
    @pyDatalog.program()
    def unary(): 
        +z()
        assert ask(z()) == set([()])
        
        + p(a) 
        # check that unary queries work
        assert ask(p(a)) == set([()])
        assert ask(p(X)) == set([('a',)])
        assert ask(p(Y)) == set([('a',)])
        assert ask(p(_X)) == set([('a',)])
        assert ask(p(b)) == None
        assert ask(p(a) & p(b)) == None
        assert ask(~p(X)) == None
        
        + p(b)
        assert ask(p(X)) == set([('a',), ('b',)])
        
        + p(b) # facts are unique
        assert ask(p(X)) == set([('a',), ('b',)])
        
        - p(b) # retract a unary fact
        assert ask(p(X)) == set([('a',)])
        
        - p(a)
        assert ask(p(X)) == None
        assert ask(~p(X)) == set([()])
        + p(a)
        
        # strings, integer, float, datetime
        + p('c')
        assert ask(p(c)) == set([()])
        
        + p(1)
        assert ask(p(1)) == set([()])
        + p(2.0)
        assert ask(p(2.0)) == set([()])
        - p(2.0)
        + p(datetime.date.today())
        assert (ask(p(datetime.date.today()))) == set([()])
        -p(datetime.date.today())
                
        + n(None)
        assert ask(n(X)) == set([(None,)])
        assert ask(n(None)) == set([()])
        
        # spaces and uppercase in strings
        + farmer('Moshe dayan')
        + farmer('omar')
        assert ask(farmer(X)) == set([('Moshe dayan',), ('omar',)])

    # execute queries in a python program
    moshe_is_a_farmer = pyDatalog.ask("farmer('Moshe dayan')")
    assert moshe_is_a_farmer == set([()])

    """ binary facts                                                         """
    
    @pyDatalog.program()
    def binary(): 
        + q(a, b)
        assert ask(q(a, b)) == set([()])
        assert ask(q(X, b)) == set([('a',)])
        assert ask(q(a, Y)) == set([('b',)])
        assert ask(q(a, c)) == None
        assert ask(q(X, Y)) == set([('a', 'b')])
        
        + q(a,c)
        assert ask(q(a, Y)) == set([('b',), ('c',)])
        
        - q(a,c)
        assert ask(q(a, Y)) == set([('b',)])
        
        assert ask(q(X, X)) == None 
        +q(a, a)
        assert ask(q(X, X)) == set([('a',)])
        -q(a, a) 

        +tuple12( (1,(2,(3,))))
        +tuple12( (1,(2,(4,))))
        assert ask(tuple12((X,(2,(3,))))) == set([(1,)])
        assert ask(tuple12((1,(2,(X,))))) == set([(3,), (4,)])
        
        +q(b=b, a=a)
        assert ask(q(a=a, b=b)) == set([()])
        assert ask(q(b=b, a=a)) == set([()])
        assert ask(q_a_b(a,b)) == set([()])
        pyDatalog.retract_fact('q_a_b', 'a', 'b')
        assert (ask(q(b=b, a=a))) == None
        pyDatalog.assert_fact('q_a_b', 'c', 'd')
        assert ask(q(b=d, a=c)) == set([()])
            
        
    """ (in)equality                                             """

    @pyDatalog.program()
    def equality():
        assert ask(X==1) == set([(1,)]) 
        assert ask(X==Y) == None
        assert ask(X==Y+1) == None
        assert ask(X==True) == set([(True,)])
        assert ask(X==1) == set([(1,)]) 
        assert ask(X==False) == set([(False,)])
        assert ask(X==0) == set([(0,)]) 
        assert ask(X==False) == set([(False,)])
        assert ask(X==None) == set([(None,)])
        assert ask(X==1.0) == set([(1.0,)])
        assert ask(X=='1.0') == set([('1.0',)])
        assert pyDatalog.ask('X==True') == set([(True,)])
        assert pyDatalog.ask('X==False') == set([(False,)])
        assert pyDatalog.ask('X==None') == set([(None,)])
        assert ask((X==1) & (Y==1) & (X==Y)) == set([(1,1)])
        assert ask((X==1) & (Y==2) & (X==Y-1)) == set([(1,2)])
        assert ask((X==1) & (Y==2) & (X+2==Y+1)) == set([(1,2)])
        assert ask((X==2) & (Y==X/2)) == set([(2,1)])
        assert ask((X==2) & (Y==X//2)) == set([(2,1)])
        assert ask((X==2) & (Y==X**2)) == set([(2,4)])
        
        assert ask((X==1) & (Y==1+X)) == set([(1,2)])
        assert ask((X==1) & (Y==1-X)) == set([(1,0)])
        assert ask((X==1) & (Y==2*X)) == set([(1,2)])
        assert ask((X==2) & (Y==2/X)) == set([(2,1)])
        assert ask((X==2) & (Y==2//X)) == set([(2,1)])
        assert ask((X==2) & (Y==3**X)) == set([(2,9)])
        assert ask((X==1) & (Y==(1, X+1))) == set([(1, (1, 2))])
        
        assert ask((Y==5) & (X==format_('5 is {}', Y))) == set([(5, '5 is 5')])
        assert ask(X==format_('5 is {} {}', 5, 'test'))  == set([('5 is 5 test',)])
        

    @pyDatalog.program()
    def slices(): 
        assert ((X==(1,2)) & (Y==X[1:2])) == [((1, 2), (2,))]
        assert ((X==(1,2)) & (Y==X[1])) == [((1, 2), 2)]
        assert ((X==(1,2)) & (Y==X[-1])) == [((1, 2), 2)]
        assert ((X==(1,2)) & (X1==1) & (Y==X[X1])) == [((1, 2), 1, 2)]
        assert ((X==(1,2)) & (X1==1) & (Y==X[X1:])) == [((1, 2), 1, (2,))]
        assert ((X==(1,2)) & (X1==1) & (Y==X[X1:X1+1])) == [((1, 2), 1, (2,))]

    
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
        assert ask((X==1) & (Y==X+(2*(X+1)))) == set([(1,5)])
        assert ask((X==1) & (Y==X+1) & (X<Y)) == set([(1,2)])
        assert ask((X==1) & (X<1)) == None
        assert ask((X==1) & (X<=1)) == set([(1,)])
        assert ask((X==1) & (X>1)) == None
        assert ask((X==1) & (X>=1)) == set([(1,)])
        assert ask(X==(1,2)) == set([((1,2),)]) 
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
        assert ask(s(a, a)) == set([()])
        assert ask(s(a, b)) == None
        assert ask(s(X,a)) == set([('a',)])
        assert ask(s(X, Y)) == set([('a', 'a'),('c', 'c'),(1, 1)])
        
        past(D) <= (D < datetime.date.today())
        assert ask(past(datetime.date.today())) == None
        assert ask(past(datetime.date.today()-datetime.timedelta(1))) == set([()])

    assert pyDatalog.ask('p(a)') == set([()])

    """ clauses                                                         """
    
    @pyDatalog.program()
    def clauses(): 
    
        p2(X) <= p(X)
        assert ask(p2(a)) == set([()])
        p2(X) <= p(X)
        
        r(X, Y) <= p(X) & p(Y)
        assert ask(r(a, a)) == set([()])
        assert ask(r(a, c)) == set([()])
        r(X, b) <= p(X)
        assert ask(r(a, b)) == set([()])
        #TODO r(Y, b) <= p(Y)
        
        - (r(X, b) <= p(X))
        assert ask(r(a, b)) == None
        
        # TODO more tests

        # integer variable
        for i in range(10):
            + successor(i+1, i)
        assert ask(successor(2, 1)) == set([()])
        
        # built-in
        assert abs(-3)==3
        assert math.sin(3)==math.sin(3)
        
        # unsafe literals
        lower(N,F) <= (N<F)
        assert ask(lower(2,3)) == set([()])
        assert ask(lower(3,3)) == None     
        
        unsafe(X, Y) <= lower(2,3) & (Y==1)
        assert ask(unsafe(3, 1)) == set([()]) #this query is safe
        assert ask(unsafe(3, Y)) == set([(1,)]) #this query is safe
        assert ask(unsafe(X, 1)) == True # X is undefined
        
        unsafe2(X) <= lower(3,2)
        assert ask(unsafe2(3)) == None
        assert ask(unsafe2(X)) == None
        
    
    """ in                                                         """
    
    pyDatalog.assert_fact('is_list', (1,2))

    @pyDatalog.program()
    def _in(): 
        assert ((X==1) & (X in (1,2))) == [(1,)]
        _in(X) <= (X in [1,2])
        assert ask(_in(1)) == set([()])
        assert ask(_in(9)) == None
        assert ask(_in(X)) == set([(1,), (2,)])
        
        _in2(X) <= is_list(Y) & (X in Y)
        assert ask(_in2(X)) == set([(1,), (2,)])

        assert ask((Y==(1,2)) & (X==1) & (X in Y)) == set([((1, 2), 1)])
        assert ask((Y==(1,2)) & (X==1) & (X in Y+(3,))) == set([((1, 2), 1)])
        assert ask(X == range_(2)) == set([((0,1),)])
                
    """ recursion                                                         """
    
    @pyDatalog.program()
    def recursion(): 
        + even(0)
        even(N) <= successor(N, N1) & odd(N1)
        odd(N) <= ~ even(N)
        assert ask(even(0)) == set([()])
        assert ask(even(X)) == set([(4,), (10,), (6,), (0,), (2,), (8,)])
        assert ask(even(10)) == set([()])
        assert ask(odd(1)) == set([()])
        assert ask(odd(5)) == set([()])
        assert ask(even(5)) == None
    
    """ recursion with expressions                                         """
    # reset the engine
    pyDatalog.clear()
    @pyDatalog.program()
    def recursive_expression(): 
        
        predecessor(X,Y) <= (X==Y-1)
        assert ask(predecessor(X,11)) == set([(10,)])
        
        p(X,Z) <= (Y==Z-1) & (X==Y-1)
        assert ask(p(X,11)) == set([(9,)])
        
        # odd and even
        + even(0)
        even(N) <= (N > 0) & odd(N-1)
        assert ask(even(0)) == set([()])
        odd(N) <= (N > 0) & ~ even(N)
        assert ask(even(0)) == set([()])
        assert ask(odd(1)) == set([()])
        assert ask(odd(5)) == set([()])
        assert ask(even(5)) == None
        assert ask((X==3) & odd(X+2)) == set([(3,)])
        
    # Factorial
    pyDatalog.clear()
    @pyDatalog.program()
    def factorial(): 
        factorial[N] = N*factorial[N-1] # most general clause first
        factorial[1] = 1
        (factorial[N] == F) <= (N < 1) & (F== -factorial[-N])
        assert ask(factorial[1] == F) == set([(1,)])
        assert ask(factorial[4] == F) == set([(24,)])
        assert ask(factorial[-4] == F) == set([(-24,)])
    
    # Fibonacci
    pyDatalog.clear()
    @pyDatalog.program()
    def fibonacci(): 
        (fibonacci[N] == F) <= (N == 0) & (F==0)
        (fibonacci[N] == F) <= (N == 1) & (F==1)
        (fibonacci[N] == F) <= (N > 1) & (F == fibonacci[N-1]+fibonacci[N-2])
        assert ask(fibonacci[1] == F) == set([(1,)])
        assert ask(fibonacci[4] == F) == set([(3,)])
        assert ask(fibonacci[18] == F) == set([(2584,)])

    # string manipulation
    @pyDatalog.program()
    def _lambda(): 
        assert ask((X=='world') & (Y==(lambda X: 'hello ' + X))) == set([('world', 'hello world')])
        split(X, Y,Z) <= (X == Y+'-'+Z)
        assert ask(split(X, 'a', 'b')) == set([('a-b',)])
        split(X, Y,Z) <= (Y == (lambda X: X.split('-')[0])) & (Z == (lambda X: X.split('-')[1]))
        assert ask(split('a-b', Y, Z)) == set([('a', 'b',)])
        assert ask(split(X, 'a', 'b')) == set([('a-b',)])
        
        (two[X]==Z) <= (Z==X+(lambda X: X))
        assert ask(two['A']==Y) == set([('AA',)])

    """ negation                                                     """    
    
    @pyDatalog.program()
    def _negation():
        +p(a, b)
        assert ask(~p(X, b)) == None
        assert ask(~p(X, c)) == set([()])
        assert(ask(~(p(X,c) & p(X,a)))) == set([()])
        assert(ask(p(X,b) & ~(p(X,c) & p(X,a)))) == set([('a',)])
        assert(ask(~(p(X,c) & p(X,a)) & p(X,b))) == set([('a',)])
        assert(ask(~(p(X,b) & p(X,a) & p(X,b)))) == set([()])

    pyDatalog.load("""
        + even(0)
        even(N) <= (N > 0) & (N1==N-1) & odd(N1)
        odd(N) <= (N2==N+2) & ~ even(N) & (N2>0)
    """)
    assert pyDatalog.ask('~ odd(7)') == None
    assert pyDatalog.ask('~ odd(2)') == set([()])
    assert pyDatalog.ask('odd(3)') == set([()])
    assert pyDatalog.ask('odd(3)'             ) == set([()])
    assert pyDatalog.ask('odd(5)') == set([()])
    assert pyDatalog.ask('odd(5)'            ) == set([()])
    assert pyDatalog.ask('even(5)') == None
    assert pyDatalog.ask('even(5)'            ) == None
    
    """ functions                                                         """
    pyDatalog.clear()
    @pyDatalog.program()
    def function(): 
        + (f[a]==b)
        assert ask(f[X]==Y) == set([('a', 'b')])
        assert ask(f[X]==b) == set([('a',)])
        assert ask(f[a]==X) == set([('b',)])
        assert ask(f[a]==b) == set([()])

        assert ask(Y==f[X]) == set([('a', 'b')])
        assert ask(b==f[X]) == set([('a',)])
        assert ask(f[X]==f[X]) == set([('a',)])
    
        f[a]=c
        assert ask(f[a]==X) == set([('c',)])
        
        + (f[a]==a)
        assert ask(f[X]==Y) == set([('a', 'a')])
        assert ask(f[f[a]]==X) == set([('a',)])
        assert ask(f[X]==f[a]) == set([('a',)])
        assert ask(f[X]==f[a]+'') == set([('a',)])
        + (f[c]==c)
        assert ask(f[f[a]]==X) == set([('a',)])

        (p[X]==a) <= (f[X]==a)
        (p[X]==c) <= (f[X]==c)
        
        assert ask(p[X]==Y) == set([('a', 'a'), ('c', 'c')])

        del f[c]
        - (f[a]==a)
        assert ask(f[f[a]]==X) == None

        + (f[a]==None)
        assert (ask(f[a]==X)) == set([(None,)])
        + (f[a]==(1,2))
        assert (ask(f[a]==X)) == set([((1,2),)])
        assert (ask(f[X]==(1,2))) == set([('a',)])

        + (f[a]==c)

        + (f2[a,x]==b)
        assert ask(f2[a,x]==b) == set([()])
    
        + (f2[a,x]==c)
        assert ask(f2[a,x]==X) == set([('c',)])
        
        g[X] = f[X]+f[X]
        assert(ask(g[a]==X)) == set([('cc',)])
        del g[X]
        assert(ask(g[a]==X)) == None
        
        h(X,Y) <= (f[X]==Y)
        assert (ask(h(X,'c'))) == set([('a',)])
        assert (ask(h(X,Y))) == set([('a', 'c')])
        
        (plus[X,Y]==Z) <= (Z==X+Y)
        assert (ask(plus[1,2]==X)) == set([(3,)])
        assert (ask(plus[plus[1,2]+1, 2+plus[2,3]] == plus[4, plus[2,5]])) == set([()])
        assert (ask(plus[plus[1,2]+1, 2+plus[2,3]] <  plus[5, plus[2,5]])) == set([()])
        assert (ask(plus[plus[1,2]+1, 2+plus[2,3]] <  plus[1, plus[2,5]])) == None

        (discount[Total] == 1)  <=  (10   < Total) # most general clause first
        (discount[Total] == 10)  <= (100  < Total)
        (discount[Total] == 100) <= (1000 < Total)
        assert (ask(discount[2000]==Y) == set([(100,)])) 
        assert (ask(discount[200]==Y) == set([(10,)])) 

        default[X] = 0
        (default[X] == 1) <= (0<X) & (X<10)
        
        assert ask(default[0]==X) == set([(0,)])
        assert ask(default[3]==X) == set([(1,)])
                
    @pyDatalog.program()
    def function_comparison(): 
        assert ask(f[X]==Y) == set([('a', 'c')])
        assert ask(f[a]<'d') == set([()])
        assert ask(f[a]>'a') == set([()])
        assert ask(f[a]>='c') == set([()])
        assert ask(f[a]>'c') == None
        assert ask(f[a]<='c') == set([()])
        assert ask(f[a]>'c') == None
        assert ask(f[a] in ['c',]) == set([()])
        
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

        assert ask(f[X]==f[X]+'') == set([('a',)])

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
        assert ask(a_sum[a]==X) == set([(2,)])
        assert ask(a_sum[a]==2) == set([()])
        assert ask(a_sum[X]==4) == set([('b',)])
        assert ask(a_sum[c]==X) == None
        assert ask((a_sum[X]==2) & (p(X, Z, Y))) == set([('a', 'c', 1), ('a', 'b', 1)])

        (a_sum2[X] == sum(Y, for_each=X)) <= p(X, Z, Y)
        assert ask(a_sum2[a]==X) == set([(1,)])

        (a_sum3[X] == sum(Y, key=(X,Z))) <= p(X, Z, Y)
        assert ask(a_sum3[X]==Y) == set([('a', 2), ('b', 4)])
        assert ask(a_sum3[a]==X) == set([(2,)])

    @pyDatalog.program()
    def len_(): 
        assert(len((1,2))) == 2
        (a_len[X] == len(Z)) <= p(X, Z, Y)
        assert ask(a_len[X]==Y) == set([('a', 2), ('b', 1)])
        assert ask(a_len[a]==X) == set([(2,)])
        assert ask(a_len[X]==1) == set([('b',)])
        assert ask(a_len[X]==5) == None
        
        (a_lenY[X] == len(Y)) <= p(X, Z, Y)
        assert ask(a_lenY[a]==X) == set([(1,)])
        assert ask(a_lenY[c]==X) == None
        
        (a_len2[X,Y] == len(Z)) <= p(X, Y, Z)
        assert ask(a_len2[a,b]==X) == set([(1,)])
        assert ask(a_len2[a,X]==Y) == set([('b', 1), ('c', 1)])
        
        (a_len3[X] == Y) <= (Y==1+len_(X))
        assert ask((X==(1,2)) & (a_len3[X]==3)) == set([((1,2),)])
        assert ask((X==(1,2)) & (a_len3[X]==Y)) == set([((1,2),3)])
        assert ask(a_len3[((1,2),)]==3) == set([()])
        assert (ask(a_len3[X]==Y)) == None

    @pyDatalog.program()
    def tuple_():
        (a_tuple[X] == tuple_(Y, order_by=Y)) <= (Y in range_(X))
        assert ask((a_tuple[2]== Y)) == set([((0,1),)])
    
    @pyDatalog.program()
    def concat(): 
        + q(a, c, 1)
        + q(a, b, 2)
        + q(b, b, 4)

        (a_concat[X] == concat(Y, key=Z, sep='+')) <= q(X, Y, Z)
        assert ask(a_concat[X]==Y) == set([('b', 'b'), ('a', 'c+b')])
        assert ask(a_concat[a]=='c+b') == set([()])
        assert ask(a_concat[a]==X) == set([('c+b',)])
        assert ask(a_concat[X]==b) == set([('b',)])

        (a_concat2[X] == concat(Y, order_by=(Z,), sep='+')) <= q(X, Y, Z)
        assert ask(a_concat2[a]==X) == set([('c+b',)])

        (a_concat3[X] == concat(Y, key=(-Z,), sep='-')) <= q(X, Y, Z)
        assert ask(a_concat3[a]==X) == set([('b-c',)])

    @pyDatalog.program()
    def min(): 
        assert min(1,2) == 1
        (a_min[X] == min(Y, key=Z)) <= q(X, Y, Z)
        assert ask(a_min[X]==Y) == set([('b', 'b'), ('a', 'c')])
        assert ask(a_min[a]=='c') == set([()])
        assert ask(a_min[a]==X) == set([('c',)])
        assert ask(a_min[X]=='b') == set([('b',)])
        
        (a_minD[X] == min(Y, order_by=-Z)) <= q(X, Y, Z)
        assert ask(a_minD[a]==X) == set([('b',)])
        
        (a_min2[X, Y] == min(Z, key=(X,Y))) <= q(X, Y, Z)
        assert ask(a_min2[Y, b]==X) == set([('a', 2),('b', 4)])
        assert ask(a_min2[Y, Y]==X) == set([('b', 4)]), "a_min2"
        
        (a_min3[Y] == min(Z, key=(-X,Z))) <= q(X, Y, Z)
        assert ask(a_min3[b]==Y) == set([(4,)]), "a_min3"
        
        (a_min1[1] == min(X, order_by=(Z))) <= q(X, Y, Z)
        assert ask(a_min1[1]==Y) == set([('a',)])
        
    @pyDatalog.program()
    def max(): 
        assert max(1,2) == 2
        (a_max[X] == max(Y, key=-Z)) <= q(X, Y, Z)
        assert ask(a_max[a]==X) == set([('c',)])
        
        (a_maxD[X] == max(Y, order_by=Z)) <= q(X, Y, Z)
        assert ask(a_maxD[a]==X) == set([('b',)])
        
        #issue#4
        (prev_zero[X, Y] == max_(Z, order_by=Z)) <= Z.in_(range_(len(X))) & (Z < Y) & (X[Z]==0)
        (next_one[X, Y] == min_(Z, order_by=Z)) <= Z.in_(range_(len(X))) & (Z > Y) & (X[Z]==1)
        assert ask(next_one[(0,0,1,1,1), 0] == X) == set([(2,)])

    @pyDatalog.program()
    def rank(): 
        + score('Superman', 10)
        + score('Tom', 3)
        + score('Jerry', 8)
        
        (place[Person]==rank(order_by=Score)) <= score(Person, Score)
        assert ask(place[Person]==Rank) == set([('Superman', 2),('Tom', 0),('Jerry', 1)])
        assert ask(place['Jerry']==Rank) == set([(1,)])
        assert ask(place[Person]==1) == set([('Jerry',)])
        
        # second definition of aggregrate
        + other_score('Phil', 5)
        (place[Person]==rank(order_by=Score)) <= other_score(Person, Score)
        assert ask(place[Person]==Rank) == set([('Superman', 2),('Tom', 0),('Jerry', 1),('Phil', 0)])
        assert ask(place['Phil']==Rank) == set([(0,)])
        
        # reverse ranking
        (reverse_place[Person]==rank(order_by=-Score)) <= score(Person, Score)
        assert ask(reverse_place[Person]==Rank) == set([('Superman', 0),('Tom', 2),('Jerry', 1)])
        assert ask(reverse_place['Superman']==Rank) == set([(0,)])
        assert ask(reverse_place[Person]==2) == set([('Tom',)])

        # multiple keys, group_by
        (multi_place[Person, Score]==rank(group_by=Person, order_by=Score)) <= score(Person, Score)
        assert ask(multi_place[Person, Score]==Rank) == set([('Superman', 10, 0),('Tom', 3,0),('Jerry', 8,0)])
        assert ask(multi_place['Superman', Score]==Rank) == set([(10,0,)])
        assert ask(multi_place[Person, 3]==0) == set([('Tom',)])

        (a_rank1[Z] == rank(group_by=Z, order_by=Z)) <= q(X, Y, Z)
        assert ask(a_rank1[X]==Y) == set([(1, 0), (2, 0), (4, 0)])
        assert ask(a_rank1[X]==0) == set([(1,), (2,), (4,)])
        assert ask(a_rank1[1]==X) == set([(0,)])
        assert ask(a_rank1[1]==0) == set([()])
        assert ask(a_rank1[1]==1) == None

        # rank
        (a_rank[X,Y] == rank(group_by=(X,Y), order_by=Z)) <= q(X, Y, Z) & q(X,Y2,Z2)
        assert ask(a_rank[X,Y]==Z) == set([('a', 'b', 0), ('a', 'c', 0), ('b', 'b', 0)])
        assert ask(a_rank[a,b]==1) == None
        assert ask(a_rank[a,b]==Y) == set([(0,)])
        assert ask(a_rank[a,X]==0) == set([('b',), ('c',)])
        assert ask(a_rank[a,X]==Y) == set([('b', 0), ('c', 0)])
        assert ask(a_rank[X,Y]==1) == None
        
    @pyDatalog.program()
    def running_sum(): 
        +movement('Account1', 2014,1,1, 10)
        +movement('Account1', 2014,1,2, -6)
        +movement('Account1', 2014,1,2, -2) #TODO ?
        +movement('Account1', 2014,1,3, -2)
        +movement('Account2', 2014,1,1, 10)
        +movement('Account2', 2014,1,2, -5)
        
        (balance[Account, Y,M,D] == running_sum(Amount, group_by=Account, order_by=(Y,M,D))) <= movement(Account, Y,M,D, Amount)
        assert ask(balance[Account, Y,M,D]==Amount) == set([('Account1', 2014,1,1, 10),('Account1', 2014,1,2, 2),('Account1', 2014,1,3, 0),
                                                           ('Account2', 2014,1,1, 10),('Account2', 2014,1,2, 5)])
        assert ask(balance['Account1', Y,M,D]==Amount) == set([(2014,1,1, 10), (2014,1,2, 2), (2014,1,3, 0)])
        assert ask(balance[Account, 2014,1,2]==Amount) == set([('Account1', 2), ('Account2', 5)])
        assert ask(balance[Account, Y,M,D]==0) == set([('Account1', 2014,1,3)])

        # end of month
        (balance[Account, Y,M] == running_sum(Amount, group_by=Account, order_by=(Y,M,D))) <= movement(Account, Y,M,D, Amount)
        assert ask(balance[Account, Y,M]==Amount) == set([('Account1', 2014,1, 0), ('Account2', 2014,1,5)])
        assert ask(balance['Account1', Y,M]==Amount) == set([(2014,1, 0)])
        assert ask(balance[Account, 2014,1]==Amount) == set([('Account1',0), ('Account2', 5)])
        assert ask(balance[Account, Y,M]==0) == set([('Account1', 2014,1)])


    """ simple in-line queries                                        """
    X = pyDatalog.Variable()
    assert ((X==1) >= X) == 1
    assert ((X==1) & (X!=2) >= X) == 1
    assert set(X.in_((1,2))) == set([(1,),(2,)])
    assert ((X==1) & (X.in_ ((1,2)))) == [(1,)]
    
    """ error handling                                                """
    # issue #6
    Y = pyDatalog.Variable()
    try:
        assert (((X==0) & (Y==1/X)) >= X )== 0
    except Exception as e:
        assert False

    """ interface with python classes                                        """

    class A(pyDatalog.Mixin):
        def __init__(self, b):
            super(A, self).__init__()
            self.b = b
        def ok(self, x):
            return "ok"[x]
        def __repr__(self):
            return self.b
        @pyDatalog.program() # indicates that the following method contains pyDatalog clauses
        def _():
            (A.c[X]==N) <= (A.b[X]==N)
            A.d(X, Y) <= (A.c[X]==Y)
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
    assert (pyDatalog.ask("A.c[X]=='a'")) == {(a,)}
    assert (A.c[X]=='a')[0] == (a,)
    assert list(X.data) == [a]
    assert X.v() == a
    assert ((A.c[a]==X) >= X) == 'a'
    assert ((A.c[a]==X) & (A.c[a]==X) >= X) == 'a'
    assert ((A.c[a]==X) & (A.c[b]==X) >= X) == None
    (A.c[X]=='b') & (A.b[X]=='a')
    assert list(X.data) == []
    (A.c[X]=='a') & (A.b[X]=='a')
    assert list(X.data) == [a]
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

    assert (A.c[X].in_(('a',))) == [(a,)]
    assert (A.c[X].in_(('a',)+('z',))) == [(a,)]
    assert ((Y==('a',)) & (A.c[X].in_(Y))) == [(('a',), a)] # TODO make ' in ' work
    assert ((Y==('b',)) & (A.c[X].in_(Y))) == [(('b',), b)]
    
    assert ((Y==('a',)) & (A.c[X].in_(Y+('z',)))) == [(('a',), a)] # TODO make ' in ' work
    assert (A.c[X].in_(('z',))) == []

    # more complex queries
    assert ((Y=='a') & (A.b[X]!=Y)) == [('a', b)] # order of appearance of the variables !
    
    assert (A.len[X]==Y) == [(b, 1), (a, 1)]
    assert (A.len[a]==Y) == [(1,)]

    # A.ok is a regular class method
    assert A.ok(a, 1) == 'k'
    assert ((X==a) & (X.ok(1)==Y)) == [(a, 'k')]
    assert ((X==a) & (X.ok(1)[0]==Y)) == [(a, 'k')]
    assert pyDatalog.ask("(A.b[X]=='a') & (X.ok(1)==Y)") == set([(a, 'k')])   
    assert pyDatalog.ask("(A.b[X]=='a') & (X.ok(1)[0]==Y)") == set([(a, 'k')])   
    
    pyDatalog.create_atoms('A.ok')
    assert A.ok(a, 1) == 'k'
    ok = A.ok
    if sys.version_info[0] == 3: # only for python 3 ! 
        # TODO 2.7 avoid error message : unbound method result() must be called with A instance as first argument
        # workaround : use X.ok(1)
        assert ((X==a) & (A.ok(X,1)==Y)) == [(a, 'k')]
        
        pyDatalog.create_atoms('ok')
        assert ((X==a) & (ok(X,1)==Y)) == [(a, 'k')]
        assert ((X==a) & (ok(X,1)[0]==Y)) == [(a, 'k')]    
    assert ok(a, 1) == 'k'
    
    # A.d is a literal defined by a clause
    assert A.d(a,'a') == [()]
    assert A.d(X,'a') == [(a,)]
    assert (A.d(X,'a') >= X) == a 
    assert (A.d(X,'b') >= X) == b
    assert pyDatalog.ask("(A.b[X]=='a') & A.d(X,Y)") == set([(a, 'a')])   
    assert pyDatalog.ask("A.d(X,Y) & (A.b[X]=='a')") == set([(a, 'a')])   
    
    # A.x is a literal defined by a python resolver
    assert(A.x(X).as_literal) == [] #TODO detect python resolver

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
    assert list(X.data) == [z]
    try:
        a.z == 'z'
    except Exception as e:
        e_message = e.message if hasattr(e, 'message') else e.args[0]
        if e_message != "Predicate without definition (or error in resolver): A.z[1]==/3":
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
    assert ((Z.c[X]>'a') & (A.c[X]=='za')) == [(z,)]
    assert (A.c[X]=='za') == [(z,)]
    assert (A.c[z]=='za') == [()]
    assert (z.b) == 'za'
    assert (z.c) == 'za'
    
    w = Z('w')
    w = Z('w') # duplicated to test __refs__[cls]
    
    assert Z.x(X).as_literal == [(z,)] #TODO detect python resolver
    assert not (~Z.x(z))
    assert ~Z.x(w)
    
    assert ~ (Z.z[w]=='z')
    assert Z.pred(X).as_literal  == [(w,)] # not duplicated !
    assert(Z.pred(X) & ~ (Z.z[X]>='z')) == [(w,)]
    assert(Z.x(X) & ~(Z.pred(X))) == [(z,)]

    assert (Z.len[X]==Y) == [(w, 1), (z, 1)]
    assert (Z.len[z]==Y) == [(1,)]
    
    assert (A.c[X]==Y) & (Z.c[X]==Y) == [(w, 'wa'), (z, 'za')]
    assert (A.b[w]==Y) == [('wa',)]
            
    # direct attribute access X.b[1]==Y
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.b[1]=='a')") == set([(w,)])   
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.b[1]==Y)") == set([(w, 'a')])
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.b[1]<'b')") == set([(w,)])   
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.b[1:2]=='a')") == set([(w,)])   
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.b[1:2]==Y)") == set([(w, 'a')])
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.b[1:2]<'b')") == set([(w,)])   

    assert ((X==w) & (X.b[1]=='a'))  == [(w,)]  
    assert ((X==w) & (X.b[1]==Y)) == [(w, 'a')] 
    assert ((X==w) & (X.b[1]<'b')) == [(w,)]   
    assert ((X==w) & (X.b[1:2]=='a'))  == [(w,)]  
    assert ((X==w) & (X.b[1:2]==Y)) == [(w, 'a')] 
    assert ((X==w) & (X.b[1:2]<'b')) == [(w,)]   
    assert ((X==w) & (Y==X.b)) == [(w, 'wa')]   

    # Z.ok is an inherited class method
    assert Z.ok(z, 1) == 'k'
    assert ((X==w) & (X.ok(1)==Y)) == [(w, 'k')]
    assert ((X==w) & (X.ok(1)[0]==Y)) == [(w, 'k')]
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.ok(1)==Y)") == set([(w, 'k')])   
    assert pyDatalog.ask("(Z.b[X]=='wa') & (X.ok(1)[0]==Y)") == set([(w, 'k')])   
    assert Z.ok(z, 1) <= 'z' # not a clause !
    assert Z.ok(z, 1) <= Z.ok(z, 1)
    assert not (Z.ok(z, 1) > Z.ok(z, 1))

    # Z.d is a literal defined by an inherited clause
    assert Z.d(z,'za') == [()]
    assert Z.d(X,'za') == [(z,)]
    assert (Z.d(X,'za') >= X) == z 
    assert (Z.d(X,'wa') >= X) == w
    assert pyDatalog.ask("(Z.b[X]=='wa') & Z.d(X,Y)") == set([(w, 'wa')])   
    assert pyDatalog.ask("Z.d(X,Y) & (Z.b[X]=='wa')") == set([(w, 'wa')])   

    """ python resolvers                                              """
    
    @pyDatalog.predicate()
    def q2(X,Y):
        yield (1,2)
        yield (2,3)
    
    assert pyDatalog.ask('q(X,Y)') == set([(1, 2), (2, 3)])
    assert pyDatalog.ask('q(1,Y)') == set([(2,)])
    assert pyDatalog.ask('q(1,2)') == set([()])

    # deprecated : function name without the arity
    @pyDatalog.predicate()
    def p(X,Y):
        yield (1,2)
        yield (2,3)
    
    assert pyDatalog.ask('p(X,Y)') == set([(1, 2), (2, 3)])
    assert pyDatalog.ask('p(1,Y)') == set([(2,)])
    assert pyDatalog.ask('p(1,2)') == set([()])
    
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
                print(code + "|" + e_message) 
            _error = True
        assert _error
        
    def assert_ask(code, message='^$'):
        _error = False
        try:
            pyDatalog.ask(code)
        except Exception as e: 
            e_message = e.message if hasattr(e, 'message') else e.args[0]
            if not re.match(message, e_message):
                print(code + "|" + e_message) 
            _error = True
        assert _error

    assert_error('ask(z(a),True)', 'Too many arguments for ask \!')
    assert_error('ask(z(a))', 'Predicate without definition \(or error in resolver\): z/1')
    assert_error("+ farmer(farmer(moshe))", "Syntax error: Literals cannot have a literal as argument : farmer\[\]")
    assert_error("+ manager[Mary]==John", "Did you mean to assert or retract a fact \? Please add parenthesis.")
    assert_error('ask(- manager[X]==1)', "Did you mean to assert or retract a fact \? Please add parenthesis.")
    assert_error("manager[X]==Y <= (X==Y)", "Syntax error: please verify parenthesis around \(in\)equalities")
    assert_error("p(X) <= X==1 & X==2", "Syntax error: please verify parenthesis around \(in\)equalities")
    assert_error("p(X) <= (manager[X]== min(X))", "Error: argument missing in aggregate")
    assert_error("p(X) <= (manager[X]== max(X, order_by=X))", "Aggregation cannot appear in the body of a clause")
    assert_error("q(min(X, order_by=X)) <= p(X)", "Syntax error: Incorrect use of aggregation\.")
    assert_error("manager[X]== min(X, order_by=X) <= manager(X)", "Syntax error: please verify parenthesis around \(in\)equalities")
    assert_error("(manager[X]== min(X, order_by=X+2)) <= manager(X)", "Arguments of aggregate must be variable\(s\).")
    assert_error("ask(X<1)", 'Error: left hand side of comparison must be bound: ')
    assert_error("ask(X<Y)", 'Error: left hand side of comparison must be bound: ')
    assert_error("ask(1<Y)", 'Error: left hand side of comparison must be bound: ')
    assert_error("ask((Z.b[X]=='wa') & (X.b[1]<Y))", 'Error: right hand side of comparison must be bound: ') 
    assert_ask("A.u[X]==Y", "Predicate without definition \(or error in resolver\): A.u\[1\]==/3")
    assert_ask("A.u[X,Y]==Z", "Predicate without definition \(or error in resolver\): A.u\[2\]==/4")
    assert_error('p(X) <= sum(X, key=X)', 'Invalid body for clause')
    assert_error("p(X) <= (X=={})", "") # error message is different in pypy

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
            
            Employee.p(X,Y) <= (Y <= Employee.salary[X] + 1)
            

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
    result = (Employee.salary[X] == 6300) # notice the similarity to a pyDatalog query
    assert result == [(Mary,), ]
    assert (X._value() == [Mary,]) # prints [Employee: Mary]
    assert (X.v() == Mary) # prints Employee:Mary

    result = (Employee.indirect_manager(Mary, X))
    assert (X.v() == John) # prints [Employee: John]
    assert result == [(John,), ]
    
    Mary.salary_class = ((Employee.salary_class[Mary]==X) >= X)
    Mary.salary = 10000
    assert Mary.salary_class != ((Employee.salary_class[Mary]==X) >= X)

    X, Y, N = pyDatalog.variables(3)
    result = (Employee.salary[X]==6800) & (Employee.name[X]==N)
    assert result == [(John,'John'), ]
    assert N.v() == 'John'
    
    result = (Employee.salary[X]==Employee.salary[X])
    assert result == [(John,), (Mary,), (Sam,)]
    
    result = (Employee.p(X,1))
    assert result == [(John,), (Mary,), (Sam,)]
    
    result = (Employee.salary[X]<Employee.salary[X]+1)
    assert result == [(John,), (Mary,), (Sam,)]
    
    result = (Employee.salary[John]==N) & Employee.p(John, N)
    assert result == [(6800,)]
    result = (Employee.salary[X]==6800) & (Employee.salary[X]==N) & Employee.p(X, N) 
    assert result == [(John, 6800)]

    """
    result = Employee.p(John, Employee.salary[John]) # TODO
    print(result)
    result = (Employee.salary[X]==6800) & Employee.p(X, Employee.salary[X]) # TODO
    print (result)
    """
    
if __name__ == "__main__":
    start_time = time.time()
    test()
    #cProfile.runctx('test()', globals(), locals())
    
    """ In-line queries using create_atoms                    """
    
    pyDatalog.create_atoms('p', 'd, Y, eq, Z')
    +p('a')
    +p(u('\xf6'))
    
    p(Y)
    assert (set(Y._value()) == set(['a',u('\xf6')]))
    - p(u('\xf6'))
    assert ((Y==u('\xf6')) >= Y) == u('\xf6')
    
    + d(Decimal(2.0))
    d(Y)
    assert (d(Y) >= Y) == Decimal(2.0)
    
    X = pyDatalog.Variable()
    pyDatalog.create_atoms('X')
    p(X) & (X=='b')
    assert (X._value() == [])
    
    assert ((X=='world') & (Y=='hello ' + X)) == [('world', 'hello world')]
    #TODO print((X=='world') & (Y==(lambda X: 'hello ' + X)))
    
    pyDatalog.create_atoms('p2')
    
    def test2():
        p2(X) <= p(X)
        p2(X)
        assert (X._value() == ['a',])
        assert (p2(X) == [('a',)])
    test2()

    eq(X,Y) <= (X==Z) & (Z==Y)
    
    assert (eq(3, Z)) == [(3,)]
    assert(eq(X, 3)) == [] # because X==Z is undefined
    assert(eq(2, 3)) == []
    assert(eq(3, 3)) == [()]

    # list unification
    pyDatalog.create_atoms('X,Y, X1, X2, X3, p, a, eq, tuple12')
    assert ( X==(1,2) ) == [((1,2),)]
    assert ( X==(1,(2,)) ) == [((1,(2,)),)] # nested
    assert ( X==(1,) + (2,) ) == [((1,2),)] # expression
    assert  ((X==(1,2)) & (Y==X+(3,))) == [((1, 2), (1, 2, 3))]
    
    assert ( (X==(1,2)) & (X==(1, X2))) == [((1, 2), 2)]
    assert ( (X==(1,(2,))) & (X==(1, (X2,))) & (Y==X2)) == [((1, (2,)), 2, 2)]
    assert ( (X==(1,2)) & (X==Y)) == [((1, 2), (1, 2))]
    assert ( (X==(1,2)) & (Y==(1,2)) & (X==Y)) == [((1, 2), (1, 2))]
    assert ( (X==(1,2)) & (Y==(1,3)) & (X==Y)) == []
        
    eq(X,Y) <= (X==Y)
    assert ( eq(X,(1,2))) == [((1,2),)]
    assert ( eq(X,(1,(2,))) ) == [((1,(2,)),)] # nested
    assert ( eq(X,(1,) + (2,)) ) == [((1,2),)] # expression
    
    assert ( eq(X,(1,2)) & (eq(X,(1, X2)))) == [((1, 2), 2)]
    assert ( eq(X,(1,(2,))) & (X==(1, (X2,))) & (Y==X2)) == [((1, (2,)), 2, 2)]
    assert ( eq(X,(1,2)) & eq(X,Y)) == [((1, 2), (1, 2))]
    assert ( eq(X,(1,2)) & (Y==(1,2)) & (X==Y)) == [((1, 2), (1, 2))]
    assert ( eq(X,(1,2)) & (Y==(1,3)) & (X==Y)) == []
    
    + (p[a] == (1,2))
    assert ( p[X]==(1,2) ) == [('a',)]
    assert ( (p[X]==(1,2)) & (p[X]==(1, X2))) == [('a', 2)]
    assert ( (p[X]==(1,2)) & (p[X]==Y)) == [('a', (1, 2))]
    assert ( (p[X]==(1,2)) & (Y==(1,2)) & (p[X]==Y)) == [('a', (1, 2))]
    assert ( (p[X]==(1,2)) & (Y==(1,3)) & (p[X]==Y)) == []
    + (p[a] == (1,(2,)))
    assert ( (p[X]==(1,(2,))) & (p[X]==(1, (X2,))) & (Y==X2)) == [('a', 2, 2)]

    # slices
    assert ((X==(1,2)) & (Y==X[1:2])) == [((1, 2), (2,))]
    assert ((X==(1,2)) & (Y==X[1])) == [((1, 2), 2)]
    assert ((X==(1,2)) & (Y==X[-1])) == [((1, 2), 2)]
    assert ((X==(1,2)) & (X1==1) & (Y==X[X1])) == [((1, 2), 1, 2)]
    assert ((X==(1,2)) & (X1==1) & (Y==X[X1:])) == [((1, 2), 1, (2,))]
    assert ((X==(1,2)) & (X1==1) & (Y==X[X1:X1+1])) == [((1, 2), 1, (2,))]

    tuple12(X) <= (X==(1,2))
    assert tuple12( (X1,X2) ) == set([(1, 2)])
    +tuple12( (1,(2,(3,))))
    +tuple12( (1,(2,(4,))))
    assert tuple12((1,(2,(X,)))) == set([(3,), (4,)])

    # unsafe literals
    pyDatalog.create_atoms('lower, unsafe, unsafe2, unsafe3')
    lower(X,Y) <= (X<Y)
    assert lower(2,3) == [()]
    assert lower(3,3) == []     

    unsafe(X, Y) <= lower(2,3) & (Y==1)
    assert unsafe(3, 1) == [()] #this query is safe
    assert unsafe(3, Y) == [(1,)] #this query is safe
    assert unsafe(X, 1) == True # X is undefined
    assert (unsafe(X,1) >= X) == True
    assert (unsafe(2,Y) >= Y) == 1
    assert (unsafe(X,Y) >= Y) == True
    
    unsafe2(X) <= lower(3,2)
    assert unsafe2(3) == []
    assert unsafe2(X) == []
    
    unsafe3(X) <= ~(X==1)
    assert unsafe3(1) == []
    assert unsafe3(2) == [()]
    assert unsafe3(X) == []

    """ built-ins and import                    """
    
    # len
    
    pyDatalog.create_atoms('len')
    assert (len('ok')) == 2
    assert ((X==(1,2)) & (Y==len(X)) >= Y)==2
    
    # str.lower
    
    pyDatalog.create_atoms('str.lower')
    assert (str.lower('OK')) == 'ok'
    assert ((X=='OK') & (Y==str.lower(X)) >= Y) == 'ok'
    
    # str
    
    pyDatalog.create_atoms('str')
    assert ( ((X==8) & (Y==str(X))) >= Y) == "8"
    assert ( ((X=='uPeP') & (Y==X.lower())) >= Y) == 'upep'
    assert ( ((X==8) & (Y==str(X).lower())) >= Y) == "8"
    
    assert (str.lower('OK')) == 'ok'
    assert ((X=='OK') & (Y==str.lower(X)) >= Y) == 'ok'
    assert ((X==('1','2')) & (Y==str.join('.', X)) >= Y) == '1.2'
    assert ((X=='ok') & (Y==str.format('Hello {0}', X)) >= Y) == 'Hello ok'
    assert (((X=='o.k') & ((Y,Z)==str.split(X, '.'))) >= Y) == 'o'
    #not supported : print((X==('1','2')) & (Y=='.'.join(X)))
    
    # time
    
    import time
    pyDatalog.create_atoms('time')
    assert 0 < (time.time())
    
    # math
    
    import math
    pyDatalog.create_atoms('math')
    assert (math.cos(0)) == 1
    assert ((X==0) & (Y==math.cos(X)) >= Y) == 1
    
    from math import cos
    pyDatalog.create_atoms('cos')
    assert (cos(0)) == 1
    assert ((X==0) & (Y==cos(X)) >= Y) == 1
    
    # string
    
    import string
    #TODO 1.9, 3.2 : pyDatalog.create_atoms('string')
    assert (string.digits) == '0123456789'

    _error = False
    try:
        pyDatalog.create_atoms('xoiker.test')
    except Exception as e:
        if e.value != "Unknown variable : xoiker":
            print(e.value) 
        _error = True
    assert _error
    
    class A(object):
        def __init__(self, arg):
            self.arg = arg
        def two(self):
            """ function : two """
            return 2*self.arg
        def __str__(self):
            return str(self.arg)
        
    class B(A):
        def two(self):
            """ function : two """
            return 4*self.arg

    pyDatalog.create_terms('A,B')
    assert str( ((X==8) & (Y==A(X))) >= Y) == "8"
    assert ( ((X==8) & (Y==A(X).two())) >= Y) == 16
    assert ( ((X==8) & (Y==B(X).two())) >= Y) == 32
    assert ( ((X==8) & (Y==A.two(A(X)))) >= Y) == 16
    + (p[8]==8)
    assert str( ((X==8) & (Y==A(p[X]+1))) >= Y) == "9"

    _error = False
    try:
        pyDatalog.create_atoms('A.qsetrwxcfv')
    except Exception as e:
        e_message = e.message if hasattr(e, 'message') else e.args[0]
        if e_message != "'Classe' object has no attribute 'qsetrwxcfv'":
            print(e_message) 
        _error = True
    assert _error
    
    def hello(X):
        return 'Hello ' + X
    
    pyDatalog.create_atoms('hello')
    assert (hello('pyDatalog')) == 'Hello pyDatalog'
    assert((Y==hello('pyDatalog')) >= Y) == 'Hello pyDatalog'
    assert (((X=='pyDatalog') & (Y==hello(X))) >= Y) == 'Hello pyDatalog'
    assert (((X=='pyDatalog') & (Y==hello(X))) >= Y) == 'Hello pyDatalog'
    
    print("Test completed successfully.")

    print(time.time() - start_time)