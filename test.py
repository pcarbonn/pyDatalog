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
import pyDatalog

if __name__ == "__main__":

    # instantiate a pyDatalog engine
    datalog_engine = pyDatalog.Datalog_engine()
    
    print "Defining a datalog program...",
    
    # a decorator is used to create a program on the pyDatalog engine
    @pyDatalog.program(datalog_engine)
    def _(): # the function name is ignored
        + p(a) # assert a unary fact
        # check that unary queries work
        assert ask(p(a)) == set([('a',)])
        assert ask(p(X)) == set([('a',)])
        assert ask(p(Y)) == set([('a',)])
        assert ask(p(b)) == None
        
        + p(b)
        assert ask(p(X)) == set([('a',), ('b',)])
        
        + p(b) # facts are unique
        assert ask(p(X)) == set([('a',), ('b',)])
        
        - p(b) # retract a unary fact
        assert ask(p(X)) == set([('a',)])
        
        # strings and integers
        + p('c')
        assert ask(p(c)) == set([('c',)])
        
        + p(1)
        assert ask(p(1)) == set([('1',)])

        # idem for secondary facts
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
        
        # integer variable
        for _i in _range(10):
            + next(_i+1, _i)
        assert ask(next(2, 1)) == set([('2', '1')])
        
        # clauses
        r(X, Y) <= p(X) & p(Y)
        assert ask(r(a, a)) == set([('a', 'a')])
        # TODO more
        
        # equality:
        s(X, Y) <= p(X) & (X == Y)
        assert ask(s(a, a)) == set([('a', 'a')])
        assert ask(s(a, b)) == None
        assert ask(s(X,a)) == set([('a', 'a')])
        print ask(s(X, Y))
        assert ask(s(X, Y)) == set([('a', 'a'),('c', 'c'),('1', '1')])
        # TODO  make this work
        # s <= (X == Y)   
        # assert ask(s(X,Y)) == set([('a', 'a'),('c', 'c'),('1', '1')])

        # unary plus defines a fact
        + farmer(moshe)
        + donkey(eeyore)
        ask(farmer(X))
        ask(donkey(Y))
        # <= + + defines a clause 
        beats(X1,Y1) <= farmer(X1) & donkey(Y1) 
        #beats(X,Y) <= farmer(X) & donkey(Y)
        ask(beats(moshe, eeyore))
        ask(beats(X2, eeyore))
        ask(beats(moshe, Y2))
        ask(beats(X3, Y3))

    #ask(farmer(moshe))

    print "Done."
    print "Definition has already updated the database as follows:"
    print
    datalog_engine.prt()
    print
    print "Trying to call the logical function raises an error:"
    print
    _()