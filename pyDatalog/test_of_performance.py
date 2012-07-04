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
    datalog_engine = pyDatalog.default_datalog_engine
    
    """ Large database + deep recursion """
    datalog_engine.clear()
    for i in range(2000):
        datalog_engine.assert_fact('successor', i+1, i+0)
        
    @pyDatalog.program()
    def _(): # the function name is ignored
        assert ask(successor(1801,1800)) == set([(1801, 1800)])

        + even(0)
        even(N) <= (N > 0) & successor(N,N1) & odd(N1)
        odd(N) <= (N > 0) & successor(N,N2) & even(N2)
        
        assert ask(odd(299)) == set([(299,)]) 
        assert ask(odd(1999)) == set([(1999,)])
        
        # TODO why is this much much slower ??
        # odd(N) <= even(N1) & successor(N, N1)

    """ Deep recursion """
    datalog_engine.clear()
    @pyDatalog.program(datalog_engine)
    def _(): # the function name is ignored

        + even(0)
        even(N) <= (N > 0) & (N1==N-1) & odd(N1)
        assert ask(even(0)) == set([(0,)])
        odd(N) <= (N > 0) & ~ even(N)

        assert ask(odd(2099)) == set([(2099,)])
            
    print("Done.")

if __name__ == "__main__":
    pyDatalog.default_datalog_engine = pyDatalog.Datalog_engine()
    #test()
    cProfile.runctx('test()', globals(), locals())