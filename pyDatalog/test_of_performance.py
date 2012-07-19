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

"""
results on Intel Core i7-2820 QM CPU @ 2.3 GHz (run from Command prompt):
* python 2.7 : 14.84 sec
* python 3.2 : 13.10 sec
* pypy 1.9 : 6.59 sec

"""
import time
import six

import pyDatalog
def test():

    """ Large database + deep recursion """
    pyDatalog.clear()
    for i in range(10000):
        pyDatalog.assert_fact('successor', i+1, i+0)
        
    @pyDatalog.program()
    def _(): # the function name is ignored
        assert ask(successor(1801,1800)) == set([(1801, 1800)])

        + even(0)
        even(N) <= (N > 0) & successor(N,N1) & odd(N1)
        odd(N) <= (N > 0) & successor(N,N2) & even(N2)
        
        assert ask(odd(299)) == set([(299,)]) 
        assert ask(odd(9999)) == set([(9999,)])
        
        # TODO why is this much much slower ??
        # odd(N) <= even(N1) & successor(N, N1)

    """ Deep recursion """
    pyDatalog.clear()
    @pyDatalog.program()
    def _(): # the function name is ignored

        + even(0)
        even(N) <= (N > 0) & (N1==N-1) & odd(N1)
        assert ask(even(0)) == set([(0,)])
        odd(N) <= (N > 0) & ~ even(N)

        assert ask(odd(9999)) == set([(9999,)])
            
    print("Done.")

if __name__ == "__main__":
    start_time = time.time()
    test()
    print(time.time() - start_time, "seconds")
