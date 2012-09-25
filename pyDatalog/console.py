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
This console application lets you enter pyDatalog clauses and facts, 
and query the datalog database directly.  (To experiment with the python API,
please use the python console/IDLE instead)

Sample session:
    pyDatalog> +p(a)
    pyDatalog> ask(p(X))
    [('a',)]
    pyDatalog> exit()

"""
import code
import sys
from pyDatalog import pyDatalog
from pyDatalog import pyEngine

pyEngine.Auto_print = True

class datalogConsole(code.InteractiveConsole):
    def runsource(self, source, filename='console', symbol='single'):
        pySource = """
pyDatalog.load('''
%s
''')
""" % source
        try:
            code.InteractiveConsole.runsource(self, pySource, filename, symbol)
        except Exception as e:
            print(e)

sys.ps1 = 'pyDatalog> '
if __name__ == "__main__":
    console = datalogConsole(locals=locals())
    console.interact('')
