'''
This setup.py is used to compile pyEngine.py exclusively

Created on 29 aout 2014

@author: pcarbonn
'''

from distutils.core import setup
from Cython.Build import cythonize

setup(
  name = 'pyEngine',
  ext_modules = cythonize("pyDatalog\pyEngine.py"),
)