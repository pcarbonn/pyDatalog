# chardet's setup.py
from distutils.core import setup
setup(
    name = "pyDatalog",
    packages = ["pyDatalog"], # TODO add files
    version = "0.1",
    description = "Datalog embedded in Python",
    author = "Pierre Carbonnelle",
    author_email = "pierre.carbonnelle@gmail.com",
    url = "https://bitbucket.org/pcarbonn/pydatalog/wiki/Home",
    download_url = "", # TODO
    classifiers = [
        "Programming Language :: Python",
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 3",
        "Programming Language :: Other Scripting Engines",
        "Development Status :: 4 - Beta",
        "Environment :: Other Environment",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: GNU Library or Lesser General Public License (LGPL)",
        "Operating System :: OS Independent",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        ],
    long_description = """\
pyDatalog brings logic programming to python.  
It is a wrapper around the Datalog engine developed in lua by John D. Ramsdell.  
It uses the SLG resolution algorithm with 'memoization' for high performance.

Assert facts and insert logic clauses directly in your python code, and use the inference engine to resolve complex, recursive queries fast.  

Datalog is a subset of prolog that is best at simulating intelligent behavior (for games), 
at performing recursive algorithms (e.g. in graph analysis) 
or at managing large sets of related information (e.g. in the semantic web).  

Datalog programs are often shorter than their python equivalent.  
"""
)