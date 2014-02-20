from distutils.core import setup
from pyDatalog import version
setup(
    name = "pyDatalog",
    packages = ["pyDatalog", "pyDatalog/examples"],
    version = version.__version__,
    description = "A pure-python implementation of Datalog, a truly declarative language derived from Prolog.",
    author = "Pierre Carbonnelle",
    author_email = "pierre.carbonnelle@gmail.com",
    url = "https://sites.google.com/site/pydatalog/",
    download_url = "http://pypi.python.org/pypi?name=pyDatalog&:action=display",
    keywords = "prolog, logic programming, database, SQL, data integration, expert system, AI",
    classifiers = [
        "Programming Language :: Python",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Programming Language :: Prolog",
        "Development Status :: 4 - Beta",
        "Environment :: Other Environment",
        "Intended Audience :: Developers",
        "Intended Audience :: Education",
        "Topic :: Database",
        "Topic :: Database :: Database Engines/Servers",
        "License :: OSI Approved :: GNU Library or Lesser General Public License (LGPL)",
        "Operating System :: OS Independent",
        "Topic :: Software Development :: Libraries :: Python Modules",
        "Topic :: Scientific/Engineering :: Artificial Intelligence",
        ],
    long_description = """\
pyDatalog adds the logic Programming paradigm to Python's toolbox, in a pythonic way.

Logic programmers can now use the extensive standard library of Python, and Python programmers can now express complex algorithms simply.  

Datalog is a truly declarative language derived from Prolog, with strong academic foundations.  Datalog excels at managing complexity.  
Datalog programs are shorter than their Python equivalent, and Datalog statements can be specified in any order, as simply as formula in a spreadsheet. 

In particular, Datalog can be used for:

* simulating intelligent behavior (e.g. in games or expert systems), 
* performing recursive algorithms (e.g. in network protocol, code and graph analysis),
* managing complex sets of related information (e.g. in data integration or the semantic web), 
* solving discrete constraint problems. 

.. pull-quote::

    "Datalog is to Python what Python was to C, and what C was to Assembly."

 """
)