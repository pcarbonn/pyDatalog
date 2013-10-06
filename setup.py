from distutils.core import setup
from pyDatalog import version
setup(
    name = "pyDatalog",
    packages = ["pyDatalog", "pyDatalog/examples"],
    version = version.__version__,
    description = "A pure-python implementation of a truly declarative language derived from prolog.  Run logic queries on databases or python objects, and use logic clauses to define python classes.",
    author = "Pierre Carbonnelle",
    author_email = "pierre.carbonnelle@gmail.com",
    url = "https://sites.google.com/site/pydatalog/",
    download_url = "http://pypi.python.org/pypi?name=pyDatalog&:action=display",
    keywords = "prolog, logic programming, datalog, object-relational mapping, data integration",
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
pyDatalog adds the logic programming paradigm to Python's toolbox, in a pythonic way.  
You can now run logic queries on databases or Python objects, and use logic clauses to define python classes.

Datalog is a truly declarative language derived from Prolog, with strong academic foundations.  It complements Python very well for:

* managing complex sets of related information (e.g. in data integration or the semantic web).  
* simulating intelligent behavior (e.g. in games), 
* performing recursive algorithms (e.g. in network protocol, code and graph analysis, parsing)
* solving discrete constraint problems. 

In particular, pyDatalog can be used as a query language: 

* it can perform multi-database queries (from memory datastore, 11 relational databases, and noSQL database with appropriate connectors)
* it is more expressive than SQL, with a cleaner syntax; 
* it facilitates re-use of SQL code snippet (e.g. for frequent joins or formula); 

Datalog excels at accelerated development : Datalog programs are often shorter than their Python equivalent, 
and Datalog statements can be specified in any order, as simply as formula in a spreadsheet."""
)