from distutils.core import setup
setup(
    name = "pyDatalog",
    packages = ["pyDatalog"],
    version = "0.8.0",
    description = "A pure-python implementation of a subset of prolog, using SLG algorithm with memoization",
    author = "Pierre Carbonnelle",
    author_email = "pierre.carbonnelle@gmail.com",
    url = "https://bitbucket.org/pcarbonn/pydatalog/wiki/Home",
    download_url = "http://pypi.python.org/pypi?name=pyDatalog&:action=display",
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
pyDatalog embeds logic programming in python.  Assert facts and insert logic clauses directly in your python code, 
and use the inference engine to resolve complex, recursive queries fast.  

Datalog is a subset of prolog that is best 
at managing large sets of related information (e.g. in data integration or the semantic web).  
at simulating intelligent behavior (e.g. in games), 
or at performing recursive algorithms (e.g. in network protocol, code and graph analysis) 

Datalog statements can be specified in any order, eliminating the need for sequence diagrams and the associated risk of tricky errors. 
Datalog programs are often shorter than their python equivalent.  
""",
    setup_requires = ["six"],
    install_requires = ["six"]
)