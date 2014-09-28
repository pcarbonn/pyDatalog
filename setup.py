import os
import re
import sys
from distutils.command.build_ext import build_ext
from distutils.errors import (CCompilerError, DistutilsExecError,
                              DistutilsPlatformError)

has_feature = False
try:
    from setuptools import setup, Extension
    try:
        # see
        # https://bitbucket.org/pypa/setuptools/issue/65/deprecate-and-remove-features,
        # where they may remove Feature.
        from setuptools import Feature
        has_feature = True
    except ImportError:
        pass
except ImportError:
    from distutils.core import setup, Extension


cmdclass = {}
extra = {}
py3k = False
if sys.version_info < (2, 7):
    raise Exception("pyDatalog requires Python 2.7 or higher.")
elif sys.version_info >= (3, 0):
    py3k = True

import platform
cpython = platform.python_implementation() == 'CPython'

ext_errors = (CCompilerError, DistutilsExecError, DistutilsPlatformError)
if sys.platform == 'win32':
    # 2.6's distutils.msvc9compiler can raise an IOError when failing to
    # find the compiler
    ext_errors += (IOError,)


class BuildFailed(Exception):

    def __init__(self):
        self.cause = sys.exc_info()[1]  # work around py 2/3 different syntax


class ve_build_ext(build_ext):
    # This class allows C extension building to fail.

    def run(self):
        try:
            build_ext.run(self)
        except DistutilsPlatformError:
            raise BuildFailed()

    def build_extension(self, ext):
        try:
            build_ext.build_extension(self, ext)
        except ext_errors:
            raise BuildFailed()
        except ValueError:
            # this can happen on Windows 64 bit, see Python issue 7511
            if "'path'" in str(sys.exc_info()[1]):  # works with both py 2/3
                raise BuildFailed()
            raise BuildFailed()

cmdclass['build_ext'] = ve_build_ext

def status_msgs(*msgs):
    print('*' * 75)
    for msg in msgs:
        print(msg)
    print('*' * 75)


def find_packages(location):
    packages = []
    for pkg in ['pyDatalog']:
        for _dir, subdirectories, files in (
                os.walk(os.path.join(location, pkg))):
            if '__init__.py' in files:
                tokens = _dir.split(os.sep)[len(location.split(os.sep)):]
                packages.append(".".join(tokens))
    return packages



from pyDatalog import version

def run_setup(with_cext):
    kwargs = extra.copy()
    ext_modules = [
        Extension('pyDatalog.pyEngine',
                  sources=['pyDatalog/pyEngine.c']),
        ]

    if with_cext:
        if has_feature:
            kwargs['features'] = {'cextensions': Feature(
                "optional C speed-enhancements",
                standard=True,
                ext_modules=ext_modules
                )}
        else:
            kwargs['ext_modules'] = ext_modules

    setup(
        name = "pyDatalog",
        packages = ["pyDatalog", "pyDatalog/examples"],
        version = version.__version__,
        cmdclass=cmdclass,
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
    
     """,
     **kwargs
    )
    
if not cpython:
    run_setup(False)
    status_msgs(
        "WARNING: C extensions are not supported on " +
        "this Python platform, speedups are not enabled.",
        "Plain-Python build succeeded."
    )
else:
    try:
        run_setup(True)
        status_msgs(
            "C extensions installed successfully."
        )
    except BuildFailed as exc:
        status_msgs(
            exc.cause,
            "WARNING: The C extension could not be compiled, " +
            "speedups are not enabled.",
            "Failure information, if any, is above.",
            "Retrying the build without the C extension now."
        )

        run_setup(False)

        status_msgs(
            "WARNING: The C extension could not be compiled, " +
            "speedups are not enabled.",
            "Plain-Python build succeeded."
        )