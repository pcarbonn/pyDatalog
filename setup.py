# -*- coding: utf-8 -*-
#################### IMPORTS
import os
import sys
import io
#
from distutils.command.build_ext import build_ext
from distutils.errors import (CCompilerError, DistutilsExecError,
                              DistutilsPlatformError)
from distutils.core import Command as TestCommand
#
#################### GLOBAL VARIABLES
BEFORE_BREAK_PY3 = (2, 7)
AFTER_BREAK_PY3 = (3, 0)
LINE_STARS = '*' * 75
PYDATALOG = 'pyDatalog'
TEST = 'test'
CURRENT = '.'
#
# The following lines could be improved
RUNTESTS = 'placeholder.py'
RUNTESTS = [CURRENT, PYDATALOG, TEST, RUNTESTS]
RUNTESTS = os.path.join(*RUNTESTS)
#
INIT = '__init__.py'
README = "README.md"
CHANGES = "CHANGES.txt"
TODO = "TODO.md"
SHORT_DESCRIPTION = "A pure-python implementation of Datalog, "\
"a truly declarative language derived from Prolog."

#################### SYSTEM ANALYSIS
#
HAS_FEATURE = False
#
try:
    from setuptools import setup, Extension
    try:
        # see
        # https://bitbucket.org/pypa/setuptools/issue/65/deprecate-and-remove-features,
        # where they may remove Feature.
        from setuptools import Feature
        HAS_FEATURE = True
    except ImportError:
        pass
except ImportError:
    from distutils.core import setup, Extension


#
CMDCLASS = {}
EXTRA = {}
PY3K = False
#
if sys.version_info < BEFORE_BREAK_PY3:
    raise Exception("pyDatalog requires Python 2.7 or higher.")
elif sys.version_info >= AFTER_BREAK_PY3:
    PY3K = True

import platform
CPYTHON = platform.python_implementation() == 'CPython'

EXT_ERRORS = (CCompilerError, DistutilsExecError, DistutilsPlatformError)
if sys.platform == 'win32':
    # 2.6's distutils.msvc9compiler can raise an IOError when failing to
    # find the compiler
    EXT_ERRORS = EXT_ERRORS + (IOError,)

######################## CLASSES

class BuildFailed(Exception):
    """
    Specific Error message to work around py 2/3
    """

    def __init__(self, *args, **kwargs):
        self.cause = sys.exc_info()[1]  # work around py 2/3 different syntax
        del args
        del kwargs


class VeBuildExt(build_ext):
    """This class allows C extension building to fail."""

    def run(self):
        try:
            build_ext.run(self)
        except DistutilsPlatformError:
            raise BuildFailed()

    def build_extension(self, ext):
        try:
            build_ext.build_extension(self, ext)
        except EXT_ERRORS:
            raise BuildFailed()
        except ValueError:
            # this can happen on Windows 64 bit, see Python issue 7511
            if "'path'" in str(sys.exc_info()[1]):  # works with both py 2/3
                raise BuildFailed()
            raise BuildFailed()

class PyTest(TestCommand):
    """
    For "python setup.py test" working.
    Usually, PyTest is subclassed from setuptools.
    Here, it is done from distutils.
    Have a look at http://pytest.org/latest/goodpractises.html#
    integrating-with-distutils-python-setup-py-test
    """
    #
    # a KILLER line, indeed !
    user_options = []
    def initialize_options(self):
        """
        Subclassing abstract class PyTest
        """
        pass
    #
    def finalize_options(self):
        """
        Subclassing abstract class PyTest
        """
        pass

    def run(self):
        """
        Here, RUNTESTS needs to be tuned...
        """
        import subprocess
        import sys
        errno = subprocess.call([sys.executable,
                                 RUNTESTS])
        raise SystemExit(errno)

#
##################### FUNCTIONS
#
def _read(*filenames, **kwargs):
    """
    Read the docs and give a long description.
    """
    encoding = kwargs.get('encoding', 'utf-8')
    sep = kwargs.get('sep', '\n')
    buf = []
    for filename in filenames:
        try:
            with io.open(filename, encoding=encoding) as _fic:
                buf.append(_fic.read())
        except IOError:
            pass
    return sep.join(buf)


def status_msgs(*msgs):
    """
    pretty message formatter
    """
    print(LINE_STARS)
    for msg in msgs:
        print(msg)
    print(LINE_STARS)


#
############## PREPARING SETUP VALUES
#
from pyDatalog import version as VERSION
_LONG_DESCRIPTION = _read(README, CHANGES, TODO)
#
CMDCLASS['build_ext'] = VeBuildExt
CMDCLASS['test'] = PyTest
#

def run_setup(with_cext):
    """
    Contains the whole configuration.
    """
    kwargs = EXTRA.copy()
    ext_modules = [
        Extension('pyDatalog.pyEngine',
                  sources=['pyDatalog/pyEngine.c']),
        ]

    if with_cext:
        if HAS_FEATURE:
            kwargs['features'] = {'cextensions': Feature(
                "optional C speed-enhancements",
                standard=True,
                ext_modules=ext_modules
                )}
        else:
            kwargs['ext_modules'] = ext_modules

    setup(
        name = PYDATALOG,
        packages = [PYDATALOG, "pyDatalog/examples"],
        version = VERSION.__version__,
        cmdclass = CMDCLASS,
        description = SHORT_DESCRIPTION,
        author = "Pierre Carbonnelle",
        author_email = "pierre.carbonnelle@gmail.com",
        tests_require=['pytest'],
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
        long_description = _LONG_DESCRIPTION,        
     **kwargs
    )

####################### MAIN

if not CPYTHON:
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