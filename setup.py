# -*- coding: utf-8 -*-
import os
import sys
import platform
from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
from distutils.errors import (CCompilerError, DistutilsExecError,
                              DistutilsPlatformError)

EXT_ERRORS = (CCompilerError, DistutilsExecError, DistutilsPlatformError)
if sys.platform == 'win32':
    EXT_ERRORS = EXT_ERRORS + (IOError,)

class BuildFailed(Exception):
    def __init__(self, *args, **kwargs):
        self.cause = sys.exc_info()[1]
        del args
        del kwargs

class VeBuildExt(build_ext):
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
            if "'path'" in str(sys.exc_info()[1]):
                raise BuildFailed()
            raise BuildFailed()

CPYTHON = platform.python_implementation() == 'CPython'

def run_setup(with_cext):
    ext_modules = [
        Extension('pyDatalog.pyEngine',
                  sources=['pyDatalog/pyEngine.c']),
    ]

    kwargs = {}
    if with_cext:
        kwargs['ext_modules'] = ext_modules

    setup(
        name="pyDatalog",
        cmdclass={'build_ext': VeBuildExt},
        **kwargs
    )

if not CPYTHON:
    run_setup(False)
else:
    try:
        run_setup(True)
    except BuildFailed:
        run_setup(False)