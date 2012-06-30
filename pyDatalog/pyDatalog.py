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

This work is derived from Pythologic, (C) 2004 Shai Berger, 
in accordance with the Python Software Foundation licence.
(See http://code.activestate.com/recipes/303057/ and
http://www.python.org/download/releases/2.0.1/license/ )

"""

"""

Datalog vocabulary:
    q(X):- q(a)
        X is a variable term
        a is a constant term
        q is a predicate
        q(a) is a literal
        q(X):- q(a) is a clause, where q(X) is the head and q(a) the body

Design principle:
Instead of writing our own parser, we use python's parser.  The datalog code is first compiled in python byte code, 
then "undefined" variables are initialized as instance of Symbol, then the code is finally executed to load the clauses.
This is done in the load() and add_program() method of Datalog_engine class.

Classes contained in this file:
* Symbol : contains a constant, a variable or a predicate. Instantiated before executing the datalog program
* Expression : made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program
* Lambda : represents a lambda function, used in expressions
* Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
* Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
* Datalog_engine_ : common part for an engine. Subclasses are Python_engine and Lua_engine
* Python_engine :implements the interface to the datalog engine written in python.  Instantiated by the calling module
* Lua_engine :implements the interface to the lua datalog engine written in Lua.  Instantiated by the calling module

Methods contained in this file:
* Datalog_engine(implementation=None) : a factory for Datalog_engine_. Returns a python or lua engine, depending on parameter
* program(datalog_engine=None) : decorator function to create datalog programs
* ask(code) : returns the result of the query contained in the code string, and run in the default datalog engine
* load(code) : loads the clauses contained in the code string into the default datalog engine
* clear() : resets the default engine

"""
from collections import defaultdict
import os
import re
import string
import six
from six.moves import builtins
import sys
import weakref

try:
    from sqlalchemy.ext.declarative import DeclarativeMeta
except:
    DeclarativeMeta = object

try:
    from sqlalchemy.orm.attributes import InstrumentedAttribute
except:
    InstrumentedAttribute = object()
    
PY3 = sys.version_info[0] == 3
func_code = '__code__' if PY3 else 'func_code'

# determine which engine to use, python or lua
try:
    import lupa
    from lupa import LuaRuntime
    Engine = 'Lua'
except:
    Engine = 'Python'
Engine = 'Python' # let's use Python after all
print(('Using %s engine for Datalog.' % Engine))

try:
    from . import pyEngine
except ValueError:
    import pyEngine

try:
    from . import pyParser
    from .pyParser import Datalog_engine_, Python_engine, Lua_engine
    from .pyParser import Symbol, Expression, Lambda, Literal, Body
except ValueError:
    import pyParser
    from pyParser import Datalog_engine_, Python_engine, Lua_engine
    from pyParser import Symbol, Expression, Lambda, Literal, Body
default_datalog_engine = pyParser.default_datalog_engine
Datalog_engine = pyParser.Datalog_engine
    
class Variable(list):
    pass

"""Keep a dictionary of classes with datalog capabilities.  This list is used by pyEngine to resolve prefixed literals."""
Class_dict = {}
pyEngine.Class_dict = Class_dict # TODO better way to share it with pyEngine.py ?

class metaMixin(type):
    """Metaclass used to define the behavior of a subclass of Mixin"""
    __refs__ = defaultdict(list)
    
    def __init__(cls, name, bases, dct):
        """when creating a subclass of Mixin, save the subclass in Class_dict. """
        super(metaMixin, cls).__init__(name, bases, dct)
        Class_dict[name]=cls
        cls.has_SQLAlchemy = any(base.__module__ in ('sqlalchemy.ext.declarative',) for base in bases)
    
    def __getattr__(cls, method):
        """when access to an attribute of a subclass of Mixin fails, return a callable that queries pyEngine """
        if cls in ('Mixin', 'metaMixin') or method in (
                '__mapper_cls__', '_decl_class_registry', '__sa_instrumentation_manager__', 
                '__table_cls__'):
            raise AttributeError

        def my_callable(self, *args):
            predicate_name = "%s.%s" % (cls.__name__, method)
            
            terms = []
            # first term is self
            if isinstance(self, Variable): 
                del self[:] # reset variable
                terms.append( Symbol('X') )
            elif self.__class__ != cls:
                raise TypeError("Object is incompatible with the class that is queried.")
            else:
                terms.append( self ) 
            
            for i, arg in enumerate(args):
                if isinstance(arg, Variable):
                    del arg[:] # reset variables
                    terms.append(Symbol('X%i' % i))
                else:
                    terms.append(arg)
                
            literal = Literal(predicate_name, terms)
            result = default_datalog_engine._ask_literal(literal)
            if result: 
                result = list(zip(*result)) # transpose result
                if isinstance(self, Variable) and len(self)==0:
                    self.extend(result[0])
                for i, arg in enumerate(args):
                    if isinstance(arg, Variable) and len(arg)==0:
                        arg.extend(result[i+1])
            return result
        return my_callable

    def pyDatalog_search(cls, literal):
        """Called by pyEngine to resolve a prefixed literal for a subclass of Mixin."""
        terms = literal.terms
        pred_name = literal.pred.id.split('/')[0]
        attr_name = pred_name.split('.')[1]
        if len(terms)==2:
            X = terms[0]
            Y = terms[1]
            if X.is_const():
                # try accessing the attribute of the first term in literal
                if X.id.__class__ != cls:
                    raise TypeError("Object is incompatible with the class that is queried.")
                try:
                    Y = getattr(X.id, attr_name)
                except:
                    pass
                else:
                    if Y: # ignore None's
                        result = Literal(pred_name, (X.id, Y))
                        yield result.lua
                    return
            if cls.has_SQLAlchemy:
                if cls.session:
                    q = cls.session.query(cls)
                    if X.is_const():
                        raise RuntimeError ("%s could not be resolved" % pred_name)
                    if Y.is_const():
                        q = q.filter(getattr(cls, attr_name) == Y.id)
                    for r in q:
                        Y = getattr(r, attr_name)
                        if Y:
                            result = Literal(pred_name, (r, Y))
                            yield result.lua
                    return
            else:
                if not X.is_const() and Y.is_const():
                    # predicate(X, atom)
                    for X in metaMixin.__refs__[cls]:
                        if getattr(X(), attr_name)==Y.id:
                            yield Literal(pred_name, (X(), Y.id)).lua 
                    return
                elif not X.is_const() and not Y.is_const():
                    # predicate(X, Y)
                    for X in metaMixin.__refs__[cls]:
                        Y = getattr(X(), attr_name)
                        if Y:
                            yield Literal(pred_name, (X(), Y)).lua
                    return
        raise RuntimeError ("%s could not be resolved" % pred_name)

# following syntax to declare Mixin is used for compatibility with python 2 and 3
Mixin = metaMixin('Mixin', (object,), {})

""" When creating a Mixin object without SQLAlchemy, add it to the list of instances,
    so that it can be included in the result of queries"""
def __init__(self):
    if not self.__class__.has_SQLAlchemy:
        metaMixin.__refs__[self.__class__].append(weakref.ref(self))
Mixin.__init__ = __init__

class sqlMetaMixin(metaMixin, DeclarativeMeta): 
    """ metaclass to be used with Mixin for SQLAlchemy"""
    pass

""" attach a method to answer class.attribute(X,Y) queries for classes with SQLAlchemy"""
def InstrumentedAttribute_call(self, *args):
    cls = self.class_
    method = self.key
    return cls.__getattr__(method)(*args)
InstrumentedAttribute.__call__ = InstrumentedAttribute_call

def program(datalog_engine=None):
    """
    A decorator for datalog program
    """
    datalog_engine = datalog_engine or default_datalog_engine
    return datalog_engine.add_program

def ask(code):
    """returns the result of the query contained in the code string, and run in the default datalog engine"""
    return default_datalog_engine.ask(code)
def load(code):
    """loads the clauses contained in the code string into the default datalog engine"""
    return default_datalog_engine.load(code)
def clear():
    """ resets the default datalog engine """
    global default_datalog_engine
    default_datalog_engine.clear()

