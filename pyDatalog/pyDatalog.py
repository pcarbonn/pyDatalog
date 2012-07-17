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
methods / classes exposed by this file:
methods for direct access to datalog knowledge base:
  * assert_fact(predicate_name, *args) : assert predicate_name(args)
  * retract_fact(predicate_name, *args) : retracts predicate_name(args)
  * program() : decorator function to create datalog programs
  * load(code) : loads the clauses contained in the code string
  * ask(code) : returns the result of the query contained in the code string
  * clear() : resets the datalog database
Answer class defines the object returned by ask()
  * __eq__ for equality test
  * __str__ for printing
classes for Python Mixin:
  * Variable : a class to define Variable, for use in datalog queries
  * Mixin : a class that can be mixed in another class to give it datalog capability
  * sqlMetaMixin : a metaclass to be used when creating a class with both SQLAlchemy and datalog capability
  
"""
from collections import defaultdict
import sys
import weakref

try:
    from . import pyEngine
    from . import pyParser
    from .pyParser import Symbol, Expression, Lambda, Literal, Body
except ValueError:
    import pyEngine
    import pyParser
    from pyParser import Symbol, Expression, Lambda, Literal, Body

try:
    from sqlalchemy.ext.declarative import DeclarativeMeta
except:
    DeclarativeMeta = object

class Dummy(object):
    pass
try:
    from sqlalchemy.orm.attributes import InstrumentedAttribute
except:
    InstrumentedAttribute = Dummy # not used; could be any class, really
    

""" ****************** direct access to datalog knowledge base ***************** """

def assert_fact(predicate_name, *args):
    """ assert predicate_name(args) """
    literal = Literal(predicate_name, args)
    _assert_fact(literal)

def retract_fact(predicate_name, *args):
    """ retracts predicate_name(args) """
    literal = Literal(predicate_name, args)
    _retract_fact(literal)

def program():
    """ A decorator for datalog program  """
    return pyParser.add_program

def load(code):
    """loads the clauses contained in the code string """
    return pyParser.load(code)

def ask(code, _fast=None):
    """returns the result of the query contained in the code string"""
    return pyParser.ask(code, _fast)

def clear():
    """ resets the default datalog database """
    pyEngine.clear()

class Answer(object):
    """ object returned by ask() """
    def __init__(self, name, arity, answers):
        self.name = name
        self.arity = arity
        self.answers = answers

    def __eq__ (self, other):
        return set(self.answers) == other if self.answers else other is None
    
    def __str__(self):
        return str(set(self.answers))
    
#utility functions, also used by pyParser

def _assert_fact(literal):
    clause = pyEngine.make_clause(literal.lua, [])
    pyEngine.assert_(clause)
    
def _retract_fact(literal):
    clause = pyEngine.make_clause(literal.lua, [])
    pyEngine.retract(clause)

def add_clause(head,body):
    if isinstance(body, Body):
        tbl = [a.lua for a in body.literals]
    else: # body is a literal
        tbl = (body.lua,)
    clause = pyEngine.make_clause(head.lua, tbl)
    return pyEngine.assert_(clause)
    
def _ask_literal(literal, _fast=None): # called by Literal
    # print("asking : %s" % str(literal))
    result = pyEngine.ask2(literal.lua, _fast)
    return result

#circ: share functions with pyParser and avoid circular import
pyDatalog = Dummy()
pyDatalog._ask_literal = _ask_literal
pyDatalog.add_clause = add_clause
pyDatalog._assert_fact = _assert_fact
pyDatalog._retract_fact = _retract_fact
pyDatalog.Answer = Answer
pyParser.pyDatalog = pyDatalog
pyEngine.pyDatalog = pyDatalog

""" ****************** python Mixin ***************** """

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
        
        def _getattr(self, attribute):
            """ responds to instance.method by asking datalog engine """
            if not attribute == '__iter__' and not attribute.startswith('_sa_'):
                predicate_name = "%s.%s[1]" % (self.__class__.__name__, attribute)
                literal = Literal(predicate_name, (self, Symbol("X")))
                result = _ask_literal(literal)
                return result.answers[0][-1] if result else None
            else: raise AttributeError
        cls.__getattr__ = _getattr   
    
    def __getattr__(cls, method):
        """when access to an attribute of a subclass of Mixin fails, return an object that responds to () and to [] """
        if cls in ('Mixin', 'metaMixin') or method in (
                '__mapper_cls__', '_decl_class_registry', '__sa_instrumentation_manager__', 
                '__table_cls__'):
            raise AttributeError

        class Pseudo_attribute(object):
            def __call__(self, *args):
                """ responds to class.attribute(x,y)"""
                predicate_name = "%s.%s" % (cls.__name__, method)
                
                terms, env = [], {}
                for i, arg in enumerate(args):
                    if isinstance(arg, Variable):
                        del arg[:] # reset variables
                        # deal with (X,X)
                        variable = env.get(id(arg), Symbol('X%i' % i))
                        env[id(arg)] = variable
                        terms.append(variable)
                    elif i==0 and arg.__class__ != cls:
                        raise TypeError("Object is incompatible with the class that is queried.")
                    else:
                        terms.append(arg)
                    
                literal = Literal(predicate_name, terms)
                result = _ask_literal(literal)
                if result: 
                    transposed = list(zip(*result.answers)) # transpose result
                    for i, arg in enumerate(args):
                        if isinstance(arg, Variable) and len(arg)==0:
                            arg.extend(transposed[i])
                return result
            
            def __getitem__(self, keys):
                """ responds to class.attribute[x,y] by returning another object"""
                if not isinstance(keys, tuple):
                    keys = (keys,)
                class Logic_function(object):
                    def __eq__(self, other):
                        if isinstance(other, Variable) or not getattr(other, '__iter__', False):
                            other = (other,)
                        predicate_name = "%s.%s[%i]" % (cls.__name__, method, len(keys))
                        
                        #TODO avoid repetition of loops, by joining keys and other
                        terms, env = [], {}
                        for i, arg in enumerate(keys):
                            if isinstance(arg, Variable):
                                del arg[:] # reset variables
                                # deal with [X,X]
                                variable = env.get(id(arg), Symbol('X%i' % i))
                                env[id(arg)] = variable
                                terms.append(variable)
                            elif i==0 and arg.__class__ != cls:
                                raise TypeError("Object is incompatible with the class that is queried.")
                            else:
                                terms.append(arg)
                        for i, arg in enumerate(other):
                            if isinstance(arg, Variable):
                                del arg[:] # reset variables
                                # deal with [X,X]
                                variable = env.get(id(arg), Symbol('Y%i' % i))
                                env[id(arg)] = variable
                                terms.append(variable)
                            else:
                                terms.append(arg)
                            
                        literal = Literal(predicate_name, terms)
                        result = _ask_literal(literal)
                        if result: 
                            transposed = list(zip(*result.answers)) # transpose result
                            for i, arg in enumerate(keys):
                                if isinstance(arg, Variable) and len(arg)==0:
                                    arg.extend(transposed[i])
                            j = i+1
                            for i, arg in enumerate(other):
                                if isinstance(arg, Variable) and len(arg)==0:
                                    arg.extend(transposed[j+i])
                        return result
                return Logic_function()
        return Pseudo_attribute()

    def pyDatalog_search(cls, literal):
        """Called by pyEngine to resolve a prefixed literal for a subclass of Mixin."""
        terms = literal.terms
        pred_name = literal.pred.id.split('/')[0]
        attr_name = pred_name.split('[')[0].split('.')[1]
        # TODO check prearity
        if len(terms)==2:
            X = terms[0]
            Y = terms[1]
            if X.is_const():
                # try accessing the attribute of the first term in literal
                if X.id.__class__ != cls:
                    raise TypeError("Object is incompatible with the class that is queried.")
                try:
                    Y1 = getattr(X.id, attr_name)
                except:
                    pass
                else:
                    if Y.is_const() and Y1 != Y.id:
                        return
                    if Y1: # ignore None's
                        result = Literal(pred_name, (X.id, Y1))
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
                        Y1 = getattr(r, attr_name)
                        if Y.is_const() and Y1 != Y.id:
                            return
                        if Y1:
                            result = Literal(pred_name, (r, Y1))
                            yield result.lua
                    return
            else: # python object with Mixin
                if not X.is_const() and Y.is_const():
                    # predicate(X, atom)
                    for X in metaMixin.__refs__[cls]:
                        X=X()
                        if getattr(X, attr_name)==Y.id:
                            yield Literal(pred_name, (X, Y.id)).lua 
                    return
                elif not X.is_const() and not Y.is_const():
                    # predicate(X, Y)
                    for X1 in metaMixin.__refs__[cls]:
                        X1=X1()
                        Y1 = getattr(X1, attr_name)
                        if Y1 and (X is not Y or ((X is Y) and (X1==Y1))): 
                            yield Literal(pred_name, (X1, Y1)).lua
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

""" ****************** support for SQLAlchemy ***************** """

class sqlMetaMixin(metaMixin, DeclarativeMeta): 
    """ metaclass to be used with Mixin for SQLAlchemy"""
    pass

""" attach a method to SQLAlchemy class.attribute, 
    so that it can answer queries like class.attribute(X,Y)"""
def InstrumentedAttribute_call(self, *args):
    cls = self.class_
    method = self.key
    return type(cls).__getattr__(cls, method)(*args)
InstrumentedAttribute.__call__ = InstrumentedAttribute_call

""" attach a method to SQLAlchemy class.attribute, 
    so that it can answer queries like class.attribute[X,Y]"""
def InstrumentedAttribute_getitem(self, *args):
    cls = self.class_
    method = self.key
    return type(cls).__getattr__(cls, method).__getitem__(*args)
InstrumentedAttribute.__getitem__ = InstrumentedAttribute_getitem
