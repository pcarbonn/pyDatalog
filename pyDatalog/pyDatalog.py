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
DatalogError class (defined in util.py)
methods for direct access to datalog knowledge base:
  * assert_fact(predicate_name, *args) : assert predicate_name(args)
  * retract_fact(predicate_name, *args) : retracts predicate_name(args)
  * program() : decorator function to create datalog programs
  * predicate() : decorator function to create a predicate resolver in python
  * load(code) : loads the clauses contained in the code string
  * ask(code) : returns the result of the query contained in the code string
  * variables(n) : convenience function to create multiple variables in one statement
  * clear() : resets the datalog database
  * create_atoms() : creates atoms for in-line queries
  * variables() : creates variables for in-line queries
Variable : a class to define Variable, for use in datalog queries. (defined in pyParser)
Answer class defines the object returned by ask()
  * __eq__ for equality test
  * __str__ for printing
classes for Python Mixin:
  * Mixin : a class that can be mixed in another class to give it datalog capability
  * sqlMetaMixin : a metaclass to be used when creating a class with both SQLAlchemy and datalog capability
  
"""
from collections import defaultdict
import inspect
import string
import weakref

try:
    from . import version
    from . import pyEngine
    from . import pyParser
    from . import util
except ValueError:
    import version
    import pyEngine
    import pyParser
    import util
    
print("pyDatalog version %s" % version.__version__)

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
DatalogError= util.DatalogError

def assert_fact(predicate_name, *args):
    """ assert predicate_name(args) """
    + pyParser.Literal.make(predicate_name, [pyParser.Expression._for(arg) for arg in args])

def retract_fact(predicate_name, *args):
    """ retracts predicate_name(args) """
    - pyParser.Literal.make(predicate_name, [pyParser.Expression._for(arg) for arg in args])

def program():
    """ A decorator for datalog program  """
    return pyParser.add_program

def predicate():
    """decorator function to create a predicate resolver in python"""
    return _predicate 

def _predicate(func):
    arity = len(inspect.getargspec(func)[0])
    pyEngine.Python_resolvers[func.__name__ + '/' + str(arity)] = func
    return func

def load(code):
    """loads the clauses contained in the code string """
    return pyParser.load(code)

def ask(code):
    """returns the result of the query contained in the code string"""
    return pyParser.ask(code)

def clear():
    """ resets the default datalog database """
    pyEngine.clear()

def create_atoms(*args):
    """ create atoms for in-line queries """
    stack = inspect.stack()
    try:
        locals_ = stack[1][0].f_locals
        args = [arg.split(',') for arg in args]
        args = [arg.strip() for argl in args for arg in argl]
        for arg in set(args + ['_sum','sum_','_min','min_','_max','max_',
        '_len','len_','concat','concat_','rank','rank_','running_sum','running_sum_',
        'range_','tuple_']):
            if arg in locals_: 
                if not isinstance(locals_[arg], (pyParser.Symbol, pyParser.Variable)):
                    raise util.DatalogError("Name conflict.  Can't redefine %s as atom" % arg, None, None)
            else:
                if arg[0] not in string.ascii_uppercase:
                    locals_[arg] = pyParser.Symbol(arg)
                else:
                    locals_[arg] = pyParser.Variable(arg)    
    finally:
        del stack

def variables(n):
    """ create variables for in-line queries """
    return [pyParser.Variable() for i in range(n)]

Variable = pyParser.Variable
Answer = pyParser.Answer


""" ****************** python Mixin ***************** """

#Keep a dictionary of classes with datalog capabilities.  
#This list is used by pyEngine to resolve prefixed literals.
Class_dict = {}
pyEngine.Class_dict = Class_dict # TODO better way to share it with pyEngine.py ?

class metaMixin(type):
    """Metaclass used to define the behavior of a subclass of Mixin"""
    __refs__ = defaultdict(weakref.WeakSet)
    
    def __init__(cls, name, bases, dct):
        """when creating a subclass of Mixin, save the subclass in Class_dict. """
        super(metaMixin, cls).__init__(name, bases, dct)
        Class_dict[name]=cls
        cls.has_SQLAlchemy = any(base.__module__ in ('sqlalchemy.ext.declarative', 
                            'sqlalchemy.ext.declarative.api') for base in bases)
        
        def _getattr(self, attribute):
            """ responds to instance.method by asking datalog engine """
            if not attribute == '__iter__' and not attribute.startswith('_sa_'):
                predicate_name = "%s.%s[1]==" % (self.__class__.__name__, attribute)
                literal = pyParser.Literal.make(predicate_name, (self, pyParser.Symbol("X")))
                result = literal.lua.ask()
                return result[0][-1] if result else None                    
            raise AttributeError
        cls.__getattr__ = _getattr   
    
    def __getattr__(cls, method):
        """
        when access to an attribute of a subclass of Mixin fails, 
        return an object that responds to () and to [] 
        """
        if cls in ('Mixin', 'metaMixin') or method in (
                '__mapper_cls__', '_decl_class_registry', '__sa_instrumentation_manager__', 
                '_sa_instance_state', '_sa_decl_prepare', '__table_cls__', '_pyD_query'):
            raise AttributeError        return pyParser.Symbol("%s.%s" % (cls.__name__, method))

    def pyDatalog_search(cls, literal):
        """Called by pyEngine to resolve a prefixed literal for a subclass of Mixin."""
        terms = literal.terms
        attr_name = literal.pred.suffix
        operator = (literal.pred.name.split(']')[1:2]+[None])[0] # what's after ']' or None
        
        # TODO check prearity
        def check_attribute(X):
            if attr_name not in X.__dict__ and attr_name not in cls.__dict__:
                raise AttributeError("%s does not have %s attribute" % (cls.__name__, attr_name))

        if len(terms)==2:
            X, Y = terms[0], terms[1]
            if X.is_constant:
                # try accessing the attribute of the first term in literal
                check_attribute(X.id)
                Y1 = getattr(X.id, attr_name)
                if not Y.is_constant or not operator or pyEngine.compare(Y1,operator,Y.id):
                    yield (X.id, Y.id if Y.is_constant else Y1 if operator=='==' else None)
            elif cls.has_SQLAlchemy:
                if cls.session:
                    q = cls.session.query(cls)
                    check_attribute(cls)
                    if Y.is_constant:
                        q = q.filter(pyEngine.compare(getattr(cls, attr_name), operator, Y.id))
                    for r in q:
                        Y1 = getattr(r, attr_name)
                        if not Y.is_constant or not operator or pyEngine.compare(Y1,operator,Y.id):
                                yield (r, Y.id if Y.is_constant else Y1 if operator=='==' else None)
            else:
                # python object with Mixin
                for X in metaMixin.__refs__[cls]:
                    check_attribute(X)
                    Y1 = getattr(X, attr_name)
                    if not Y.is_constant or not operator or pyEngine.compare(Y1,operator,Y.id):
                        yield (X, Y.id if Y.is_constant else Y1 if operator=='==' else None)
            return
        else:
            raise AttributeError ("%s could not be resolved" % literal.pred.name)

# following syntax to declare Mixin is used for compatibility with python 2 and 3
Mixin = metaMixin('Mixin', (object,), {})

#When creating a Mixin object without SQLAlchemy, add it to the list of instances,
#so that it can be included in the result of queries

def __init__(self):
    if not self.__class__.has_SQLAlchemy:
        for cls in self.__class__.__mro__:
            if cls.__name__ in Class_dict and cls not in (Mixin, object):
                metaMixin.__refs__[cls].add(self)
Mixin.__init__ = __init__

""" ****************** support for SQLAlchemy ***************** """

class sqlMetaMixin(metaMixin, DeclarativeMeta): 
    """ metaclass to be used with Mixin for SQLAlchemy"""
    pass

""" attach a method to SQLAlchemy class.attribute, 
    so that it can answer queries like class.attribute[X]==Y"""
def InstrumentedAttribute_getitem(self, *args):
    cls = self.class_
    method = self.key
    return type(cls).__getattr__(cls, method).__getitem__(*args)
InstrumentedAttribute.__getitem__ = InstrumentedAttribute_getitem
