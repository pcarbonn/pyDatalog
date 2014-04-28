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
  * create_terms() : creates terms for in-line queries
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

from . import version
from . import Logic
from . import pyEngine
from . import pyParser
from . import util
    
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
    + pyParser.Literal.make(predicate_name, [pyParser.Expression._pyD_for(arg) for arg in args])

def retract_fact(predicate_name, *args):
    """ retracts predicate_name(args) """
    - pyParser.Literal.make(predicate_name, [pyParser.Expression._pyD_for(arg) for arg in args])

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
    pyParser.clear()
    Logic()

class Classe(object):
    def __init__(self, cls):
        self.cls = cls
    def __call__(self, *arguments, **keyargs):
        for a in arguments: 
            if isinstance(a, pyParser.Expression):
                assert not keyargs, "Sorry, key word arguments are not supported yet" #TODO
                return pyParser.Operation(self.cls, '(', arguments) 
        return self.cls(*arguments, **keyargs)
        

def _pyD_decorator(arg):
    if hasattr(arg, '_pyD_atomized'): 
        return arg
    atomized = arg
    if inspect.isclass(arg):
        atomized = Classe(arg)
        for c in arg.__mro__[-2::-1]: # reverse order, ignoring object class
            for a in c.__dict__:
                try:
                    new_f = _pyD_decorator(getattr(arg, a))
                    atomized.__dict__[a] = new_f
                except AttributeError:
                    pass # sometimes raised by pypy, e.g. for time
    elif inspect.ismodule(arg):
        for a in arg.__dict__:
            new_f = _pyD_decorator(getattr(arg, a))
            setattr(arg, a, new_f)
    elif hasattr(arg, '__call__'): # it's a function
        if inspect.isgeneratorfunction(arg):
            #TODO support atomized generator functions
            atomized = arg
        else:
            def atomized(*arguments, **keyargs):
                # if any argument is an Expression, return an Operation
                # else immediately evaluate the function
                # TODO give it arg's name ?
                for a in arguments: 
                    if isinstance(a, pyParser.Expression):
                        assert not keyargs, "Sorry, key word arguments are not supported yet" #TODO
                        return pyParser.Operation(arg, '(', arguments) 
                return arg(*arguments, **keyargs)
        
            try: # copy __doc__
                atomized.__dict__.update(arg.__dict__)
            except:
                pass
    try:
        setattr(atomized, '_pyD_atomized', True)
    except:
        pass
    return atomized

ATOMS = ['_sum','sum_','_min','min_','_max','max_', '_len','len_','concat','concat_','rank','rank_',
         'running_sum','running_sum_','range_','tuple_', 'format_']

def create_terms(*args):
    """ create terms for in-line clauses and queries """
    stack = inspect.stack()
    try:
        locals_ = stack[1][0].f_locals
        args = [arg.strip() for arglist in args for arg in 
                (arglist.split(',') if isinstance(arglist, util.string_types) else [arglist])]
        for arg in set(args + ATOMS):
            assert isinstance(arg, util.string_types)
            words = arg.split('.')
            if 2<len(words): #TODO deal with more
                    raise util.DatalogError("Too many '.' in atom %s" % arg, None, None)
            b = __builtins__ if isinstance(__builtins__, dict) else __builtins__.__dict__ # for pypy
            if words[0] in b: # if it 's a builtin
                root = b[words[0]]
                locals_[words[0]] = _pyD_decorator(root) 
            elif words[0] in locals_:
                root = locals_[words[0]]
                if len(words)==2: # e.g. str.split
                    atom = getattr(root, words[1])
                    setattr(root, words[1], _pyD_decorator(atom))
                else: # e.g. math
                    locals_[arg] = _pyD_decorator(root)
            else:
                if len(words)==2: # e.g. kkecxivarenx.len
                    raise util.DatalogError("Unknown variable : %s" % words[0], None, None)
                locals_[arg] = pyParser.Term(arg)
    finally:
        del stack

create_atoms = create_terms # for backward compatibility

def variables(n):
    """ create variables for in-line clauses and queries """
    return [pyParser.Term('??') for i in range(n)]

Variable = pyParser.Term
Answer = pyParser.Answer


""" ****************** python Mixin ***************** """

class metaMixin(type):
    """Metaclass used to define the behavior of a subclass of Mixin"""
    __refs__ = defaultdict(weakref.WeakSet)
    
    def __init__(cls, name, bases, dct):
        """when creating a subclass of Mixin, save the subclass in Class_dict. """
        super(metaMixin, cls).__init__(name, bases, dct)
        pyEngine.add_class(cls, name)
        cls.has_SQLAlchemy = any(base.__module__ in ('sqlalchemy.ext.declarative', 
                            'sqlalchemy.ext.declarative.api') for base in bases)
        
        def _getattr(self, attribute):
            """ responds to instance.method by asking datalog engine """
            if not attribute == '__iter__' and not attribute.startswith('_sa_'):
                predicate_name = "%s.%s[1]==" % (self.__class__.__name__, attribute)
                terms = (pyParser.Term('_pyD_class', forced_type='constant'), self, pyParser.Term("X")) #prefixed
                literal = pyParser.Literal.make(predicate_name, terms) #TODO predicate_name[:-2]
                result = literal.lua.ask()
                return result[0][-1] if result else None                    
            raise AttributeError
        cls.__getattr__ = _getattr   

        def __lt__(self, other): # needed for sorting in aggregate functions using Python 3
            return id(self) < id(other)
        cls.__lt__ = __lt__    
    
    def __getattr__(cls, method):
        """
        when access to an attribute of a subclass of Mixin fails, 
        return an object that responds to () and to [] 
        """
        if cls in ('Mixin', 'metaMixin') or method in (
                '__mapper_cls__', '_decl_class_registry', '__sa_instrumentation_manager__', 
                '_sa_instance_state', '_sa_decl_prepare', '__table_cls__', '_pyD_query'):
            raise AttributeError        return pyParser.Term("%s.%s" % (cls.__name__, method))

    def pyDatalog_search(cls, literal):
        """Called by pyEngine to resolve a prefixed literal for a subclass of Mixin."""
        terms = literal.terms
        attr_name = literal.pred.suffix
        operator = literal.pred.name.split(']')[1] # what's after ']' or None
        
        # TODO check prearity
        def check_attribute(X):
            if attr_name not in X.__dict__ and attr_name not in cls.__dict__:
                raise AttributeError("%s does not have %s attribute" % (cls.__name__, attr_name))

        if len(terms)==3: #prefixed
            X, Y = terms[1], terms[2]
            if X.is_constant:
                # try accessing the attribute of the first term in literal
                check_attribute(X.id)
                Y1 = getattr(X.id, attr_name)
                if not Y.is_constant or not operator or pyEngine.compare(Y1,operator,Y.id):
                    yield (terms[0], X.id, Y.id if Y.is_constant else Y1 if operator=='==' else None)
            elif cls.has_SQLAlchemy:
                if cls.session:
                    q = cls.session.query(cls)
                    check_attribute(cls)
                    if Y.is_constant:
                        q = q.filter(pyEngine.compare(getattr(cls, attr_name), operator, Y.id))
                    for r in q:
                        Y1 = getattr(r, attr_name)
                        if not Y.is_constant or not operator or pyEngine.compare(Y1,operator,Y.id):
                                yield (terms[0], r, Y.id if Y.is_constant else Y1 if operator=='==' else None)
            else:
                # python object with Mixin
                for X in metaMixin.__refs__[cls]:
                    check_attribute(X)
                    Y1 = getattr(X, attr_name)
                    if not Y.is_constant or not operator or pyEngine.compare(Y1,operator,Y.id):
                        yield (terms[0], X, Y.id if Y.is_constant else Y1 if operator=='==' else None)
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
            if cls.__name__ in pyEngine.Class_dict and cls not in (Mixin, object):
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
