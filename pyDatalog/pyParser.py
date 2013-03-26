"""
pyDatalog

Copyright (C) 2012 Pierre Carbonnelle
Copyright (C) 2004 Shai Berger

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
Design principle:
Instead of writing our own parser, we use python's parser.  

The base objects are Symbol and VarSymbol : they represent atoms and variables respectively
and have methods to implement the datalog language.

Methods exposed by this file include:
* load(code)
* add_program(func)
* ask(code)

For these methods, the datalog code is first compiled in python byte code, 
then undefined variables are initialized as instance of Symbol, 
then the code is finally executed to load the clauses.
This is done in the load() and add_program() method of Parser class.

Classes hierarchy contained in this file: see class diagram on http://bit.ly/YRnMPH
* ProgramContext : class to safely differentiate between In-line queries and pyDatalog program / ask(), using ProgramMode global variable
* _transform_ast : performs some modifications of the abstract syntax tree of the datalog program
* LazyList : a subclassable list that is populated when it is accessed. Mixin for pyDatalog.Variable.
    * LazyListOfList : Mixin for Query and Body
* Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
    * HeadLiteral
    * Query
* Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
* Expression : base class for objects that can be part of an inequality, operation or slice
    * VarSymbol : represents the symbol of a variable. Mixin for pyDatalog.Variable
        * Symbol : contains a constant, a variable or a predicate. Instantiated before executing the datalog program
    * Function : represents f[X]
    * Operation : made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program
* Aggregate : represents calls to aggregation method, e.g. min(X)
    * Sum_aggregate
    * Len_aggregate
    * Concat_aggregate
    * Min_aggregate
    * Max_aggregate
    * Rank_aggregate
    * Running_sum
"""

import ast
from collections import defaultdict, OrderedDict
import inspect
import os
import re
import string
import six
from six.moves import builtins, xrange
import sys
import weakref

PY3 = sys.version_info[0] == 3
func_code = '__code__' if PY3 else 'func_code'

try:
    from . import pyEngine
    from . import UserList
except ValueError:
    import pyEngine
    import UserList
pyDatalog = None #circ: later set by pyDatalog to avoid circular import

# global variable to differentiate between in-line queries and pyDatalog program / ask
ProgramMode = False

class ProgramContext(object):
    """class to safely use ProgramMode within the "with" statement"""
    def __enter__(self):
        global ProgramMode
        ProgramMode = True
    def __exit__(self, exc_type, exc_value, traceback):
        global ProgramMode
        ProgramMode = False
 
"""                             Parser methods                                                   """

def add_symbols(names, variables):
    """ add the names to the variables dictionary"""
    for name in names:
        variables[name] = Symbol(name)            
    
class _transform_ast(ast.NodeTransformer):
    """ does some transformation of the Abstract Syntax Tree of the datalog program """
    def visit_Call(self, node):
        """rename builtins to allow customization"""
        self.generic_visit(node)
        if hasattr(node.func, 'id'):
            node.func.id = '_sum' if node.func.id == 'sum' else node.func.id
            node.func.id = '_len' if node.func.id == 'len' else node.func.id
            node.func.id = '_min' if node.func.id == 'min' else node.func.id
            node.func.id = '_max' if node.func.id == 'max' else node.func.id
        return node
    
    def visit_Compare(self, node):
        """ rename 'in' to allow customization of (X in (1,2))"""
        self.generic_visit(node)
        if 1 < len(node.comparators): 
            raise pyDatalog.DatalogError("Syntax error: please verify parenthesis around (in)equalities", node.lineno, None) 
        if not isinstance(node.ops[0], (ast.In, ast.NotIn)): return node
        var = node.left # X, an _ast.Name object
        comparators = node.comparators[0] # (1,2), an _ast.Tuple object
        newNode = ast.Call(
                ast.Attribute(var, '_in' if isinstance(node.ops[0], ast.In) else '_not_in', var.ctx), # func
                [comparators], # args
                [], # keywords
                None, # starargs
                None # kwargs
                )
        return ast.fix_missing_locations(newNode)

def load(code, newglobals=None, defined=None, function='load'):
    """ code : a string or list of string 
        newglobals : global variables for executing the code
        defined : reserved symbols
    """
    newglobals, defined = newglobals or {}, defined or set([])
    # remove indentation based on first non-blank line
    lines = code.splitlines() if isinstance(code, six.string_types) else code
    r = re.compile('^\s*')
    for line in lines:
        spaces = r.match(line).group()
        if spaces and line != spaces:
            break
    code = '\n'.join([line.replace(spaces,'') for line in lines])
    
    tree = ast.parse(code, function, 'exec')
    try:
        tree = _transform_ast().visit(tree)
    except pyDatalog.DatalogError as e:
        e.function = function
        e.message = e.value
        e.value = "%s\n%s" % (e.value, lines[e.lineno-1])
        six.reraise(*sys.exc_info())
    code = compile(tree, function, 'exec')

    defined = defined.union(dir(builtins))
    defined.add('None')
    for name in set(code.co_names).difference(defined): # for names that are not defined
        add_symbols((name,), newglobals)
    try:
        with ProgramContext():
            six.exec_(code, newglobals)
    except pyDatalog.DatalogError as e:
        e.function = function
        traceback = sys.exc_info()[2]
        e.lineno = 1
        while True:
            if traceback.tb_frame.f_code.co_name == '<module>':
                e.lineno = traceback.tb_lineno
                break
            elif traceback.tb_next:
                traceback = traceback.tb_next 
        e.message = e.value
        e.value = "%s\n%s" % (e.value, lines[e.lineno-1])
        six.reraise(*sys.exc_info())
        
class _NoCallFunction(object):
    """ This class prevents a call to a datalog program created using the 'program' decorator """
    def __call__(self):
        raise TypeError("Datalog programs are not callable")

def add_program(func):
    """ A helper for decorator implementation   """
    source_code = inspect.getsource(func)
    lines = source_code.splitlines()
    # drop the first 2 lines (@pydatalog and def _() )
    if '@' in lines[0]: del lines[0]
    if 'def' in lines[0]: del lines[0]
    source_code = lines

    try:
        code = func.__code__
    except:
        raise TypeError("function or method argument expected")
    newglobals = func.__globals__.copy() if PY3 else func.func_globals.copy()
    func_name = func.__name__ if PY3 else func.func_name
    defined = set(code.co_varnames).union(set(newglobals.keys())) # local variables and global variables

    load(source_code, newglobals, defined, function=func_name)
    return _NoCallFunction()

def ask(code, _fast=None):
    """ runs the query in the code string """
    with ProgramContext():
        tree = ast.parse(code, 'ask', 'eval')
        tree = _transform_ast().visit(tree)
        code = compile(tree, 'ask', 'eval')
        newglobals = {}
        add_symbols(code.co_names, newglobals)
        parsed_code = eval(code, newglobals)
        return pyDatalog.Answer.make(parsed_code.ask())

"""                             Parser classes                                                   """

class LazyList(UserList.UserList):
    """a subclassable list that is populated when it is accessed """
    """used by Literal, Body, pyDatalog.Variable to delay evaluation of datalog queries written in python  """
    """ during debugging, beware that viewing a Lazylist will force its udpate""" 
    def __init__(self):
        self.todo = None # self.todo.ask() calculates self.data
        self._data = []
    
    @property
    def data(self):
        """ returns the list, after recalculation if needed """
        if self.todo is not None: self.todo.ask()
        return self._data

    def _value(self): # for backward compatibility ?
        return self.data
    
    def v(self):
        """ returns the first value in the list, or None """
        return self._data[0] if self.data else None

class LazyListOfList(LazyList):
    """ represents the result of an inline query (a Literal or Body)"""
    def __eq__(self, other):
        """ uses set comparison"""
        return set(self.data) == set(other)
    
    def __ge__(self, variable):
        """ returns the first value of the variable in the result of a query, or None """
        if self.data:
            assert isinstance(variable, pyDatalog.Variable)
            for t in self.literal().terms:
                if id(t) == id(variable):
                    return t.data[0]
    
class Expression(object):
    """ base class for objects that can be part of an inequality, operation or slice """
    @classmethod
    def _for(cls, operand):
        """ factory that converts an operand to an Expression """
        if isinstance(operand, (Expression, Aggregate)):
            return operand
        if isinstance(operand, type(lambda: None)):
            return Operation(None, operand, [Symbol(var) for var in getattr(operand,func_code).co_varnames])
        return Symbol(operand, forced_type="constant")
    
    def _precalculation(self):
        """ by default, there is no precalculation needed to evaluate an expression """
        return Body()
    
    # handlers of inequality and operations
    def __eq__(self, other):
        if isinstance(self, Operation) and self.operator in '+-' and self.lhs._pyD_value == 0:
            assert False, "Did you mean to assert or retract a fact ? Please add parenthesis."
        return Literal.make_for_comparison(self, "==", other)
    def __ne__(self, other):
        return Literal.make_for_comparison(self, '!=', other)
    def __le__(self, other):
        return Literal.make_for_comparison(self, '<=', other)
    def __lt__(self, other):
        return Literal.make_for_comparison(self, '<', other)
    def __ge__(self, other):
        return Literal.make_for_comparison(self, '>=', other)
    def __gt__(self, other):
        return Literal.make_for_comparison(self, '>', other)
    def _in(self, values):
        """ called when evaluating (X in (1,2)) """
        return Literal.make_for_comparison(self, '_pyD_in', values)
    def _not_in(self, values):
        """ called when evaluating (X not in (1,2)) """
        return Literal.make_for_comparison(self, '_pyD_not_in', values)
    
    def __pos__(self):
        """ called when evaluating -X """
        return 0 + self
    def __neg__(self):
        """ called when evaluating -X """
        return 0 - self

    def __add__(self, other):
        return Operation(self, '+', other)
    def __sub__(self, other):
        return Operation(self, '-', other)
    def __mul__(self, other):
        return Operation(self, '*', other)
    def __div__(self, other):
        return Operation(self, '/', other)
    def __truediv__(self, other):
        return Operation(self, '/', other)
    def __floordiv__(self, other):
        return Operation(self, '//', other)
    
    # called by constant + Symbol (or lambda + symbol)
    def __radd__(self, other):
        return Operation(other, '+', self)
    def __rsub__(self, other):
        return Operation(other, '-', self)
    def __rmul__(self, other):
        return Operation(other, '*', self)
    def __rdiv__(self, other):
        return Operation(other, '/', self)
    def __rtruediv__(self, other):
        return Operation(self, '/', other)
    def __rfloordiv__(self, other):
        return Operation(other, '//', self)

    def __getitem__(self, keys):
        """ called when evaluating expression[keys] """
        if isinstance(keys, slice):
            return Operation(self, 'slice', [keys.start, keys.stop, keys.step])
        return Operation(self, 'slice', keys)
    
    
class VarSymbol(Expression):
    """ represents the symbol of a variable, both inline and in pyDatalog program """
    def __init__ (self, name, forced_type=None):
        self._pyD_negated = False # for aggregate with sort in descending order
        if isinstance(name, (list, tuple, xrange)):
            self._pyD_value = list(map(Expression._for, name))
            self._pyD_name = str([element._pyD_name for element in self._pyD_value])
            self._pyD_type = 'tuple'
            self._pyD_lua = pyEngine.Interned.of([e._pyD_lua for e in self._pyD_value])
        elif isinstance(name, slice):
            start, stop, step = map(Expression._for, (name.start, name.stop, name.step))
            self._pyD_value = (start, stop, step)
            self._pyD_name = '[%s:%s:%s]' % (start._pyD_name, stop._pyD_name, step._pyD_name)
            self._pyD_type = 'slice'
            self._pyD_lua = pyEngine.Interned.of([start._pyD_lua, stop._pyD_lua, step._pyD_lua])
        elif forced_type=="constant" or isinstance(name, (int, float)) or not name or name[0] not in string.ascii_uppercase + '_':
            self._pyD_value = name
            self._pyD_name = str(name)
            self._pyD_type = 'constant'
            self._pyD_lua = pyEngine.Const(name)
        else:
            self._pyD_value = name
            self._pyD_name = name
            self._pyD_type = 'variable'
            self._pyD_lua = pyEngine.Var(name)

    def __neg__(self):
        """ called when evaluating -X. Used in aggregate arguments """
        neg = Symbol(self._pyD_value)
        neg._pyD_negated = True

        expr = 0 - self
        expr.variable = neg
        return expr
    
    def _variables(self):
        """ returns an ordered dictionary of the variables in the varSymbol """
        if self._pyD_type == 'variable' and not self._pyD_name.startswith('_pyD_'):
            return OrderedDict({self._pyD_name : self})
        elif self._pyD_type == 'tuple':
            variables = OrderedDict()
            for element in self._pyD_value:
                variables.update(element._variables())
            return variables
        elif self._pyD_type == 'slice':
            variables = OrderedDict()
            variables.update(self._pyD_value[0]._variables())
            variables.update(self._pyD_value[1]._variables())
            variables.update(self._pyD_value[2]._variables())
            return variables
        else:
            return OrderedDict()

def pre_calculations(args):
    """ returns a new list of args, and pre_calculations"""
    # logic shared by Symbol and Function 
    new_args, pre_calculations = [], Body()
    for arg in args:
        if isinstance(arg, Operation):
            pre_calculations = pre_calculations & arg._precalculation()
            new_args.append(arg)
        elif isinstance(arg, (Operation, Function)):
            Y = Function.newSymbol()
            new_args.append(Y)
            pre_calculations = pre_calculations & (Y == arg)
        else:
            new_args.append(arg)
    return new_args, pre_calculations
        
class Symbol(VarSymbol):
    """
    can be constant, list, tuple, variable or predicate name
    ask() creates a query
    """
    def __call__ (self, *args, **kwargs):
        """ called when evaluating p(args) """
        if self._pyD_name == 'ask': # call ask() and return an answer
            if 1<len(args):
                raise RuntimeError('Too many arguments for ask !')
            fast = kwargs['_fast'] if '_fast' in list(kwargs.keys()) else False
            return pyDatalog.Answer.make(args[0].ask())
        
        # manage the aggregate functions
        elif self._pyD_name == '_sum':
            if isinstance(args[0], VarSymbol):
                return Sum_aggregate(args[0], for_each=kwargs.get('for_each', kwargs.get('key', [])))
            else:
                return sum(args)
        elif self._pyD_name == 'concat':
            return Concat_aggregate(args[0], order_by=kwargs.get('order_by',kwargs.get('key', [])), sep=kwargs['sep'])
        elif self._pyD_name == '_min':
            if isinstance(args[0], VarSymbol):
                return Min_aggregate(args[0], order_by=kwargs.get('order_by',kwargs.get('key', [])),)
            else:
                return min(args)
        elif self._pyD_name == '_max':
            if isinstance(args[0], VarSymbol):
                return Max_aggregate(args[0], order_by=kwargs.get('order_by',kwargs.get('key', [])),)
            else:
                return max(args)
        elif self._pyD_name == 'rank':
            return Rank_aggregate(None, for_each=kwargs.get('for_each', []), order_by=kwargs.get('order_by', []))
        elif self._pyD_name == 'running_sum':
            return Running_sum(args[0], for_each=kwargs.get('for_each', []), order_by=kwargs.get('order_by', []))
        elif self._pyD_name == '_len':
            if isinstance(args[0], VarSymbol):
                return Len_aggregate(args[0])
            else: 
                return len(args[0]) 
        else: # create a literal
            new_args, pre_calc = pre_calculations(args)
            literal = Literal.make(self._pyD_name, tuple(new_args))
            literal.pre_calculations = pre_calc
            return literal

    def __getattr__(self, name):
        """ called when evaluating class.attribute """
        return Symbol(self._pyD_name + '.' + name)
    
    def __getitem__(self, keys):
        """ called when evaluating name[keys] """
        return Function(self._pyD_name, keys)

    def __str__(self):
        return str(self._pyD_name)
    
    def __setitem__(self, keys, value):
        """  called when evaluating f[X] = expression """
        function = Function(self._pyD_name, keys)
        # following statement translates it into (f[X]==V) <= (V==expression)
        (function == function.symbol) <= (function.symbol == value)
        
class Function(Expression):
    """ represents predicate[a, b]"""
    Counter = 0 # counter of functions evaluated so far
    @classmethod
    def newSymbol(cls):
        """ returns a new unique Symbol associated to a Function """
        Function.Counter += 1
        return Symbol('_pyD_X%i' % Function.Counter)
        
    def __init__(self, name, keys):
        if not isinstance(keys, tuple):
            keys = (keys,)
        self.name = "%s[%i]" % (name, len(keys))
        self.keys, self.pre_calculations = pre_calculations(keys)
                
        self.symbol = Function.newSymbol()
        self.dummy_variable_name = '_pyD_X%i' % Function.Counter
        self._pyD_lua = self.symbol._pyD_lua
    
    @property
    def _pyD_name(self):
        return str(self)
    
    def __eq__(self, other):
        return Literal.make_for_comparison(self, '==', other)
    
    # following methods are used when the function is used in an expression
    def _variables(self):
        """ returns an ordered dictionary of the variables in the keys of the function"""
        return self.pre_calculations._variables()

    def _precalculation(self):
        """ returns the literal(s) that must be pre-calculated before the Literal that uses this function""" 
        return self.pre_calculations & (self == self.symbol)
    
class Operation(Expression):
    """created when evaluating an operation (+, -, *, /, //) """
    def __init__(self, lhs, operator, rhs):
        self.operator = operator
        self.lhs = Expression._for(lhs) # left  hand side
        self.rhs = Expression._for(rhs)
        self._pyD_lua = pyEngine.Operation(self.lhs._pyD_lua, self.operator, self.rhs._pyD_lua)
        
    @property
    def _pyD_name(self):
        return str(self)
    
    def _variables(self):
        """ returns an ordered dictionary of the variables in this Operation"""
        temp = self.lhs._variables()
        temp.update(self.rhs._variables())
        return temp
    
    def _precalculation(self):
        """ returns the literal(s) that must be pre-calculated before the Literal that uses this Operation""" 
        return self.lhs._precalculation() & self.rhs._precalculation()
    
    def __str__(self):
        return '(' + str(self.lhs._pyD_name) + self.operator + str(self.rhs._pyD_name) + ')'

class Literal(object):
    """
    created by source code like 'p(a, b)'
    operator '<=' means 'is true if', and creates a Clause
    """
    def __init__(self, predicate_name, args, prearity=None, aggregate=None):
        self.predicate_name = predicate_name
        self.prearity = prearity or len(args)
        self.pre_calculations = Body()
        
        self.args = args
        self.todo = self
        cls_name = predicate_name.split('.')[0].replace('~','') if 1< len(predicate_name.split('.')) else ''
        self.terms = [] # the list of args converted to Expression
        for i, arg in enumerate(args):
            if isinstance(arg, Literal):
                raise pyDatalog.DatalogError("Syntax error: Literals cannot have a literal as argument : %s%s" % (predicate_name, self.terms), None, None)
            elif not isinstance(arg, VarSymbol) and i==0 and cls_name and cls_name not in [c.__name__ for c in arg.__class__.__mro__]:
                raise TypeError("Object is incompatible with the class that is queried.")
            elif isinstance(arg, Aggregate):
                raise pyDatalog.DatalogError("Syntax error: Incorrect use of aggregation.", None, None)
            if isinstance(arg, pyDatalog.Variable):
                arg.todo = self
                del arg._data[:] # reset the variable. For use in in-line queries
            self.terms.append(Expression._for(arg))
                            
        tbl = [a._pyD_lua for a in self.terms]
        # now create the literal for the head of a clause
        self.lua = pyEngine.Literal(predicate_name, tbl, prearity, aggregate)
        # TODO check that l.pred.aggregate is empty

    @classmethod
    def make(cls, predicate_name, terms, prearity=None, aggregate=None):
        """ factory class that creates a Query or HeadLiteral """
        if predicate_name[-1]=='!': #pred e.g. aggregation literal
            return HeadLiteral(predicate_name, terms, prearity, aggregate)
        else:
            return Query(predicate_name, terms, prearity, aggregate)
    
    @classmethod
    def make_for_comparison(cls, self, operator, other):
        """ factory of Literal for a comparison """
        assert operator=="==" or not isinstance(other, Aggregate), "Aggregate operators can only be used with =="
        other = Expression._for(other)
        if isinstance(self, Function):
            if isinstance(other, Aggregate): # p[X]==aggregate() # TODO create 2 literals here
                if operator != '==':
                    raise pyDatalog.DatalogError("Aggregate operator can only be used with equality.", None, None)
                name, terms, prearity = (self.name + '==', list(self.keys) + [other], len(self.keys))
                
                # 1 create literal for queries
                terms[-1] = (Symbol('X')) # (X, X)
                l = Literal.make(name, terms, prearity, other)
                pyDatalog.add_clause(l, l) # body will be ignored, but is needed to make the clause safe

                # 2 prepare literal for the calculation. It can be used in the head only
                del terms[-1] # --> (X,)
                terms.extend(other.args)
                prearity = len(terms) # (X,Y,Z)
                return Literal.make(name + '!', terms, prearity=prearity) #pred
            elif operator != '==' or isinstance(other, (Operation, Function)):
                if '.' not in self.name: # p[X]<Y+Z transformed into (p[X]=Y1) & (Y1<Y+Z)
                    literal = Literal.make(self.name+'==', list(self.keys)+[self.symbol], prearity=len(self.keys))
                    return literal & pyEngine.compare2(self.symbol, operator, other)
                elif isinstance(other, (Operation, Function)): # a.p[X]<Y+Z transformed into (Y2==Y+Z) & (a.p[X]<Y2)
                    Y2 = Function.newSymbol()
                    return (Y2 == other) & Literal.make(self.name + operator, list(self.keys) + [Y2], prearity=len(self.keys))
            return Literal.make(self.name + operator, list(self.keys) + [other], prearity=len(self.keys))
        else:
            if not isinstance(other, Expression):
                raise pyDatalog.DatalogError("Syntax error: Symbol or Expression expected", None, None)
            literal = Literal.make(operator, [self] + [other])
            literal.pre_calculations = self._precalculation() & other._precalculation()
            return literal

    @property
    def literals(self):
        return [self]
    
    def _variables(self):
        """ returns an ordered dictionary of the variables in the Literal"""
        if self.predicate_name[0] == '~': #pred ignore variables of negated literals
            return OrderedDict()
        variables = OrderedDict()
        for term in self.terms:
            variables.update(term._variables())
        return variables
    
    def __le__(self, body):
        " head <= body creates a clause"
        if not isinstance(body, (Literal, Body)):
            raise pyDatalog.DatalogError("Invalid body for clause", None, None)
        newBody = Body()
        for literal in body.literals:
            if isinstance(literal, HeadLiteral):
                raise pyDatalog.DatalogError("Aggregation cannot appear in the body of a clause", None, None)
            newBody = newBody & literal.pre_calculations & literal
        result = pyDatalog.add_clause(self, newBody)
        if not result: 
            raise pyDatalog.DatalogError("Can't create clause", None, None)
        return result

class HeadLiteral(Literal):
    """ represents literals that can be used only in head of clauses, i.e. literals with aggregate function"""
    pass

class Query(Literal, LazyListOfList):
    """
    represents a literal that can be queried (thus excludes aggregate literals)
    unary operator '+' means insert it as fact
    binary operator '&' means 'and', and returns a Body
    """
    def __init__(self, predicate_name, terms, prearity=None, aggregate=None):
        LazyListOfList.__init__(self)
        Literal.__init__(self, predicate_name, terms, prearity, aggregate)
        
    def ask(self):
        self._data = Body(self.pre_calculations, self).ask()
        self.todo = None
        return self._data

    def __pos__(self):
        " unary + means insert into database as fact "
        assert not self._variables(), "Cannot assert a fact containing Variables"
        pyDatalog._assert_fact(self)

    def __neg__(self):
        " unary - means retract fact from database "
        assert not self._variables(), "Cannot retract a fact containing Variables"
        pyDatalog._retract_fact(self)
        
    def __invert__(self):
        """unary ~ means negation """
        # TODO test with python queries
        return Literal.make('~' + self.predicate_name, self.terms) #pred

    def __and__(self, other):
        " literal & literal" 
        return Body(self, other)

    def __str__(self):
        if ProgramMode:
            terms = list(map (str, self.terms))
            return str(self.predicate_name) + "(" + ','.join(terms) + ")"
        else:
            return LazyListOfList.__str__(self)
    
    def __eq__(self, other):
        if ProgramMode:
            raise pyDatalog.DatalogError("Syntax error near equality: consider using brackets. %s" % str(self), None, None)
        else:
            return super(Literal, self).__eq__(other)

    def literal(self):
        return self

class Body(LazyListOfList):
    """ created by p(a,b) & q(c,d)  """
    Counter = 0
    def __init__(self, *args):
        LazyListOfList.__init__(self)
        self.literals = []
        for arg in args:
            self.literals += [arg] if isinstance(arg, Literal) else arg.literals
            
        env = OrderedDict()
        for literal in self.literals:
            for term in literal._variables().values():
                env[term._pyD_name] = term
        self.__variables = env
        
        self.todo = self
        for variable in env.values():
            variable.todo = self

    def _variables(self):
        return self.__variables

    def __and__(self, body2):
        """ operator '&' means 'and', and returns a Body """
        return Body(self, body2)
    
    def __str__(self):
        if self._variables().values(): # an in-line query --> evaluate it
            return LazyListOfList.__str__(self)
        return ' & '.join(list(map (str, self.literals)))

    def literal(self, permanent=False):
        """ return a literal that can be queried to resolve the body """
        if permanent:
            literal = Literal.make('_pyD_query' + str(Body.Counter), list(self._variables().values()))
            Body.Counter = Body.Counter + 1
        else:
            literal = Literal.make('_pyD_query', list(self._variables().values()))
            literal.lua.pred.reset_clauses()
        literal <= self
        return literal
        
    def __invert__(self):
        """unary ~ means negation """
        return ~(self.literal(permanent=True))

    def ask(self):
        """ resolve the query and determine the values of its variables"""
        literal = self.literal()
        self._data = literal.lua.ask(False)
        literal.todo, self.todo = None, None
        if not ProgramMode and self._data: 
            transposed = list(zip(*(self._data))) # transpose result
            result = []
            for i, arg in enumerate(literal.terms):
                if isinstance(arg, pyDatalog.Variable) and len(arg._data)==0:
                    arg._data.extend(transposed[i])
                    arg.todo = None
                    result.append(transposed[i])
            self._data = list(zip(*result)) if result else [()]
        return self._data

##################################### Aggregation #####################################
    
class Aggregate(object):
    """ 
    represents a generic aggregation_method(X, for_each=Y, order_by=Z, sep=sep)
    e.g. 'sum(Y,key=Z)' in '(a[X]==sum(Y,key=Z))'
    pyEngine calls sort_result(), key(), reset(), add() and fact() to compute the aggregate
    """
    
    def __init__(self, Y=None, for_each=tuple(), order_by=tuple(), sep=None):
        # convert for_each=Z to for_each=(Z,)
        self.Y = Y
        self.for_each = (for_each,) if isinstance(for_each, Expression) else tuple(for_each)
        self.order_by = (order_by,) if isinstance(order_by, Expression) else tuple(order_by)
        
        # try to recast expressions to variables
        self.for_each = tuple([e.__dict__.get('variable', e) for e in self.for_each]) 
        self.order_by = tuple([e.__dict__.get('variable', e) for e in self.order_by])
        
        assert all([isinstance(e, VarSymbol) for e in self.for_each]), "for_each argument of aggregate must be variable(s), not expression(s)."
        assert all([isinstance(e, VarSymbol) for e in self.order_by]), "order_by argument of aggregate must be variable(s), not expression(s)."
        
        if sep and not isinstance(sep, six.string_types):
            raise pyDatalog.DatalogError("Separator in aggregation must be a string", None, None)
        self.sep = sep
        
        # verify presence of keyword arguments
        for kw in self.required_kw:
            arg = getattr(self, kw)
            if arg is None or (isinstance(arg, tuple) and arg == tuple()):
                raise pyDatalog.DatalogError("Error: argument missing in aggregate", None, None)
        
        # used to create literal. TODO : filter on symbols
        self.args = ((Y,) if Y is not None else tuple()) + self.for_each + self.order_by + ((sep,) if sep is not None else tuple())
        self.Y_arity = 1 if Y is not None else 0
        self.sep_arity = 1 if sep is not None else 0
        
    @property
    def arity(self):
        """returns the arity of the aggregate function, not of the full predicate """
        return len(self.args)
        
    def sort_result(self, result):
        """ sort result according to the aggregate argument """
        # significant indexes in the result rows
        order_by_start = len(result[0]) - len(self.order_by) - self.sep_arity
        for_each_start = order_by_start - len(self.for_each)
        self.to_add = for_each_start-1
        
        self.slice_for_each = slice(for_each_start, order_by_start)
        self.reversed_order_by = range(len(result[0])-1-self.sep_arity, order_by_start-1,  -1)
        self.slice_group_by = slice(0, for_each_start-self.Y_arity)
        # first sort per order_by, allowing for _pyD_negated
        for i in self.reversed_order_by:
            result.sort(key=lambda literal, i=i: literal[i].id,
                reverse = self.order_by[i-order_by_start]._pyD_negated)
        # then sort per group_by
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[self.slice_group_by]])
        pass
    
    def key(self, result):
        """ return the grouping key of a result """
        return list(result[:len(result)-self.arity])
    
    def reset(self):
        """ by default, _value is 0 """
        self._value = 0
        
    @property
    def value(self):
        """ by default, value is _value"""
        return self._value
    
    def fact(self, key):
        """ returns the terms of an aggregated fact"""
        return key + [pyEngine.Const(self.value)]
       
class Sum_aggregate(Aggregate):
    """ represents sum(Y, for_each=(Z,T))"""
    required_kw = ('Y', 'for_each')

    def add(self, row):
        self._value += row[-self.arity].id
        
class Len_aggregate(Aggregate):
    """ represents len(X)"""
    required_kw = ('Y')

    def add(self, row):
        self._value += 1

class Concat_aggregate(Aggregate):
    """ represents concat(Y, order_by=(Z1,Z2), sep=sep)"""
    required_kw = ('Y', 'order_by', 'sep')
        
    def reset(self):
        self._value = []
        
    def add(self, row):
        self._value.append(row[-self.arity].id)
        
    @property
    def value(self):
        return self.sep.join(self._value)

class Min_aggregate(Aggregate):
    """ represents min(Y, order_by=(Z,T))"""
    required_kw = ('Y', 'order_by')

    def reset(self):
        self._value = None
        
    def add(self, row):
        self._value = row[-self.arity].id if self._value is None else self._value

class Max_aggregate(Min_aggregate):
    """ represents max(Y, order_by=(Z,T))"""
    def __init__(self, *args, **kwargs):
        Min_aggregate.__init__(self, *args, **kwargs)
        for a in self.order_by:
            a._pyD_negated = not(a._pyD_negated)

class Rank_aggregate(Aggregate):
    """ represents rank(for_each=(Z), order_by(T))"""
    required_kw = ('for_each', 'order_by')
    
    def reset(self):
        self.count = 0
        self._value = None

    def add(self, row):
        # retain the value if (X,) == (Z,)
        if row[self.slice_group_by] == row[self.slice_for_each]:
            self._value = list(row[self.slice_group_by]) + [pyEngine.Const(self.count),]
            return self._value
        self.count += 1
        
    def fact(self, k):
        return self._value

class Running_sum(Rank_aggregate):
    """ represents running_sum(Y, for_each=(Z), order_by(T)"""
    required_kw = ('Y', 'for_each', 'order_by')
    
    def add(self,row):
        self.count += row[self.to_add].id # TODO
        if row[:self.to_add] == row[self.slice_for_each]:
            self._value = list(row[:self.to_add]) + [pyEngine.Const(self.count),]
            return self._value