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
Instead of writing our own parser, we use python's parser.  The datalog code is first compiled in python byte code, 
then "undefined" variables are initialized as instance of Symbol, then the code is finally executed to load the clauses.
This is done in the load() and add_program() method of Parser class.

Methods exposed by this file:
* load(code)
* add_program(func)
* ask(code)

Classes hierarchy contained in this file:
* ProgramContext : class to safely differentiate between In-line queries and pyDatalog program / ask(), using ProgramMode global variable
* _transform_ast : performs some modifications of the abstract syntax tree of the datalog program
* LazyList : a subclassable list that is populated when it is accessed. Mixin for pyDatalog.Variable.
    * LazyListOfList : Mixin for Literal and Body
        * Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
        * Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
* Expression : base class for objects that can be part of an inequality or operation
    * VarSymbol : represents the symbol of a variable. Mixin for pyDatalog.Variable
        * Symbol : contains a constant, a variable or a predicate. Instantiated before executing the datalog program
    * Function : represents f[X]
    * Operation : made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program
    * Lambda : represents a lambda function, used in expressions
* Aggregate : represents calls to aggregation method, e.g. min(X)

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
except ValueError:
    import pyEngine
pyDatalog = None #circ: later set by pyDatalog to avoid circular import

""" global variable to differentiate between in-line queries and pyDatalog program / ask"""
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
    for name in names:
        variables[name] = Symbol(name)            
    
class _transform_ast(ast.NodeTransformer):
    """ does some transformation of the Abstract Syntax Tree of the datalog program """
    def visit_Call(self, node):
        """rename builtins to allow customization"""
        self.generic_visit(node)
        if hasattr(node.func, 'id'):
            node.func.id = '__sum__' if node.func.id == 'sum' else node.func.id
            node.func.id = '__len__' if node.func.id == 'len' else node.func.id
            node.func.id = '__min__' if node.func.id == 'min' else node.func.id
            node.func.id = '__max__' if node.func.id == 'max' else node.func.id
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

def load(code, newglobals={}, defined=set([]), function='load'):
    """ code : a string or list of string 
        newglobals : global variables for executing the code
        defined : reserved symbols
    """
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
    with ProgramContext():
        tree = ast.parse(code, 'ask', 'eval')
        tree = _transform_ast().visit(tree)
        code = compile(tree, 'ask', 'eval')
        newglobals = {}
        add_symbols(code.co_names, newglobals)
        parsed_code = eval(code, newglobals)
        parsed_code = parsed_code.literal() if isinstance(parsed_code, Body) else parsed_code
        return pyEngine.toAnswer(parsed_code.lua, parsed_code.lua.ask(_fast))

"""                             Parser classes                                                   """

class LazyList(object):
    """a subclassable list that is populated when it is accessed """
    """used by Literal, Body, pyDatalog.Variable to delay evaluation of datalog queries written in python  """
    """ during debugging, beware that viewing a Lazylist will force its udpate""" 
    def __init__(self):
        self.todo = None # self.todo.ask() calculates self._list
        self._list = []
    def _value(self):
        # returns the list, after recalculation if needed
        if self.todo is not None: self.todo.ask()
        return self._list

    def __iter__(self):
        return iter(self._value())
    def __len__(self):
        return len(self._value())
    def __getitem__(self, item):
        return list.__getitem__(self._value(), item)
    def __str__(self): 
        return str(self._value())
    def __repr__(self):
        return repr(self._value())
    def __reversed__(self): # dead code
        return reversed(self._value())
    def __nonzero__(self):
        return bool(self._value())
    def __eq__(self, other): # dead code
        return self._value() == other
    def v(self):
        return self._list[0] if self._value() else None

class LazyListOfList(LazyList):
    """ represents the result of an inline query (a Literal or Body)"""
    def __eq__(self, other):
        return set(self._value()) == set(other)
    
    def __ge__(self, other):
        # returns the first occurrence of 'other' variable in the result of a function
        if self._value():
            assert isinstance(other, pyDatalog.Variable)
            for t in self.literal().args:
                if id(t) == id(other):
                    return t._list[0]
    
class Expression(object):
    def _precalculation(self):
        return Body() # by default, there is no precalculation needed to evaluate an expression
    
    def __eq__(self, other):
        if self._pyD_type == 'variable' and not isinstance(other, Symbol):
            return self._make_expression_literal('==', other)
        else:
            return Literal("=", (self, other))
    def __ne__(self, other):
        return self._make_expression_literal('!=', other)
    def __le__(self, other):
        return self._make_expression_literal('<=', other)
    def __lt__(self, other):
        return self._make_expression_literal('<', other)
    def __ge__(self, other):
        return self._make_expression_literal('>=', other)
    def __gt__(self, other):
        return self._make_expression_literal('>', other)
    def _in(self, values):
        """ called when compiling (X in (1,2)) """
        return self._make_expression_literal('in', values)
    def _not_in(self, values):
        """ called when compiling (X not in (1,2)) """
        return self._make_expression_literal('not in', values)
    
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
    
class VarSymbol(Expression):
    """ represents the symbol of a variable, both inline and in pyDatalog program 
    """
    def __init__ (self, name):
        self._pyD_name = name
        self._pyD_negated = False # for aggregate with sort in descending order
        if isinstance(name, (int, list, tuple, xrange)) or not name or name[0] not in string.ascii_uppercase + '_':
            self._pyD_type = 'constant'
            self._pyD_lua = pyEngine.Const(name)
        else:
            self._pyD_type = 'variable'
            self._pyD_lua = pyEngine.Var(name)
        
    def _make_expression_literal(self, operator, other):
        """private function to create a literal for comparisons"""
        if isinstance(other, type(lambda: None)):
            other = Lambda(other)
        name = '=' + str(self) + operator + str(other)
        if other is None or isinstance(other, (int, six.string_types, list, tuple, xrange)):
            literal = Literal(name, [self])
            expr = pyEngine.make_operand('constant', other)
        else: 
            if not isinstance(other, (Symbol, Expression)):
                raise pyDatalog.DatalogError("Syntax error: Symbol or Expression expected", None, None)
            literal = Literal(name, [self] + list(other._variables().values()))
            expr = other.lua_expr(list(self._variables().keys())+list(other._variables().keys()))
            literal.pre_calculations = other._precalculation()
        pyEngine.add_expr_to_predicate(literal.lua.pred, operator, expr)
        return literal

    def __neg__(self):
        """ called when compiling -X """
        neg = Symbol(self._pyD_name)
        neg._pyD_negated = True
        return neg
    
    def lua_expr(self, variables):
        if self._pyD_type == 'variable':
            return pyEngine.make_operand('variable', variables.index(self._pyD_name))
        else:
            return pyEngine.make_operand('constant', self._pyD_name)
    
    def _variables(self):
        if self._pyD_type == 'variable':
            return OrderedDict({self._pyD_name : self})
        else:
            return OrderedDict()

class Symbol(VarSymbol):
    """
    can be constant, variable or predicate name
    ask() creates a query
    created when analysing the datalog program
    """
    def __call__ (self, *args, **kwargs):
        """ called when compiling p(args) """
        "time to create a literal !"
        def check(kwargs, template):
            for arg in template:
                if not [kw for kw in arg if kw in kwargs]:
                    raise pyDatalog.DatalogError("Error: argument missing in aggregate", None, None)
        if self._pyD_name == 'ask':
            if 1<len(args):
                raise RuntimeError('Too many arguments for ask !')
            fast = kwargs['_fast'] if '_fast' in list(kwargs.keys()) else False
            literal = args[0] if not isinstance(args[0], Body) else args[0].literal()
            return pyEngine.toAnswer(literal.lua, literal.lua.ask(fast))
        elif self._pyD_name == '__sum__':
            if isinstance(args[0], Symbol):
                check(kwargs, (('key', 'for_each'),))
                return Sum_aggregate((args[0], kwargs.get('for_each', kwargs.get('key'))))
            else:
                return sum(args)
        elif self._pyD_name == 'concat':
            check(kwargs, (('key','order_by'),('sep',)))
            return Concat_aggregate((args[0], kwargs.get('order_by',kwargs.get('key')), kwargs['sep']))
        elif self._pyD_name == '__min__':
            if isinstance(args[0], Symbol):
                check(kwargs, (('key', 'order_by'),))
                return Min_aggregate((args[0], kwargs.get('order_by',kwargs.get('key')),))
            else:
                return min(args)
        elif self._pyD_name == '__max__':
            if isinstance(args[0], Symbol):
                check(kwargs, (('key', 'order_by'),))
                return Max_aggregate((args[0], kwargs.get('order_by',kwargs.get('key')),))
            else:
                return max(args)
        elif self._pyD_name == 'rank':
            return Rank_aggregate((kwargs['for_each'], kwargs['order_by']))
        elif self._pyD_name == 'running_sum':
            return Running_sum((args[0], kwargs['for_each'], kwargs['order_by']))
        elif self._pyD_name == '__len__':
            if isinstance(args[0], Symbol):
                return Len_aggregate(args[0])
            else: 
                return len(args[0]) 
        else:
            new_args, pre_calculations = [], Body()
            for arg in args:
                if isinstance(arg, (Operation, Function, Lambda)):
                    Y = Function.newSymbol()
                    new_args.append(Y)
                    pre_calculations = pre_calculations & (Y == arg)
                else:
                    new_args.append(arg)
            literal = Literal(self._pyD_name, tuple(new_args))
            literal.pre_calculations = pre_calculations
            return literal

    def __getattr__(self, name):
        """ called when compiling class.attribute """
        return Symbol(self._pyD_name + '.' + name)
    
    def __getitem__(self, keys):
        """ called when compiling name[keys] """
        return Function(self._pyD_name, keys)

    def __str__(self):
        return str(self._pyD_name)
    
    def __setitem__(self, keys, value):
        """  called when compiling f[X] = expression """
        function = Function(self._pyD_name, keys)
        # following statement translates it into (f[X]==V) <= (V==expression)
        (function == function.symbol) <= (function.symbol == value)
        
class Function(Expression):
    """ represents predicate[a, b]"""
    Counter = 0
    @classmethod
    def newSymbol(cls):
        Function.Counter += 1
        return Symbol('_pyD_X%i' % Function.Counter)
        
    def __init__(self, name, keys):
        if not isinstance(keys, tuple):
            keys = (keys,)
        self.name = "%s[%i]" % (name, len(keys))
        self.keys, self.pre_calculations = [], Body()
        for key in keys:
            if isinstance(key, (Operation, Function, Lambda)):
                Y = Function.newSymbol()
                self.keys.append(Y)
                self.pre_calculations = self.pre_calculations & (Y == key)
            else:
                self.keys.append(key)
                
        self.symbol = Function.newSymbol()
        self.dummy_variable_name = '_pyD_X%i' % Function.Counter
        
    def _make_expression_literal(self, operator, other):
        if isinstance(other, type(lambda: None)):
            other = Lambda(other)
        assert operator=="==" or not isinstance(other, Aggregate), "Aggregate operators can only be used with =="
        if operator == '==' and not isinstance(other, (Operation, Function, Lambda)): # p[X]==Y # TODO use positive list of class
            return Literal(self.name + '==', list(self.keys) + [other], prearity=len(self.keys))
        literal = Literal(self.name+'==', list(self.keys)+[self.symbol], prearity=len(self.keys))
        if '.' not in self.name: # p[X]<Y+Z transformed into (p[X]=Y1) & (Y1<Y+Z)
            return literal & pyEngine.compare2(self.symbol, operator, other)
        elif isinstance(other, (Operation, Function, Lambda)): # a.p[X]<Y+Z transformed into (Y2==Y+Z) & (a.p[X]<Y2)
            Y2 = Function.newSymbol()
            return (Y2 == other) & Literal(self.name + operator, list(self.keys) + [Y2], prearity=len(self.keys))
        else: 
            return Literal(self.name + operator, list(self.keys) + [other], prearity=len(self.keys))
    
    def __eq__(self, other):
        return self._make_expression_literal('==', other)
    
    def __pos__(self):
        raise pyDatalog.DatalogError("bad operand type for unary +: 'Function'. Please consider adding parenthesis", None, None)
    
    def __neg__(self):
        raise pyDatalog.DatalogError("bad operand type for unary -: 'Function'. Please consider adding parenthesis", None, None)
    
    # following methods are used when the function is used in an expression
    def _variables(self):
        return {self.dummy_variable_name : self.symbol}

    def lua_expr(self, variables):
        return pyEngine.make_operand('variable', variables.index(self.dummy_variable_name))

    def _precalculation(self): 
        return self.pre_calculations & (self == self.symbol)
    
class Operation(Expression):
    """made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program"""
    def __init__(self, lhs, operator, rhs):
        self.operator = operator
        
        def _convert(operand):
            if operand is None or isinstance(operand, (six.string_types, int, list, tuple, xrange)):
                return Symbol(operand)
            elif isinstance(operand, type(lambda: None)):
                return Lambda(operand)
            return operand
        
        self.lhs = _convert(lhs)
        self.rhs = _convert(rhs)
        
    def _variables(self):
        temp = self.lhs._variables()
        temp.update(self.rhs._variables())
        return temp
    
    def _precalculation(self):
        return self.lhs._precalculation() & self.rhs._precalculation()
    
    def lua_expr(self, variables):
        return pyEngine.make_expression(self.operator, self.lhs.lua_expr(variables), self.rhs.lua_expr(variables))
    
    def __str__(self):
        return '(' + str(self.lhs) + self.operator + str(self.rhs) + ')'

class Lambda(Expression):
    """represents a lambda function, used in expressions"""
    def __init__(self, other):
        self.operator = '<lambda>'
        self.lambda_object = other
        
    def _variables(self):
        return dict([ [var, Symbol(var)] for var in getattr(self.lambda_object,func_code).co_varnames])
    
    def lua_expr(self, variables):
        operands = [pyEngine.make_operand('variable', variables.index(varname)) for varname in getattr(self.lambda_object,func_code).co_varnames] 
        return pyEngine.make_lambda(self.lambda_object, operands)
    
    def __str__(self):
        return 'lambda%i(%s)' % (id(self.lambda_object), ','.join(getattr(self.lambda_object,func_code).co_varnames))
        
class Literal(LazyListOfList):
    """
    created by source code like 'p(a, b)'
    unary operator '+' means insert it as fact
    binary operator '&' means 'and', and returns a Body
    operator '<=' means 'is true if', and creates a Clause
    """
    def __init__(self, predicate_name, terms, prearity=None, aggregate=None):
        LazyListOfList.__init__(self)
        self.predicate_name = predicate_name
        self.prearity = prearity or len(terms)
        self.pre_calculations = Body()
        
        # TODO cleanup by redifining self.args, .HasSymbols, .hasVariables, .prefix
        if not ProgramMode: # in-line, thus
            self.todo = self
            self.args = terms
            cls = self.predicate_name.split('.')[0] if 1< len(self.predicate_name.split('.')) else ''
            terms, env = [], {}
            for i, arg in enumerate(self.args):
                if isinstance(arg, pyDatalog.Variable):
                    arg.todo = self
                    del arg._list[:] # reset variables
                    # deal with (X,X)
                    variable = env.get(id(arg), Symbol('X%i' % id(arg)))
                    env[id(arg)] = variable
                    terms.append(variable)
                elif isinstance(arg, Symbol):
                    terms.append(arg)
                elif i==0 and cls and arg.__class__.__name__ != cls.replace('~',''):
                    raise TypeError("Object is incompatible with the class that is queried.")
                else:
                    terms.append(arg)
        else:
            self.args = []
        self.terms = terms
                            
        # adjust head literal for aggregate
        h_terms = list(terms)
        if terms and self.prearity != len(terms) and isinstance(terms[-1], Aggregate):
            # example : a[X] = sum(Y, key = Z)
            self.aggregate = terms[-1] # sum(Y, key=Z)
            h_predicate_name = predicate_name + '!'
            # compute list of terms
            del h_terms[-1] # --> (X,)
            base_terms = list(h_terms) # creates a copy
            # OK to use any variable to represent the aggregate value
            base_terms.append(Symbol('X')) # (X, X)

            for arg in self.aggregate.args:
                if isinstance(arg, Symbol):
                    h_terms.append(arg)
                else:
                    h_terms.extend(arg)
            h_prearity = len(h_terms) # (X,Y,Z)
            
            # create the second predicate # TODO use Pred() instead
            l = Literal(predicate_name, base_terms, prearity, self.aggregate)
            pyDatalog.add_clause(l, l) # body will be ignored, but is needed to make the clause safe
            # TODO check that l.pred.aggregate_method is correct
        else:
            self.aggregate = None
            h_predicate_name = predicate_name
            h_prearity = prearity
        
        tbl = []
        for a in h_terms:
            if isinstance(a, Symbol):
                tbl.append(a._pyD_lua)
            elif isinstance(a, Literal):
                raise pyDatalog.DatalogError("Syntax error: Literals cannot have a literal as argument : %s%s" % (predicate_name, terms), None, None)
            elif isinstance(a, Aggregate):
                raise pyDatalog.DatalogError("Syntax error: Incorrect use of aggregation.", None, None)
            else:
                tbl.append(pyEngine.Const(a))
        # now create the literal for the head of a clause
        self.lua = pyEngine.Literal(h_predicate_name, tbl, h_prearity, aggregate)
        # TODO check that l.pred.aggregate is empty

    def ask(self):
        self._list = self.lua.ask(False)
        self.todo = None
        if self.args and self._list: 
            transposed = list(zip(*(self._list))) # transpose result
            result = []
            for i, arg in enumerate(self.args):
                if isinstance(arg, pyDatalog.Variable) and len(arg._list)==0:
                    arg._list.extend(transposed[i])
                    arg.todo = None
                    result.append(transposed[i])
            self._list = list(zip(*result)) if result else [()]

    def __pos__(self):
        " unary + means insert into database as fact "
        global ProgramMode
        assert ProgramMode, "'+' cannot be used to assert facts in python programs"
        pyDatalog._assert_fact(self)

    def __neg__(self):
        " unary - means retract fact from database "
        assert not self.args # '-' cannot be used with literal containing pyDatalog.Variable instances
        pyDatalog._retract_fact(self)
        
    def __invert__(self):
        """unary ~ means negation """
        # TODO test with python queries
        return Literal('~' + self.predicate_name, self.terms)

    def __le__(self, body):
        " head <= body"
        global ProgramMode
        #TODO assert ProgramMode # '<=' cannot be used with literal containing pyDatalog.Variable instances
        if isinstance(body, Literal):
            newBody = body.pre_calculations & body
        else:
            if not isinstance(body, Body):
                raise pyDatalog.DatalogError("Invalid body for clause", None, None)
            newBody = Body()
            for literal in body.literals:
                newBody = newBody & literal.pre_calculations & literal
        result = pyDatalog.add_clause(self, newBody)
        if not result: 
            raise pyDatalog.DatalogError("Can't create clause", None, None)
        return result

    def __and__(self, other):
        " literal & literal" 
        return Body(self, other)

    def __str__(self):
        if not self.args:
            terms = list(map (str, self.terms))
            return str(self.predicate_name) + "(" + ','.join(terms) + ")"
        else:
            return LazyListOfList.__str__(self)
    
    def __eq__(self, other):
        if not self.args:
            raise pyDatalog.DatalogError("Syntax error near equality: consider using brackets. %s" % str(self), None, None)
        else:
            return super(Literal, self).__eq__(other)

    def literal(self):
        return self

class Body(LazyListOfList):
    """
    created by p(a,b) & q(c,d)
    operator '&' means 'and', and returns a Body
    """
    Counter = 0
    def __init__(self, *args):
        LazyListOfList.__init__(self)
        self.literals = []
        for arg in args:
            self.literals += [arg] if isinstance(arg, Literal) else arg.literals
        self.hasVariables = False
        for literal in self.literals:
            if hasattr(literal, 'args'):
                self.hasVariables = True
                self.todo = self
                for arg in literal.args:
                    if isinstance(arg, pyDatalog.Variable):
                        arg.todo = self

    def __and__(self, body2):
        if not (isinstance(body2, Body) or not body2.aggregate):
            raise pyDatalog.DatalogError("Aggregation cannot appear in the body of a clause", None, None)
        return Body(self, body2)
    
    def __str__(self):
        if self.hasVariables:
            return LazyListOfList.__str__(self)
        return ' & '.join(list(map (str, self.literals)))

    def literal(self, permanent=False):
        # return a literal that can be queried to resolve the body
        env, args = OrderedDict(), []
        for literal in self.literals:
            for term in literal.terms:
                if isinstance(term, Symbol) and term._pyD_type == 'variable':
                    env[term._pyD_name] = term
            for arg in literal.args:
                if isinstance(arg, pyDatalog.Variable):
                    args.append(arg)
        # TODO cleanup : use args instead of env.values() ?
        if permanent:
            literal = Literal('_pyD_query' + str(Body.Counter), list(env.values()))
            Body.Counter = Body.Counter + 1
        else:
            literal = Literal('_pyD_query', list(env.values()))
            literal.lua.pred.reset_clauses()
        literal <= self
        literal.args = args
        return literal 
        
    def __invert__(self):
        """unary ~ means negation """
        return ~(self.literal(permanent=True))

    def ask(self):
        literal = self.literal()
        literal.ask()
        self._list = literal._list
    
class Aggregate(object):
    """ represents aggregation_method(X,Y), e.g. 'sum(Y,key=Z)' in '(a[X]==sum(Y,key=Z))'"""
    """ provide defaults methods that may need to be overriden for some aggregation methods"""
    """ pyEngine calls sort_result(), key(), reset(), add() and fact() to compute the aggregate"""
    # TODO refactor aggregate to reduce repetition
    def __init__(self, args):
        # correct for method(Y), method(Y,key=(Z,..)) 
        if isinstance(args, Symbol): # len(Y), for example
            args = (args,) # (Y,)
        elif 1 < len(args) and isinstance(args[1], Symbol): # correct only for method(Y,key=Z)
            # make sure that 2nd argument is iterable
            args = list(args)
            args[1] = (args[1],) # [Y, (Z,)]
        self.args = args
    
    @property
    def arity(self): 
        # correct only for aggregation_method(Y, key=(Z,...))
        return 1 + len(self.args[1])
        
    def sort_result(self, result):
        # default sort order : X,Y,Z for a[X] = agg(Y,key=Z)
        result.sort(key=lambda literal: [id(term) for term in literal])
    
    def key(self, result):
        # return the grouping key of a result
        # default is X1,X2 for a[X1,X2] = agg(Y,key=Z)
        return list(result[:len(result)-self.arity])
    
    def reset(self):
        self._value = 0
        
    @property
    def value(self):
        return self._value
    
    def fact(self,k):
        return k + [pyEngine.Const(self.value)]
       
class Sum_aggregate(Aggregate):
    """ represents sum(X, key=(Y,Z))"""
    def add(self, row):
        self._value += row[-self.arity].id
        
class Len_aggregate(Aggregate):
    """ represents len(X)"""
    @property
    def arity(self):
        return 1
        
    def add(self, row):
        self._value += 1

class Concat_aggregate(Aggregate):
    """ represents concat(Y, key=(Z1,Z2), sep=sep)"""
    @property
    def arity(self):
        return 2 + len(self.args[1])
        
    def sort_result(self, result):
        # first sort per Z1,Z2
        for i in range(len(self.args[1])):
            result.sort(key=lambda literal, i=i: literal[-i-2].id, # -2 because of separator
                reverse = self.args[1][-i-1]._pyD_negated)
        # then sort per X1,X2
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[:len(result)-self.arity]])
    
    def reset(self):
        self._value = []
        
    def add(self, row):
        self._value.append(row[-self.arity].id)
        
    @property
    def value(self):
        return self.args[2].join(self._value)

class Min_aggregate(Aggregate):
    """ represents min(X, key=(Y,Z))"""
    def sort_result(self, result):
        # first sort per Z1,Z2
        for i in range(len(self.args[1])):
            result.sort(key=lambda literal, i=i: literal[-i-1].id,
                reverse = self.args[1][-i-1]._pyD_negated)
        # then sort per X1,X2
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[:len(result)-self.arity]])
        pass
    
    def reset(self):
        self._value = None
        
    def add(self, row):
        self._value = row[-self.arity].id if self._value is None else self._value

class Max_aggregate(Min_aggregate):
    """ represents max(X, key=(Y,Z))"""
    def __init__(self, args):
        Min_aggregate.__init__(self, args)
        for a in self.args[1]:
            a._pyD_negated = not(a._pyD_negated)

class Rank_aggregate(Aggregate):
    """ represents rank(for_each=(Z), order_by(T))"""
    def __init__(self, args):
        args = list(args)
        if 0 < len(args) and isinstance(args[0], Symbol):
            args[0] = (args[0],)
        if 1 < len(args) and isinstance(args[1], Symbol):
            args[1] = (args[1],)
        self.args = args
    
    @property
    def arity(self):
        return len(self.args[0]) + len(self.args[1])

    def sort_result(self, result):
        # significant indexes in the result rows
        order_by_start = len(result[0]) - len(self.args[1])
        for_each_start = order_by_start - len(self.args[0])
        
        self.for_each = slice(for_each_start, order_by_start)
        self.reversed_order_by = range(len(result[0])-1, order_by_start-1,  -1)
        self.group_by = slice(0, for_each_start)
        # first sort per order_by
        for i in self.reversed_order_by:
            result.sort(key=lambda literal, i=i: literal[i].id,
                reverse = self.args[1][i-order_by_start]._pyD_negated)
        # then sort per group_by
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[self.group_by]])
        pass

    def reset(self):
        self.count = 0
        self._value = None

    def add(self, row):
        # retain the value if (X,) == (Z,)
        if row[self.group_by] == row[self.for_each]:
            self._value = list(row[self.group_by]) + [pyEngine.Const(self.count),]
            return self._value
        self.count += 1
        
    def fact(self, k):
        return self._value

class Running_sum(Rank_aggregate):
    """ represents running_sum(N, for_each=(Z), order_by(T)"""
    def __init__(self, args):
        args = list(args)
        if 1 < len(args) and isinstance(args[1], Symbol):
            args[1] = (args[1],)
        if 2 < len(args) and isinstance(args[2], Symbol):
            args[2] = (args[2],)
        self.args = args
    
    @property
    def arity(self):
        return 1+len(self.args[1]) + len(self.args[2])

    def sort_result(self, result):
        # significant indexes in the result rows
        order_by_start = len(result[0]) - len(self.args[2])
        for_each_start = order_by_start - len(self.args[1])
        self.to_add = for_each_start-1
        
        self.for_each = slice(for_each_start, order_by_start)
        self.reversed_order_by = range(len(result[0])-1, order_by_start-1,  -1)
        self.group_by = slice(0, self.to_add)
        # first sort per order_by
        for i in self.reversed_order_by:
            result.sort(key=lambda literal, i=i: literal[i].id,
                reverse = self.args[2][i-order_by_start]._pyD_negated)
        # then sort per group_by
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[self.group_by]])
        pass

    def add(self,row):
        self.count += row[self.to_add].id # TODO
        if row[:self.to_add] == row[self.for_each]:
            self._value = list(row[:self.to_add]) + [pyEngine.Const(self.count),]
            return self._value
        