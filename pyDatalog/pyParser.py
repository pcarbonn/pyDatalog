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

Classes contained in this file:
* _transform_ast : performs some modifications of the abstract syntax tree of the datalog program
* Expression : base class for objects that can be part of an inequality or operation
* Symbol : contains a constant, a variable or a predicate. Instantiated before executing the datalog program
* Operation : made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program
* Lambda : represents a lambda function, used in expressions
* Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
* Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
* Function : represents f[X]
* Aggregate : represents calls to aggregation method, e.g. min(X)

"""

import ast
from collections import defaultdict
import inspect
import os
import re
import string
import six
from six.moves import builtins
import sys
import weakref
    
PY3 = sys.version_info[0] == 3
func_code = '__code__' if PY3 else 'func_code'

try:
    from . import pyEngine
except ValueError:
    import pyEngine
pyDatalog = None #circ: later set by pyDatalog to avoid circular import
 
"""                             Parser methods                                                   """

def add_symbols(names, variables):
    for name in names:
        variables[name] = Symbol(name)            
    
class _transform_ast(ast.NodeTransformer):
    """ does some transformation of the Abstract Syntax Tree of the datalog program """
    def visit_Call(self, node):
        """rename builtins to allow customization"""
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
            raise SyntaxError("please add parenthesis around (in)equalities (line %i)." % node.lineno)
        if not isinstance(node.ops[0], ast.In): return node
        var = node.left # X, an _ast.Name object
        comparators = node.comparators[0] # (1,2), an _ast.Tuple object
        newNode = ast.Call(
                ast.Attribute(var, '_in', var.ctx), # func
                [comparators], # args
                [], # keywords
                None, # starargs
                None # kwargs
                )
        newNode = ast.fix_missing_locations(newNode)
        return newNode

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
    tree = _transform_ast().visit(tree)
    code = compile(tree, function, 'exec')

    defined = defined.union(dir(builtins))
    defined.add('None')
    for name in set(code.co_names).difference(defined): # for names that are not defined
        add_symbols((name,), newglobals)
    six.exec_(code, newglobals)
        
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
    if PY3:
        newglobals = func.__globals__.copy()
        func_name = func.__name__
    else:
        newglobals = func.func_globals.copy()
        func_name = func.func_name
    defined = set(code.co_varnames).union(set(newglobals.keys())) # local variables and global variables

    load(source_code, newglobals, defined, function=func_name)
    return _NoCallFunction()

def ask(code, _fast=None):
    tree = ast.parse(code, 'ask', 'eval')
    tree = _transform_ast().visit(tree)
    code = compile(tree, 'ask', 'eval')
    newglobals = {}
    add_symbols(code.co_names, newglobals)
    lua_code = eval(code, newglobals)
    return pyDatalog._ask_literal(lua_code, _fast)

"""                             Parser classes                                                   """

class Expression(object):
    def _precalculation(self):
        # by default, there is no precalculation needed to evaluate an expression
        return Body()
    
    def __eq__(self, other):
        if self.type == 'variable' and not isinstance(other, Symbol):
            return self._make_expression_literal('=', other)
        else:
            return Literal("=", (self, other))
    def __ne__(self, other):
        return self._make_expression_literal('~=', other)
    def __le__(self, other):
        return self._make_expression_literal('<=', other)
    def __lt__(self, other):
        return self._make_expression_literal('<', other)
    def __ge__(self, other):
        return self._make_expression_literal('>=', other)
    def __gt__(self, other):
        return self._make_expression_literal('>', other)
    
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
    def __rfloordiv__(self, other):
        return Operation(other, '//', self)
    
class Symbol(Expression):
    """
    can be constant, variable or predicate name
    ask() creates a query
    created when analysing the datalog program
    """
    def __init__ (self, name):
        self.name = name
        self.negated = False # for aggregate with sort in descending order
        if isinstance(name, int):
            self.type = 'constant'
        elif (name[0] not in string.ascii_uppercase):
            self.type = 'constant'
        else:
            self.type = 'variable'
        if self.type == 'variable':
            self.lua = pyEngine.make_var(name)
        else:
            self.lua = pyEngine.make_const(name)
        
    def __call__ (self, *args, **kwargs):
        """ called when compiling p(args) """
        "time to create a literal !"
        if self.name == 'ask':
            if 1<len(args):
                raise RuntimeError('Too many arguments for ask !')
            fast = kwargs['_fast'] if '_fast' in list(kwargs.keys()) else False
            return pyDatalog._ask_literal(args[0], fast)
        elif self.name == 'sum_foreach': # TODO remove
            args = (args[0], kwargs['key'])
            return Sum_aggregate(args)
        elif self.name == '__sum__':
            if isinstance(args[0], Symbol):
                args = (args[0], kwargs['key'])
                return Sum_aggregate(args)
            else:
                return sum(args)
        elif self.name == 'concat':
            args = (args[0], kwargs['key'], kwargs['sep'])
            return Concat_aggregate(args)
        elif self.name == '__min__':
            if isinstance(args[0], Symbol):
                args = (args[0], kwargs['key'],)
                return Min_aggregate(args)
            else:
                return min(args)
        elif self.name == '__max__':
            if isinstance(args[0], Symbol):
                args = (args[0], kwargs['key'],)
                return Max_aggregate(args)
            else:
                return max(args)
        elif self.name == 'rank':
            args = (kwargs['key'],)
            return Rank_aggregate(args)
        elif self.name == '__len__':
            if isinstance(args[0], Symbol):
                return Len_aggregate(args[0])
            else: 
                return len(args[0]) 
        else:
            return Literal(self.name, args)

    def _make_expression_literal(self, operator, other):
        """private function to create a literal for comparisons"""
        if isinstance(other, type(lambda: None)):
            other = Lambda(other)
        name = '=' + str(self) + operator + str(other)
        if isinstance(other, (int, six.string_types, list, tuple)):
            literal = Literal(name, [self])
            expr = pyEngine.make_operand('constant', other)
        else: 
            assert isinstance(other, (Symbol, Expression)), "Symbol or Expression expected"
            literal = Literal(name, [self] + list(other._variables().values()))
            expr = other.lua_expr(list(self._variables().keys())+list(other._variables().keys()))
            literal.pre_calculations = other._precalculation()
        pyEngine.add_expr_to_predicate(literal.lua.pred, operator, expr)
        return literal

    def __neg__(self):
        """ called when compiling -X """
        neg = Symbol(self.name)
        neg.negated = True
        return neg
    
    def _in(self, values):
        """ called when compiling (X in (1,2)) """
        return self._make_expression_literal('in', values)
    
    def __coerce__(self, other):
        return None
    
    def __getattr__(self, name):
        """ called when compiling class.attribute """
        return Symbol(self.name + '.' + name)
    
    def __getitem__(self, keys):
        """ called when compiling name[keys] """
        return Function(self.name, keys)
    
    def lua_expr(self, variables):
        if self.type == 'variable':
            return pyEngine.make_operand('variable', variables.index(self.name))
        else:
            return pyEngine.make_operand('constant', self.name)
    
    def _variables(self):
        if self.type == 'variable':
            return {self.name : self}
        else:
            return {}

    def __str__(self):
        return str(self.name)
    
    def __setitem__(self, keys, value):
        """  called when compiling f[X] = expression """
        function = Function(self.name, keys)
        # following statement translates it into (f[X]==V) <= (V==expression)
        (function == function.symbol) <= (function.symbol == value)

class Operation(Expression):
    """made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program"""
    def __init__(self, lhs, operator, rhs):
        self.operator = operator
        
        self.lhs = lhs
        if isinstance(lhs, six.string_types) or isinstance(lhs, int):
            self.lhs = Symbol(lhs)
        elif isinstance(lhs, type(lambda: None)):
            self.lhs = Lambda(lhs)
            
        self.rhs = rhs
        if isinstance(rhs, six.string_types) or isinstance(rhs, int):
            self.rhs = Symbol(rhs)
        elif isinstance(rhs, type(lambda: None)):
            self.rhs = Lambda(rhs)
        
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
        
class Literal(object):
    """
    created by source code like 'p(a, b)'
    unary operator '+' means insert it as fact
    binary operator '&' means 'and', and returns a Body
    operator '<=' means 'is true if', and creates a Clause
    """
    def __init__(self, predicate_name, terms, prearity=None, aggregate=None):
        self.predicate_name = predicate_name
        self.terms = terms
        self.prearity = prearity or len(terms)
        self.pre_calculations = Body()

        # adjust head literal for aggregate
        h_terms = list(terms)
        if isinstance(terms[-1], Aggregate):
            self.aggregate = terms[-1]
            h_predicate_name = predicate_name + '!'
            # compute list of terms
            del h_terms[-1]
            base_terms = list(h_terms) # creates a copy
            for arg in self.aggregate.args:
                if isinstance(arg, Symbol):
                    h_terms.append(arg)
                else:
                    h_terms.extend(arg)
            h_prearity = len(h_terms)
            base_terms.append(Symbol('X')) # OK to use any variable
            
            # create the second predicate # TODO use make_pred instead
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
                tbl.append(a.lua)
            elif isinstance(a, six.string_types):
                tbl.append(pyEngine.make_const(a))
            elif isinstance(a, Literal):
                raise SyntaxError("Literals cannot have a literal as argument : %s%s" % (predicate_name, terms))
            elif isinstance(a, Aggregate):
                raise TypeError("Incorrect use of '%s' aggregation." % a.method)
            else:
                tbl.append(pyEngine.make_const(a))
        # now create the literal for the head of a clause
        self.lua = pyEngine.make_literal(h_predicate_name, tbl, h_prearity, aggregate)
        # TODO check that l.pred.aggregate is empty

    def __pos__(self):
        " unary + means insert into database as fact "
        pyDatalog._assert_fact(self)

    def __neg__(self):
        " unary - means retract fact from database "
        pyDatalog._retract_fact(self)
        
    def __invert__(self):
        """unary ~ means negation """
        negated_literal = Literal('~' + self.predicate_name, self.terms)
        return negated_literal

    def __le__(self, body):
        " head <= body"
        if isinstance(body, Literal):
            newBody = body.pre_calculations & body
        else:
            assert isinstance(body, Body), "Invalid body for clause"
            newBody = Body()
            for literal in body.literals:
                newBody = newBody & literal.pre_calculations & literal
        result = pyDatalog.add_clause(self, newBody)
        if not result: 
            raise TypeError("Can't create clause %s <= %s" % (str(self), str(newBody)))

    def __and__(self, other):
        " literal & literal" 
        return Body(self, other)

    def __str__(self):
        terms = list(map (str, self.terms))
        return str(self.predicate_name) + "(" + ','.join(terms) + ")"

class Body(object):
    """
    created by p(a,b) & q(c,d)
    operator '&' means 'and', and returns a Body
    """
    def __init__(self, *args):
        self.literals = []
        for arg in args:
            self.literals += [arg] if isinstance(arg, Literal) else arg.literals

    def __and__(self, body2):
        assert isinstance(body2, Body) or not body2.aggregate, "Aggregation cannot appear in the body of a clause"
        return Body(self, body2)
    
    def __str__(self):
        literals = list(map (str, self.literals))
        return ' & '.join(literals)

class Function(Expression):
    """ represents predicate[a, b]"""
    Counter = 0
    def __init__(self, name, keys):
        if not isinstance(keys, tuple):
            keys = (keys,)
        self.name = "%s[%i]" % (name, len(keys))
        self.keys = keys
        self.dummy_variable_name = '_X%i' % Function.Counter
        Function.Counter += 1
        
        self.symbol = Symbol(self.dummy_variable_name)
        self.symbol.type = 'variable'
        self.symbol.lua = pyEngine.make_var(self.dummy_variable_name)
        
    def _variables(self):
        return {self.dummy_variable_name : self.symbol}

    def lua_expr(self, variables):
        return pyEngine.make_operand('variable', variables.index(self.dummy_variable_name))

    def __eq__(self, other):
        assert isinstance(other, (six.string_types, int, Symbol, Aggregate)), "The left hand side of a function literal must be a constant, variable"
        terms = list(self.keys)
        terms.append(other)
        l = Literal(self.name, terms, prearity=len(self.keys))
        return l
    
    def _precalculation(self):
        literal = (self == self.symbol)
        return Body(literal)

        
class Aggregate(object):
    """ represents aggregation_method(X,Y)"""
    def __init__(self, args):
        if isinstance(args, Symbol):
            args = (args,)
        # make sure that 2nd argument is iterable
        if 1 < len(args) and isinstance(args[1], Symbol):
            args = list(args)
            args[1] = (args[1],)
        self.args = args
    
    @property
    def arity(self):
        return 1 + len(self.args[1])
        
    def sort_result(self, result):
        result.sort(key=lambda literal: [id(term) for term in literal])
    
    def key(self, result):
        # return the grouping key of a result
        return list(result[:len(result)-self.arity])
    
    def reset(self):
        self._value = 0
        
    @property
    def value(self):
        return self._value
    
    def __le__(self,other):
        raise SyntaxError("Invalid use of Aggregate function. Please consider using parenthesis around aggregate definition.")

class Sum_aggregate(Aggregate):
    """ represents sum_foreach(X, key=(Y,Z))"""
    def add(self, other):
        self._value += other[-self.arity].id
        
class Len_aggregate(Aggregate):
    """ represents len(X)"""
    @property
    def arity(self):
        return 1
        
    def add(self, other):
        self._value += 1

class Concat_aggregate(Aggregate):
    """ represents concat(X, key=(Y,Z), sep=sep)"""
    @property
    def arity(self):
        return 2 + len(self.args[1])
        
    def sort_result(self, result):
        for i in reversed(range(len(self.args[1]))):
            result.sort(key=lambda literal, i=i: literal[-i-2].id, # -1 for separator
                reverse = self.args[1][i].negated)
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[:len(result)-self.arity]])
    
    def reset(self):
        self._value = []
        
    def add(self, other):
        self._value.append(other[-self.arity].id)
        
    @property
    def value(self):
        return self.args[2].join(self._value)

class Min_aggregate(Aggregate):
    """ represents min(X, key=(Y,Z))"""
    def sort_result(self, result):
        for i in reversed(range(len(self.args[1]))):
            result.sort(key=lambda literal, i=i: literal[-i-1].id,
                reverse = self.args[1][i].negated)
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[:len(result)-self.arity]])
        pass
    
    def reset(self):
        self._value = None
        
    def add(self, other):
        self._value = other[-self.arity].id if self._value is None else self._value

class Max_aggregate(Aggregate):
    """ represents max(X, key=(Y,Z))"""
    def sort_result(self, result):
        for i in reversed(range(len(self.args[1]))):
            result.sort(key=lambda literal, i=i: literal[-i-1].id,
                reverse = not self.args[1][i].negated)
        result.sort(key=lambda literal, self=self: [id(term) for term in literal[:len(result)-self.arity]])
        pass
    
    def reset(self):
        self._value = None
        
    def add(self, other):
        self._value = other[-self.arity].id if self._value is None else self._value

class Rank_aggregate(Aggregate):
    """ TODO represents rank(group_by=(X), key(Y)"""
    @property
    def arity(self):
        return len(self.args[0])

    def sort_result(self, result):
        key = self.args[0]
        for i in reversed(range(len(key))):
            result.sort(key=lambda literal, i=i: literal[-i-1].id,
                reverse = key[i].negated)
