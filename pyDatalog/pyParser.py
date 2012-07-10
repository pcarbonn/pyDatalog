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
This is done in the load() and add_program() method of Datalog_engine class.

Classes contained in this file:
* Datalog_engine_ : common part for an engine. Subclasses are Python_engine and Lua_engine
* Python_engine :implements the interface to the datalog engine written in python.  Instantiated by the calling module
* Lua_engine :implements the interface to the lua datalog engine written in Lua.  Instantiated by the calling module
* Symbol : contains a constant, a variable or a predicate. Instantiated before executing the datalog program
* Expression : made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program
* Lambda : represents a lambda function, used in expressions
* Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
* Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
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

import pyDatalog
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

import pyEngine

"""                             ENGINES                                                   """

class Datalog_engine_(object):
    """
    common part for an engine. Subclasses are Python_engine and Lua_engine
    """

    def add_symbols(self, names, variables):
        for name in names:
            variables[name] = Symbol(name, self)            
        
    def _assert_fact(self, literal):
        clause = self._make_clause(literal.lua, [])
        self._assert(clause)
        #print pr(self._db)
        
    def assert_fact(self, predicate_name, *args):
        literal = Literal(predicate_name, args, datalog_engine=self)
        self._assert_fact(literal)
    
    def _retract_fact(self, literal):
        clause = self._make_clause(literal.lua, [])
        self._retract(clause)

    def retract_fact(self, predicate_name, *args):
        literal = Literal(predicate_name, args, datalog_engine=self)
        self._retract_fact(literal)
    
    def add_clause(self,head,body):
        if isinstance(body, Body):
            tbl = [a.lua for a in body.body]
            self.clauses.append((head, body.body))
        else: # body is a literal
            #print(body)
            tbl = (body.lua,)
            self.clauses.append((head,[body]))
        clause = self._make_clause(head.lua, tbl)
        return self._assert(clause)
        
    class _NoCallFunction(object):
        """
        This class prevents a call to a datalog program created using the 'program' decorator
        """
        def __call__(self):
            raise TypeError("Datalog programs are not callable")
    
    def add_program(self, func):
        """
        A helper for decorator implementation
        """
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
        else:
            newglobals = func.func_globals.copy()
            
        defined = set(code.co_varnames).union(set(newglobals.keys())) # local variables and global variables
        defined = defined.union(dir(builtins))
        defined.add('None')

        self.load(source_code, newglobals, defined)
        return self._NoCallFunction()
    
    def ask(self, code, _fast=None):
        tree = ast.parse(code, '<string>', 'eval')
        #TODO transform tree ?
        code = compile(tree, '<string>', 'eval')
        newglobals = {}
        self.add_symbols(code.co_names, newglobals)
        lua_code = eval(code, newglobals)
        return self._ask_literal(lua_code, _fast)

    class _transform_ast(ast.NodeTransformer):
        def visit_Call(self, node):
            """len() --> __len__()"""
            if hasattr(node.func, 'id'):
                node.func.id = '__len__' if node.func.id == 'len' else node.func.id
                node.func.id = '__min__' if node.func.id == 'min' else node.func.id
                node.func.id = '__max__' if node.func.id == 'max' else node.func.id
            return node
    
    def load(self, code, newglobals={}, defined=set([])):
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
        
        tree = ast.parse(code, '<string>', 'exec')
        tree = Datalog_engine_._transform_ast().visit(tree)
        code = compile(tree, '<string>', 'exec')
        for name in set(code.co_names).difference(defined): # for names that are not defined
            self.add_symbols((name,), newglobals)
        six.exec_(code, newglobals)

class Python_engine(Datalog_engine_):
    def __init__(self):
        self.clauses = []
        self.clear()
        
        self._make_const = pyEngine.make_const       # make_const(id) --> { id: } unique, inherits from Const
        self._make_var = pyEngine.make_var           # make_var(id) --> { id: ) unique, inherits from Var
        self._make_literal = pyEngine.make_literal   # make_literal(pred_name, terms) --> { pred: , id: , <i>: , tag: } 
                                            #    where id represents name, terms; 
                                            #    where tag is used as a key to literal by the subgoal table
        
        self._make_clause = pyEngine.make_clause  # make_clause(head, body) = { head: , <i>: }
        
        self._assert = pyEngine.assert_              # assert(clause) --> clause or nil
        self._retract = pyEngine.retract             # retract(clause) --> clause
        self._ask = pyEngine.ask                    # ask(literal) = nil or {name: , arity: , <i>: {i: }}
        self._ask2 = pyEngine.ask2                  # ask2(literal, _fast) = nil or {name: , arity: , <i>: {i: }}
        self._db = pyEngine.db
        self._add_iter_prim = pyEngine.add_iter_prim # add_iter_prim(name, arity, iter) = 
        self._make_operand = pyEngine.make_operand
        self._make_expression = pyEngine.make_expression
        self._make_lambda = pyEngine.make_lambda
        self._add_expr_to_predicate = pyEngine.add_expr_to_predicate
        """ other functions available in datalog.lua
            # make_pred(name, arity) -->  { id: , db: { <clause ID>: }} unique, where id = name/arity.  (Called by make_pred)
            # get_name(pred) = 
            # get_arity(pred) = 
            # insert(pred) =
            # remove(pred) = 
            # save() = 
            # restore() = 
            # copy(src=None) = 
            # revert(clone) = 
        """

    def clear(self):
        pyEngine.clear()
        
    def _ask_literal(self, literal, _fast=None): # called by Literal
        # print("asking : %s" % str(literal))
        result = self._ask2(literal.lua, _fast)
        return None if not result else set(result.answers)
    
        
class Lua_engine(Datalog_engine_):
    def __init__(self):
        self.clauses = []
        self.lua = LuaRuntime()
        
        lua_program_path = os.path.join(os.path.dirname(__file__), 'luaEngine.py')
        lua_program = open(lua_program_path).read()
        self.lua.execute(lua_program)
        
        self._insert = self.lua.eval('table.insert')
        self._make_const = self.lua.eval('datalog.make_const')      # make_const(id) --> { id: } unique, inherits from Const
        self._make_var = self.lua.eval('datalog.make_var')          # make_var(id) --> { id: ) unique, inherits from Var
        self.lua_make_literal = self.lua.eval('datalog.make_literal')  # make_literal(pred_name, terms) --> { pred: , id: , <i>: , tag: } 
                                                                    #    where id represents name, terms; 
                                                                    #    where tag is used as a key to literal by the subgoal table
        self._make_literal = lambda name, atoms: self.lua_make_literal(name, self.lua_table(atoms))
        
        self.lua_make_clause = self.lua.eval('datalog.make_clause')    # make_clause(head, body) = { head: , <i>: }
        self._make_clause = lambda head, body: self.lua_make_clause(head, self.lua_table(body))
        
        self._assert = self.lua.eval('datalog.assert')              # assert(clause) --> clause or nil
        self._retract = self.lua.eval('datalog.retract')            # retract(clause) --> clause
        self._ask = self.lua.eval('datalog.ask')                    # ask(literal) = nil or {name: , arity: , <i>: {i: }}
        self._ask2 = self.lua.eval('datalog.ask2')                  # ask2(literal, _fast) = nil or {name: , arity: , <i>: {i: }}
        self._db = self.lua.eval('datalog.db')
        self._add_iter_prim = self.lua.eval('datalog.add_iter_prim')# add_iter_prim(name, arity, iter) = 
        self._make_operand = self.lua.eval('datalog.make_operand')
        self._make_expression = self.lua.eval('datalog.make_expression')
        self.lua_make_lambda = self.lua.eval('datalog.make_lambda')
        self._make_lambda = lambda lambda_object, operands: self.lua_make_lambda(lambda_object, self.lua_table(operands))
        self._add_expr_to_predicate = self.lua.eval('datalog.add_expr_to_predicate')
        """ other functions available in datalog.lua
            # make_pred(name, arity) -->  { id: , db: { <clause ID>: }} unique, where id = name/arity.  (Called by make_pred)
            # get_name(pred) = 
            # get_arity(pred) = 
            # insert(pred) =
            # remove(pred) = 
            # save() = 
            # restore() = 
            # copy(src=None) = 
            # revert(clone) = 
        """
    def clear(self):
        self.__init__()
        
    def lua_table(self, table):
        tbl = self.lua.eval('{ }')
        for a in table:
            self._insert(tbl, a)
        return tbl
        
    def _ask_literal(self, literal, _fast=None): # called by Literal
        # print("asking : %s" % str(literal))
        lua_result = self._ask2(literal.lua, _fast)
        
        if not lua_result: return None
        # print pr(lua_result)
        result = set(tuple(dict(lua_result[i+1]).values()) for i in range(len(lua_result)))
        #print(result)
        return result

def Datalog_engine(implementation=None): 
    """ a factory for Datalog_engine_ """
    if (implementation or Engine) == 'Lua':
        return Lua_engine()
    else:
        return Python_engine()
    
default_datalog_engine = Datalog_engine()

class Symbol(object):
    """
    can be constant, variable or predicate name
    ask() creates a query
    created when analysing the datalog program
    """
    def __init__ (self, name, datalog_engine=default_datalog_engine):
        self.name = name
        self.negated = False # for aggregate with sort in descending order
        self.datalog_engine = datalog_engine # needed to create Literal
        if isinstance(name, int):
            self.type = 'constant'
        elif (name[0] in string.ascii_uppercase):
            self.type = 'variable'
        else:
            self.type = 'constant'
        if self.type == 'variable':
            self.lua = datalog_engine._make_var(name)
        else:
            self.lua = datalog_engine._make_const(name)
        
    def __call__ (self, *args, **kwargs):
        "time to create a literal !"
        if self.name == 'ask':
            if 1<len(args):
                raise RuntimeError('Too many arguments for ask !')
            fast = kwargs['_fast'] if '_fast' in list(kwargs.keys()) else False
            return self.datalog_engine._ask_literal(args[0], fast)
        elif self.name == 'sum_foreach':
            args = (args[0], kwargs['key'])
            return Sum_aggregate(args)
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
            return Literal(self.name, args, self.datalog_engine)

    def _make_expression_literal(self, operator, other):
        """private function to create a literal for comparisons"""
        if isinstance(other, type(lambda: None)):
            other = Lambda(other, self.datalog_engine)
        name = '=' + str(self) + operator + str(other)
        if isinstance(other, int):
            literal = Literal(name, [self], self.datalog_engine)
            expr = self.datalog_engine._make_operand('constant', other)
        else: # other is a symbol or an expression
            literal = Literal(name, [self] + list(other._variables().values()), self.datalog_engine)
            expr = other.lua_expr(list(self._variables().keys())+list(other._variables().keys()))
        self.datalog_engine._add_expr_to_predicate(literal.lua.pred, operator, expr)
        return literal

    def __eq__(self, other):
        if self.type == 'variable' and (isinstance(other, Expression) or isinstance(other, type(lambda: None))):
            return self._make_expression_literal('=', other)
        else:
            return Literal("=", (self, other), self.datalog_engine)
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
        return Expression(self, '+', other, self.datalog_engine)
    def __sub__(self, other):
        return Expression(self, '-', other, self.datalog_engine)
    def __mul__(self, other):
        return Expression(self, '*', other, self.datalog_engine)
    def __div__(self, other):
        return Expression(self, '/', other, self.datalog_engine)
    def __truediv__(self, other):
        return Expression(self, '/', other, self.datalog_engine)
    def __floordiv__(self, other):
        return Expression(self, '//', other, self.datalog_engine)
    
    def __radd__(self, other):
        return Expression(other, '+', self, self.datalog_engine)
    def __rsub__(self, other):
        return Expression(other, '-', self, self.datalog_engine)
    def __rmul__(self, other):
        return Expression(other, '*', self, self.datalog_engine)
    def __rdiv__(self, other):
        return Expression(other, '/', self, self.datalog_engine)

    def __neg__(self):
        neg = Symbol(self.name, self.datalog_engine)
        neg.negated = True
        return neg
    
    def __coerce__(self, other):
        return None
    
    def __getattr__(self, name):
        return Symbol(self.name + '.' + name, self.datalog_engine)
    
    def __getitem__(self, keys):
        return Function(self.name, self.datalog_engine, keys)
    
    def lua_expr(self, variables):
        if self.type == 'variable':
            return self.datalog_engine._make_operand('variable', variables.index(self.name))
        else:
            return self.datalog_engine._make_operand('constant', self.name)
    
    def _variables(self):
        if self.type == 'variable':
            return {self.name : self}
        else:
            return {}

    def __str__(self):
        return str(self.name)

class Expression(object):
    """made of an operator and 2 operands. Instantiated when an operator is applied to a symbol while executing the datalog program"""
    def __init__(self, lhs, operator, rhs, datalog_engine=default_datalog_engine):
        self.operator = operator
        
        self.lhs = lhs
        if isinstance(lhs, six.string_types) or isinstance(lhs, int):
            self.lhs = Symbol(lhs, datalog_engine)
        elif isinstance(lhs, type(lambda: None)):
            self.lhs = Lambda(rhs, datalog_engine)
            
        self.rhs = rhs
        if isinstance(rhs, six.string_types) or isinstance(rhs, int):
            self.rhs = Symbol(rhs, datalog_engine)
        elif isinstance(rhs, type(lambda: None)):
            self.rhs = Lambda(rhs, datalog_engine)
        self.datalog_engine = datalog_engine
        
    def _variables(self):
        temp = self.lhs._variables()
        temp.update(self.rhs._variables())
        return temp
    
    def lua_expr(self, variables):
        return self.datalog_engine._make_expression(self.operator, self.lhs.lua_expr(variables), self.rhs.lua_expr(variables))
    
    def __str__(self):
        return '(' + str(self.lhs) + self.operator + str(self.rhs) + ')'

class Lambda(object):
    """represents a lambda function, used in expressions"""
    def __init__(self, other, datalog_engine=default_datalog_engine):
        self.operator = '<lambda>'
        self.lambda_object = other
        self.datalog_engine = datalog_engine
        
    def _variables(self):
        return dict([ [var, Symbol(var, self.datalog_engine)] for var in getattr(self.lambda_object,func_code).co_varnames])
    
    def lua_expr(self, variables):
        operands = [self.datalog_engine._make_operand('variable', variables.index(varname)) for varname in getattr(self.lambda_object,func_code).co_varnames] 
        return self.datalog_engine._make_lambda(self.lambda_object, operands)
    
    def __str__(self):
        return 'lambda%i(%s)' % (id(self.lambda_object), ','.join(getattr(self.lambda_object,func_code).co_varnames))
        
class Literal(object):
    """
    created by source code like 'p(a, b)'
    unary operator '+' means insert it as fact
    binary operator '&' means 'and', and returns a Body
    operator '<=' means 'is true if', and creates a Clause
    """
    def __init__(self, predicate_name, terms, datalog_engine=default_datalog_engine, prearity=None, aggregate=None):
        self.datalog_engine = datalog_engine # needed to insert facts, clauses
        self.predicate_name = predicate_name
        self.terms = terms
        self.prearity = prearity or len(terms)

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
            l = Literal(predicate_name, base_terms, self.datalog_engine, prearity, self.aggregate)
            self.datalog_engine.add_clause(l, l) # body will be ignored, but is needed to make the clause safe
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
                tbl.append(datalog_engine._make_const(a))
            elif isinstance(a, Literal):
                raise SyntaxError("Literals cannot have a literal as argument : %s%s" % (predicate_name, terms))
            elif isinstance(a, Aggregate):
                raise TypeError("Incorrect use of '%s' aggregation." % a.method)
            else:
                tbl.append(datalog_engine._make_const(a))
        # now create the literal for the head of a clause
        self.lua = datalog_engine._make_literal(h_predicate_name, tbl, h_prearity, aggregate)
        # TODO check that l.pred.aggregate is empty

    def __pos__(self):
        " unary + means insert into datalog_engine as fact "
        self.datalog_engine._assert_fact(self)

    def __neg__(self):
        " unary - means retract fact from datalog_engine "
        self.datalog_engine._retract_fact(self)
        
    def __invert__(self):
        """unary ~ means negation """
        negated_literal = Literal('~' + self.predicate_name, self.terms, self.datalog_engine)
        return negated_literal

    def __le__(self, body):
        " head <= body"
        result = self.datalog_engine.add_clause(self, body)
        if not result: 
            raise TypeError("Can't create clause %s <= %s" % (str(self), str(body)))

    def __and__(self, literal):
        " literal & literal" 
        return Body(self, literal)

    def __str__(self):
        terms = list(map (str, self.terms))
        return str(self.predicate_name) + "(" + string.join(terms,',') + ")"

class Body(object):
    """
    created by p(a,b) + q(c,d)
    operator '+' means 'and', and returns a Body
    """
    def __init__(self, literal1, literal2):
        self.body = [literal1, literal2]

    def __and__(self, literal):
        assert not literal.aggregate, "Aggregation cannot appear in the body of a clause"
        self.body.append(literal) 
        return self

class Function(object):
    """ represents predicate[a, b]"""
    def __init__(self, name, datalog_engine, keys):
        if not isinstance(keys, tuple):
            keys = (keys,)
        self.name = "%s[%i]" % (name, len(keys))
        self.datalog_engine = datalog_engine
        self.keys = keys
        
    def __eq__(self, other):
        terms = list(self.keys)
        terms.append(other)
        l = Literal(self.name, terms, datalog_engine=self.datalog_engine, prearity=len(self.keys))
        return l
    
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
