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

Datalog vocabulary:
    q(X):- q(a)
        X is a variable term
        a is a constant term
        q is a predicate
        q(a) is a literal
        q(X):- q(a) is a clause

Design principle:
Instead of writing our own parser, we use python's parser.  The datalog code is first compiled, 
then "undefined" variables are initialized as instance of Symbol, then the code is finally executed.
This is done in the Datalog_engine class.

Classes contained in this file:
* Symbol : contains a constant, a variable or a predicate. Instantiated before executing the datalog program
* Expression : made of an operator and 2 operands. Instantiated when an operand is applied to a symbol while executing the datalog program
* Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
* Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
* Datalog_engine : implements the interface to the lua datalog engine.  Instantiated by the calling module
* program : decorator function to create datalog programs

"""
import os
import string
import six
from six.moves import builtins

try:
    import lupa
    from lupa import LuaRuntime
    Engine = 'Lua'
except:
    Engine = 'Python'
#Engine = 'Python'
print(('Using %s engine for Datalog.' % Engine))

try:
    from . import pyEngine
except ValueError:
    import pyEngine

default_datalog_engine = None # will contain the default datalog engine

class Symbol:
    """
    can be constant, variable or predicate name
    ask() creates a query
    created when analysing the datalog program
    """
    def __init__ (self, name, datalog_engine=default_datalog_engine):
        self.name = name
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
            # TODO check that there is only one argument
            fast = kwargs['_fast'] if '_fast' in list(kwargs.keys()) else False
            return self.datalog_engine._ask_literal(args[0], fast)
        elif self.type == 'variable':
            raise TypeError("predicate name must start with a lower case : %s" % self.name)
        else:
            return Literal(self.name, args, self.datalog_engine)

    def _make_expression_literal(self, operator, other):
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
        if self.type == 'variable' and isinstance(other, Expression):
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
    
    def __radd__(self, other):
        return Expression(other, '+', self, self.datalog_engine)
    def __rsub__(self, other):
        return Expression(other, '-', self, self.datalog_engine)
    def __rmul__(self, other):
        return Expression(other, '*', self, self.datalog_engine)
    def __rdiv__(self, other):
        return Expression(other, '/', self, self.datalog_engine)
        
    def lua_expr(self, variables):
        if self.type == 'variable':
            return self.datalog_engine._make_operand(type, variables.index(self.name))
        else:
            return self.datalog_engine._make_operand('constant', self.name)
    
    def _variables(self):
        if self.type == 'variable':
            return {self.name : self}
        else:
            return {}

    def __str__(self):
        return str(self.name)

class Expression:
    def __init__(self, lhs, operator, rhs, datalog_engine=default_datalog_engine):
        self.operator = operator
        self.lhs = lhs
        if isinstance(lhs, six.string_types) or isinstance(lhs, six.string_types):
            self.lhs = Symbol(lhs, datalog_engine)
        self.rhs = rhs
        if isinstance(rhs, str) or isinstance(rhs, int):
            self.rhs = Symbol(rhs, datalog_engine)
        self.datalog_engine = datalog_engine
        
    def _variables(self):
        temp = self.lhs._variables()
        temp.update(self.rhs._variables())
        return temp
    
    def lua_expr(self, variables):
        return self.datalog_engine._make_expression(self.operator, self.lhs.lua_expr(variables), self.rhs.lua_expr(variables))
    
    def __str__(self):
        return '(' + str(self.lhs) + self.operator + str(self.rhs) + ')'

class Literal:
    """
    created by source code like 'p(a, b)'
    unary operator '+' means insert it as fact
    binary operator '+' means 'and', and returns a Body
    operator '<=' means 'is true if', and creates a Clause
    """
    def __init__(self, predicate_name, terms, datalog_engine=default_datalog_engine):
        # TODO verify that terms are not Literals
        self.datalog_engine = datalog_engine # needed to insert facts, clauses
        self.predicate_name = predicate_name
        self.terms = terms
        tbl = []
        for a in terms:
            if isinstance(a, Symbol):
                tbl.append(a.lua)
            elif isinstance(a, str):
                tbl.append(datalog_engine._make_const(a))
            elif isinstance(a, Literal):
                raise SyntaxError("Literals cannot have a literal as argument : %s%s" % (predicate_name, terms))
            else:
                tbl.append(datalog_engine._make_const(a))
        self.lua = datalog_engine._make_literal(predicate_name, tbl)
        #print pr(self.lua)

    def __pos__(self):
        " unary + means insert into datalog_engine as fact "
        # TODO verify that terms are constants !
        self.datalog_engine.assert_fact(self)

    def __neg__(self):
        " unary + means insert into datalog_engine as fact "
        # TODO verify that terms are constants !
        self.datalog_engine.retract_fact(self)

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

class Body:
    """
    created by p(a,b) + q(c,d)
    operator '+' means 'and', and returns a Body
    """
    def __init__(self, literal1, literal2):
        self.body = [literal1, literal2]

    def __and__(self, literal):
        self.body.append(literal) 
        return self

def Datalog_engine(implementation=None): # factory
    if (implementation or Engine) == 'Lua':
        return Lua_engine()
    else:
        return Python_engine()
    
class Datalog_engine_:
    """
    common part for an engine. Subclasses are Python_engine and Lua_engine
    """

    def add_symbols(self, names, variables):
        for name in names:
            if not name.startswith('_'):
                variables[name] = Symbol(name, self)            
        
    def assert_fact(self, literal):
        clause = self._make_clause(literal.lua, [])
        self._assert(clause)
        #print pr(self._db)
        
    def retract_fact(self, literal):
        clause = self._make_clause(literal.lua, [])
        self._retract(clause)

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
        
    class _NoCallFunction:
        """
        This class prevents a call to a datalog program
        """
        def __call__(self):
            raise TypeError("Datalog programs are not callable")
    
    def add_program(self, func):
        """
        A helper for decorator implementation
        """
        try:
            code = func.__code__
        except:
            raise TypeError("function or method argument expected")
        names = set(code.co_names)
        defined = set(code.co_varnames).union(set(func.__globals__.keys())) # local variables and global variables
        defined = defined.union(dir(builtins))
        defined.add('None')
        newglobals = func.__globals__.copy()
        i = None
        for name in names.difference(defined): # for names that are not defined
            if not name.startswith('_'):
                self.add_symbols((name,), newglobals)
                # newglobals[name] = Symbol(name, self)
            else:
                newglobals[name] = i
        six.exec_(code, newglobals)
        return self._NoCallFunction()
    
    def ask(self, code, _fast=None):
        ast = compile(code, '<string>', 'eval')
        newglobals = {}
        self.add_symbols(ast.co_names, newglobals)
        lua_code = eval(code, newglobals)
        return self._ask_literal(lua_code, _fast)

    def load(self, code):
        ast = compile(code, '<string>', 'exec')
        newglobals = {}
        self.add_symbols(ast.co_names, newglobals)
        six.exec_(ast, newglobals)

    def prt(self):
        """
        TODO Print the clauses
        """
        for (h,b) in self.clauses:
            if isinstance(b, list):
                print((h, ":-", string.join(list(map(str, b)), " , "), "."))
            else:
                print((h, ":-", str(b), "."))

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
        
        lua_program_path = os.path.join(os.path.dirname(__file__), 'pyDatalog.lua')
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
    
def program(datalog_engine=None):
    """
    A decorator for datalog program
    """
    datalog_engine = datalog_engine or default_datalog_engine
    return datalog_engine.add_program

def pr(a, level=0):
    try:
        #if isinstance(a, 'Lua_table'):
        if level<3:
            return [ (x[0], pr(x[1], level+1)) for x in list(a.items()) ]
        else:
            return [ (x[0], x[1]) for x in list(a.items()) ]

    except:
        return a

def ask(code):
    return default_datalog_engine.ask(code)
def load(code):
    return default_datalog_engine.load(code)
def clear():
    """ create a new engine """
    global default_datalog_engine
    default_datalog_engine.clear()

default_datalog_engine = Datalog_engine()