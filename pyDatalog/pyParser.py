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
* LazyList : a subclassable list that is populated when it is accessed. 
    * LazyListOfList : Mixin for Query and Body
* Literal : made of a predicate and a list of arguments.  Instantiated when a symbol is called while executing the datalog program
    * HeadLiteral
    * Query
* Body : a list of literals to be used in a clause. Instantiated when & is executed in the datalog program
* Expression : base class for objects that can be part of an inequality, operation or slice
    * VarSymbol : represents a constant, a variable or a predicate 
        * Symbol : represents a constant, a variable or a predicate in ProgramMode. Instantiated before executing the datalog program
        * Variable : represents a variable in in-line queries
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
* ProgramContext : class to safely differentiate between In-line queries and pyDatalog program / ask(), using ProgramMode global variable
* _transform_ast : performs some modifications of the abstract syntax tree of the datalog program
"""

import ast
from collections import OrderedDict
import inspect
import re
import string
import sys
import threading

PY3 = sys.version_info[0] == 3
func_code = '__code__' if PY3 else 'func_code'

try:
    from . import pyEngine
    from . import util
    from . import UserList
except ValueError:
    import pyEngine
    import util
    import UserList

# global variable to differentiate between in-line queries and pyDatalog program / ask
Thread_storage = threading.local()
Thread_storage.ProgramMode = False
Thread_storage.variables = set([]) #call list of variables parsed since the last clause

def clear():
    Thread_storage.ProgramMode = False

"""                             Parser classes                                                   """

class LazyList(UserList.UserList):
    """a subclassable list that is populated when it is accessed """
    """used by Literal, Body, Variable to delay evaluation of datalog queries written in python  """
    """ during debugging, beware that viewing a Lazylist will force its udpate""" 
    def __init__(self):
        self.todo = None # self.todo.ask() calculates self.data
        self._data = []
    
    @property
    def data(self):
        """ returns the list, after recalculation if needed """
        if self.todo is not None:
            self.variables = tuple(self.todo._variables().keys()) 
            self.todo.ask()
        return self._data

    def _value(self): # for backward compatibility ?
        return self.data
    
    def v(self):
        """ returns the first value in the list, or None """
        return True if self.data is True else self._data[0] if 0<len(self._data) else None

class LazyListOfList(LazyList):
    """ represents the result of an inline query (a Literal or Body)"""
    def __eq__(self, other):
        """ uses set comparison"""
        return other is True if self.data is True \
            else set(self.data) == set(other)
    
    def __ge__(self, variable):
        """ returns the first value of the variable in the result of a query, or None """
        if not isinstance(variable, Variable):
            return variable.__le__(self)
        return variable.v()
    
    def __str__(self):
        """ pretty print the result """
        if self.data in (True, [], [()]): return str(self._data)
        # get the widths of each column
        widths = [max(len(str(x)) for x in column) for column in zip(*(self._data))]
        widths = [max(widths[i], len(str(self.variables[i]))) for i in util.xrange(len(widths))]
        # get the formating string
        fofo = ' | '.join('%%-%ss' % widths[i] for i in util.xrange(len(widths)))
        return '\n'.join((fofo % self.variables, 
                            '-|-'.join( widths[i]*'-' for i in util.xrange(len(widths))),
                            '\n'.join(fofo % s for s in self._data)))


class Expression(object):
    """ base class for objects that can be part of an inequality, operation or slice """
    @classmethod
    def _pyD_for(cls, operand):
        """ factory that converts an operand to an Expression """
        if isinstance(operand, (Expression, Aggregate)):
            return operand
        if isinstance(operand, type(lambda: None)) and (operand.__name__ if PY3 else operand.func_name) == '<lambda>':
            return Operation(None, operand, [Symbol(var) for var in getattr(operand,func_code).co_varnames])
        if isinstance(operand, slice):
            return Symbol([operand.start, operand.stop, operand.step])
        return Symbol(operand, forced_type="constant")
    
    # handlers of inequality and operations
    def __eq__(self, other):
        if isinstance(self, Operation) and self._pyD_operator in '+-' and self._pyD_lhs._pyD_value == 0:
            raise util.DatalogError("Did you mean to assert or retract a fact ? Please add parenthesis.", None, None)
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
    def in_(self, values):
        """ called when evaluating (X in (1,2)) """
        return Literal.make_for_comparison(self, '_pyD_in', values)
    _in = in_ # for backward compatibility
    def not_in_(self, values):
        """ called when evaluating (X not in (1,2)) """
        return Literal.make_for_comparison(self, '_pyD_not_in', values)
    _not_in = not_in_  # for backward compatibility
    
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
            return Operation(self, '[', [keys.start, keys.stop, keys.step])
        return Operation(self, '[', keys)
    
    def __getattr__(self, name):
        """ called when evaluating <expression>.attribute """
        return Operation(self, '.',  VarSymbol(name, forced_type='constant'))

    def __call__ (self, *args, **kwargs):
        if kwargs:
            raise "Sorry, key word arguments are not supported yet"
        return Operation(self, '(', args)

    
class VarSymbol(Expression):
    """ represents the symbol of a variable, both inline and in pyDatalog program """
    def __init__ (self, name, forced_type=None):
        self._pyD_negated = False # for aggregate with sort in descending order
        self._pyD_precalculations = Body() # no precalculations
        self._pyD_atomized = True
        name = True if name=='True' else False if name =='False' else name
        if isinstance(name, (list, tuple, util.xrange)):
            self._pyD_value = list(map(Expression._pyD_for, name))
            self._pyD_name = str([element._pyD_name for element in self._pyD_value])
            self._pyD_type = 'tuple'
            self._pyD_lua = pyEngine.Interned.of([e._pyD_lua for e in self._pyD_value])
            self._pyD_precalculations = pre_calculations(self._pyD_value)
        elif forced_type=="constant" or isinstance(name, (int, float)) \
        or name in (True, False, None) \
        or (isinstance(name, util.string_types) and name[0] not in string.ascii_uppercase + '_' and not '.' in name):
            self._pyD_value = name
            self._pyD_name = str(name)
            self._pyD_type = 'constant'
            self._pyD_lua = pyEngine.Const(name)
        else:
            self._pyD_value = name
            self._pyD_name = name
            self._pyD_type = 'variable'
            index = name.find('.')
            if 0 < index:
                self._pyD_lua = pyEngine.Operation(pyEngine.Var(name[:index]), '.',  pyEngine.Const(name[index:]))
            else:
                self._pyD_lua = pyEngine.Var(name) 

    @classmethod
    def make_for_prefix(cls, name): #prefixed #call
        """ returns either '_pyD_class' or the prefix"""
        prefix = name.split('.')[0]
        if prefix in pyEngine.Class_dict or prefix not in Thread_storage.variables:
            return VarSymbol('_pyD_class', forced_type='constant')
        return VarSymbol(prefix)

    def __neg__(self):
        """ called when evaluating -X. Used in aggregate arguments """
        neg = Symbol(self._pyD_value)
        neg._pyD_negated = True

        expr = 0 - self
        expr.variable = neg
        return expr
    
    def _pyD_variables(self):
        """ returns an ordered dictionary of the variables in the varSymbol """
        if self._pyD_type == 'variable' and not self._pyD_name.startswith('_pyD_'):
            return OrderedDict({self._pyD_name : self})
        elif self._pyD_type == 'tuple':
            variables = OrderedDict()
            for element in self._pyD_value:
                variables.update(element._pyD_variables())
            return variables
        else:
            return OrderedDict()
    
class Variable(threading.local, VarSymbol, LazyList):
    def __init__(self, name=None):
        name = 'X%i' % id(self) if name is None else name
        LazyList.__init__(self)
        VarSymbol.__init__(self, name)

    def __add__(self, other):
        return Operation(self, '+', other)
    def __radd__(self, other):
        return Operation(other, '+', self)


def pre_calculations(args):
    """ collects the pre_calculations of all args"""
    pre_calculations = Body()
    for arg in args:
        if isinstance(arg, Expression):
            pre_calculations = pre_calculations & arg._pyD_precalculations
    return pre_calculations
        
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
            return Answer.make(args[0].ask())
        
        # manage the aggregate functions
        elif self._pyD_name in ('_sum', 'sum_'):
            if isinstance(args[0], VarSymbol):
                return Sum_aggregate(args[0], for_each=kwargs.get('for_each', kwargs.get('key', [])))
            else:
                return sum(args)
        elif self._pyD_name in ('concat', 'concat_'):
            return Concat_aggregate(args[0], order_by=kwargs.get('order_by',kwargs.get('key', [])), sep=kwargs['sep'])
        elif self._pyD_name in ('_min', 'min_'):
            if isinstance(args[0], VarSymbol):
                return Min_aggregate(args[0], order_by=kwargs.get('order_by',kwargs.get('key', [])),)
            else:
                return min(args)
        elif self._pyD_name in ('_max', 'max_'):
            if isinstance(args[0], VarSymbol):
                return Max_aggregate(args[0], order_by=kwargs.get('order_by',kwargs.get('key', [])),)
            else:
                return max(args)
        elif self._pyD_name in ('rank', 'rank_'):
            return Rank_aggregate(None, for_each=kwargs.get('for_each', []), order_by=kwargs.get('order_by', []))
        elif self._pyD_name in ('running_sum', 'running_sum_'):
            return Running_sum(args[0], for_each=kwargs.get('for_each', []), order_by=kwargs.get('order_by', []))
        elif self._pyD_name == 'tuple_':
            return Tuple(args[0], order_by=kwargs.get('order_by', []))
        elif self._pyD_name in ('_len', 'len_'):
            if isinstance(args[0], VarSymbol):
                return Len_aggregate(args[0])
            else: 
                return len(args[0]) 
        elif self._pyD_name == 'range_':
            return Operation(None, '..', args[0])
        elif self._pyD_name == 'format_':
            return Operation(args[0], '%', args[1:])
        elif '.' in self._pyD_name: #call
            pre_term = (VarSymbol.make_for_prefix(self._pyD_name), ) #prefixed
            if pyEngine.Pred.is_known('%s/%i' % (self._pyD_name, len(args)+1)):
                return Literal.make(self._pyD_name, pre_term + tuple(args))
            return Call(self._pyD_name, pre_term + tuple(args))
        else: # create a literal
            literal = Literal.make(self._pyD_name, tuple(args))
            return literal

    def __getattr__(self, name):
        """ called when evaluating class.attribute """
        if self._pyD_name in Thread_storage.variables: #prefixed
            return Operation(self, '.', name)
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
        (function == function._pyD_symbol) <= (function._pyD_symbol == value)
        
class Function(Expression):
    """ represents predicate[a, b]"""
    counter = util.Counter() # counter of functions evaluated so far
    @classmethod
    def newSymbol(cls):
        """ returns a new unique Symbol associated to a Function """
        return Symbol('_pyD_X%i' % Function.counter.next())
        
    def __init__(self, name, keys):
        self._pyD_keys = keys if isinstance(keys, tuple) else (keys,)
        self._pyD_name = "%s[%i]" % (name, len(self._pyD_keys))
        self._argument_precalculations = pre_calculations(self._pyD_keys)
                
        self._pyD_symbol = Function.newSymbol()
        self._pyD_lua = self._pyD_symbol._pyD_lua
        self._pyD_precalculations = self._argument_precalculations & (self == self._pyD_symbol)
    
    def __eq__(self, other):
        return Literal.make_for_comparison(self, '==', other)
    
    # following methods are used when the function is used in an expression
    def _pyD_variables(self):
        """ returns an ordered dictionary of the variables in the keys of the function"""
        return self._argument_precalculations._variables()

    def __str__(self):
        return "%s[%s]" % (self._pyD_name.split('[')[0], ','.join(str(key) for key in self._pyD_keys))
    
class Operation(Expression):
    """created when evaluating an operation (+, -, *, /, //) """
    def __init__(self, lhs, operator, rhs):
        self._pyD_operator = operator
        self._pyD_lhs = Expression._pyD_for(lhs) # left  hand side
        self._pyD_rhs = Expression._pyD_for(rhs)
        self._pyD_lua = pyEngine.Operation(self._pyD_lhs._pyD_lua, self._pyD_operator, self._pyD_rhs._pyD_lua)
        self._pyD_precalculations = pre_calculations((lhs, rhs)) #TODO test for slice, len
        
    @property
    def _pyD_name(self):
        return str(self)
    
    def _pyD_variables(self):
        """ returns an ordered dictionary of the variables in this Operation"""
        temp = self._pyD_lhs._pyD_variables()
        temp.update(self._pyD_rhs._pyD_variables())
        return temp
    
    def __str__(self):
        return '(' + str(self._pyD_lhs._pyD_name) + self._pyD_operator + str(self._pyD_rhs._pyD_name) + ')'

class Literal(object):
    """
    created by source code like 'p(a, b)'
    operator '<=' means 'is true if', and creates a Clause
    """
    def __init__(self, predicate_name, args, prearity=None, aggregate=None):
        self.predicate_name = predicate_name
        self.prearity = len(args) if prearity is None else prearity
        self.pre_calculations = Body()
        
        self.args = args
        self.todo = self
        
        cls_name = predicate_name.split('.')[0].replace('~','') if 1< len(predicate_name.split('.')) else ''
        if pyEngine.Class_dict.get(cls_name, None):
            if 2<=len(args) and not isinstance(args[1], VarSymbol) and cls_name not in [c.__name__ for c in args[1].__class__.__mro__]:
                raise TypeError("Object is incompatible with the class that is queried.") #prefixed

        self.terms = [] # the list of args converted to Expression
        for arg in args:
            if isinstance(arg, Literal):
                raise util.DatalogError("Syntax error: Literals cannot have a literal as argument : %s%s" % (predicate_name, self.terms), None, None)
            elif isinstance(arg, Aggregate):
                raise util.DatalogError("Syntax error: Incorrect use of aggregation.", None, None)
            if isinstance(arg, Variable):
                arg.todo = self
                arg._data = [] # reset the variable. For use in in-line queries
            self.terms.append(Expression._pyD_for(arg))
                            
        for term in self.terms:
            for var in term._pyD_variables().keys():
                Thread_storage.variables.add(var) #call update the list of variables since the last clause
            
        tbl = [a._pyD_lua for a in self.terms]
        # now create the literal for the head of a clause
        self.lua = pyEngine.Literal(predicate_name, tbl, self.prearity, aggregate)
        # TODO check that l.pred.aggregate is empty

    @classmethod
    def make(cls, predicate_name, terms, prearity=None, aggregate=None):
        """ factory class that creates a Query or HeadLiteral """
        precalculations = pre_calculations(terms)
        if predicate_name[-1]=='!': #pred e.g. aggregation literal
            return precalculations & HeadLiteral(predicate_name, terms, prearity, aggregate)
        else:
            return precalculations & Query(predicate_name, terms, prearity, aggregate)
    
    @classmethod
    def make_for_comparison(cls, self, operator, other):
        """ factory of Literal (or Body) for a comparison. """
        other = Expression._pyD_for(other)
        if isinstance(other, Function) and operator == '==':
            self, other = other, self
        if isinstance(self, Function):
            #TODO perf : do not add pre-term for non prefixed #prefixed
            name, prearity = self._pyD_name + operator, 1+len(self._pyD_keys)
            terms = [VarSymbol.make_for_prefix(self._pyD_name)] + list(self._pyD_keys) + [other]  #prefixed
            if isinstance(other, Aggregate): # p[X]==aggregate() # TODO create 2 literals here
                if operator != '==':
                    raise util.DatalogError("Aggregate operator can only be used with equality.", None, None)
                
                # 1 create literal for queries
                terms[-1] = (Symbol('X')) # (X, X)
                l = Literal.make(name, terms, prearity, other)
                add_clause(l, l) # body will be ignored, but is needed to make the clause safe

                # 2 prepare literal for the calculation. It can be used in the head only
                #TODO for speed use terms[1:], prearity-1
                del terms[-1] # --> (X,)
                terms.extend(other.args)
                prearity = len(terms) # (X,Y,Z)
                return Literal.make(name + '!', terms, prearity=prearity) #pred
            literal = Query(name, terms, prearity)
            return self._argument_precalculations & other._pyD_precalculations & literal
        else:
            if not isinstance(other, Expression):
                raise util.DatalogError("Syntax error: Symbol or Expression expected", None, None)
            literal = Query(operator, [self] + [other])
            return self._pyD_precalculations & other._pyD_precalculations & literal

    @property
    def literals(self):
        return [self]
    
    def _variables(self):
        """ returns an ordered dictionary of the variables in the Literal"""
        if self.predicate_name[0] == '~': #pred ignore variables of negated literals
            return OrderedDict()
        variables = OrderedDict()
        for term in self.terms:
            variables.update(term._pyD_variables())
        return variables
    
    def __le__(self, body):
        " head <= body creates a clause"
        Thread_storage.variables = set([]) #call reset the list of variables
        body = body.as_literal if isinstance(body, Call) else body #call
        if not isinstance(body, (Literal, Body)):
            raise util.DatalogError("Invalid body for clause", None, None)
        newBody = Body()
        for literal in body.literals:
            if isinstance(literal, HeadLiteral):
                raise util.DatalogError("Aggregation cannot appear in the body of a clause", None, None)
            newBody = newBody & literal
        return add_clause(self, newBody)

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
        if self._variables():
            raise util.DatalogError("Cannot assert a fact containing Variables", None, None)
        clause = pyEngine.Clause(self.lua, [])
        pyEngine.assert_(clause)

    def __neg__(self):
        " unary - means retract fact from database "
        if self._variables():
            raise util.DatalogError("Cannot retract a fact containing Variables", None, None)
        clause = pyEngine.Clause(self.lua, [])
        pyEngine.retract(clause)
        
    def __invert__(self):
        """unary ~ means negation """
        return Literal.make('~' + self.predicate_name, self.terms) #pred

    def __and__(self, other):
        " literal & literal" 
        return Body(self, other)

    def __str__(self):
        if Thread_storage.ProgramMode:
            terms = list(map (str, self.terms))
            return str(self.predicate_name) + "(" + ','.join(terms) + ")"
        else:
            return LazyListOfList.__str__(self)
    
    def __eq__(self, other):
        if Thread_storage.ProgramMode:
            raise util.DatalogError("Syntax error near equality: consider using brackets. %s" % str(self), None, None)
        else:
            return super(Literal, self).__eq__(other)

    def literal(self):
        return self

class Call(Expression): #call
    """ represents an ambiguous A.b(X) : usually an expression, but sometimes a literal"""
    def __init__(self, name, args):
        self._pyD_name, self._pyD_args = name, args
        self.as_literal = Query(self._pyD_name, self._pyD_args)
        for arg in self._pyD_args:
            if isinstance(arg, Variable):
                arg.todo = self.as_literal
    
    @property
    def literals(self):
        return [self.as_literal]
    
    def __and__(self, other):
        " Call & literal"
        return Body(self.as_literal, other)
        
    def __invert__(self):
        """unary ~ means negation """
        return ~ self.as_literal

    def __le__(self, other):
        " head <= other creates a clause or comparison"
        if isinstance(other, (Literal, Body)):
            return self.as_literal <= other
        return other > self
        
class Body(LazyListOfList):
    """ created by p(a,b) & q(c,d)  """
    counter = util.Counter()
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
        b = Body(self, body2)
        return b if len(b.literals) != 1 else b.literals[0]
    
    def __str__(self):
        if True: # use False for debugging of parser
            return LazyListOfList.__str__(self)
        return ' & '.join(list(map (str, self.literals)))

    def literal(self):
        """ return a literal that can be queried to resolve the body """
        prearity = None
        if len(self.literals)==1: # determine the literal prearity in case of a single literal
            # it could be less than the literal prearity in case of repetition of a variable
            base_literal = self.literals[0]
            if not base_literal.predicate_name.startswith('~'):
                variables = OrderedDict()
                for i in range(base_literal.prearity):
                    variables.update(base_literal.terms[i]._pyD_variables())
                prearity = len(variables)
        literal = Literal.make('_pyD_query' + str(Body.counter.next()), list(self._variables().values()), prearity=prearity)
        literal <= self
        return literal
        
    def __invert__(self):
        """unary ~ means negation """
        return ~(self.literal())

    def ask(self):
        """ resolve the query and determine the values of its variables"""
        literal = self.literal()
        self._data = literal.lua.ask()
        literal.todo, self.todo = None, None
        if not Thread_storage.ProgramMode and self._data: 
            if self._data is True:
                for arg in literal.terms:
                    if isinstance(arg, Variable):
                        arg._data = True
                        arg.todo = None
                return True
            transposed = list(zip(*(self._data))) # transpose result
            result = []
            for i, arg in enumerate(literal.terms):
                if isinstance(arg, Variable) and len(arg._data)==0:
                    arg._data.extend(transposed[i])
                    arg.todo = None
                    result.append(transposed[i])
            self._data = list(zip(*result)) if result else [()]
        - (literal <= self) # delete the temporary clause
        return self._data

def add_clause(head,body):
    if isinstance(body, Body):
        tbl = [a.lua for a in body.literals]
    else: # body is a literal
        tbl = (body.lua,)
    clause = pyEngine.Clause(head.lua, tbl)
    result = pyEngine.assert_(clause)
    if not result: 
        raise util.DatalogError("Can't create clause", None, None)
    return result


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
        
        if not all([isinstance(e, VarSymbol) for e in self.for_each]):
            raise util.DatalogError("for_each argument of aggregate must be variable(s).", None, None)
        if not all([isinstance(e, VarSymbol) for e in self.order_by]):
            raise util.DatalogError("order_by argument of aggregate must be variable(s).", None, None)
        
        if sep and not isinstance(sep, util.string_types):
            raise util.DatalogError("Separator in aggregation must be a string", None, None)
        self.sep = sep
        
        # verify presence of keyword arguments
        for kw in self.required_kw:
            arg = getattr(self, kw)
            if arg is None or (isinstance(arg, tuple) and arg == tuple()):
                raise util.DatalogError("Error: argument missing in aggregate", None, None)
        
        # used to create literal.
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
        self.slice_group_by = slice(1, for_each_start-self.Y_arity)  #prefixed
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
    """ represents sum_(Y, for_each=(Z,T))"""
    required_kw = ('Y', 'for_each')

    def add(self, row):
        self._value += row[-self.arity].id
        
class Len_aggregate(Aggregate, Operation):
    """ represents len_(X) : a simple or aggregate operation"""
    required_kw = ('Y')

    def __init__(self, Y):
        Aggregate.__init__(self, Y)
        Operation.__init__(self, None, '#', Y)
        
    def add(self, row):
        self._value += 1

class Tuple(Aggregate):
    """ represents tuple_(X, order_by=(Y,)"""
    required_kw = ('Y', 'order_by')
        
    def reset(self):
        self._value = []
        
    def add(self, row):
        self._value.append(row[-self.arity].id)
        
    @property
    def value(self):
        return tuple(self._value)
    
class Concat_aggregate(Tuple):
    """ represents concat_(Y, order_by=(Z1,Z2), sep=sep)"""
    required_kw = ('Y', 'order_by', 'sep')

    @property
    def value(self):
        return self.sep.join(self._value)

class Min_aggregate(Aggregate):
    """ represents min_(Y, order_by=(Z,T))"""
    required_kw = ('Y', 'order_by')

    def reset(self):
        self._value = None
        
    def add(self, row):
        self._value = row[-self.arity].id if self._value is None else self._value

class Max_aggregate(Min_aggregate):
    """ represents max_(Y, order_by=(Z,T))"""
    def __init__(self, *args, **kwargs):
        Min_aggregate.__init__(self, *args, **kwargs)
        for a in self.order_by:
            a._pyD_negated = not(a._pyD_negated)

class Rank_aggregate(Aggregate):
    """ represents rank_(for_each=Z, order_by=T)"""
    required_kw = ('for_each', 'order_by')
    
    def reset(self):
        self.count = 0
        self._value = None

    def add(self, row):
        # retain the value if (X,) == (Z,)
        if row[self.slice_group_by] == row[self.slice_for_each]:
            self._value = [row[0],] + list(row[self.slice_group_by]) + [pyEngine.Const(self.count),] #prefixed
            return self._value
        self.count += 1
        
    def fact(self, k):
        return self._value

class Running_sum(Rank_aggregate):
    """ represents running_sum(Y, for_each=Z, order_by=T"""
    required_kw = ('Y', 'for_each', 'order_by')
    
    def add(self,row):
        self.count += row[self.to_add].id # TODO
        if row[1:self.to_add] == row[self.slice_for_each]: #prefixed
            self._value = list(row[:self.to_add]) + [pyEngine.Const(self.count),]
            return self._value

        
"""                             Parser methods                                                   """

class ProgramContext(object):
    """class to safely use ProgramMode within the "with" statement"""
    def __enter__(self):
        self.Mode = Thread_storage.ProgramMode
        Thread_storage.ProgramMode = True
    def __exit__(self, exc_type, exc_value, traceback):
        Thread_storage.ProgramMode = self.Mode
 
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
            node.func.id = 'sum_' if node.func.id == 'sum' else node.func.id
            node.func.id = 'len_' if node.func.id == 'len' else node.func.id
            node.func.id = 'min_' if node.func.id == 'min' else node.func.id
            node.func.id = 'max_' if node.func.id == 'max' else node.func.id
        return node
    
    def visit_Compare(self, node):
        """ rename 'in' to allow customization of (X in (1,2))"""
        self.generic_visit(node)
        if 1 < len(node.comparators): 
            raise util.DatalogError("Syntax error: please verify parenthesis around (in)equalities", node.lineno, None) 
        if not isinstance(node.ops[0], (ast.In, ast.NotIn)): return node
        var = node.left # X, an _ast.Name object
        comparators = node.comparators[0] # (1,2), an _ast.Tuple object
        newNode = ast.Call(
                ast.Attribute(var, 'in_' if isinstance(node.ops[0], ast.In) else 'not_in_', var.ctx), # func
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
    lines = code.splitlines() if isinstance(code, util.string_types) else code
    r = re.compile('^\s*')
    for line in lines:
        spaces = r.match(line).group()
        if spaces and line != spaces:
            break
    code = '\n'.join([line.replace(spaces,'') for line in lines])
    
    tree = ast.parse(code, function, 'exec')
    try:
        tree = _transform_ast().visit(tree)
    except util.DatalogError as e:
        e.function = function
        e.message = e.value
        e.value = "%s\n%s" % (e.value, lines[e.lineno-1])
        util.reraise(*sys.exc_info())
    code = compile(tree, function, 'exec')

    defined = defined.union(dir(util.builtins))
    defined.add('None')
    for name in set(code.co_names).difference(defined): # for names that are not defined
        add_symbols((name,), newglobals)
    try:
        with ProgramContext():
            util.exec_(code, newglobals)
    except util.DatalogError as e:
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
        util.reraise(*sys.exc_info())
        
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

def ask(code):
    """ runs the query in the code string """
    with ProgramContext():
        tree = ast.parse(code, 'ask', 'eval')
        tree = _transform_ast().visit(tree)
        code = compile(tree, 'ask', 'eval')
        newglobals = {}
        add_symbols(code.co_names, newglobals)
        parsed_code = eval(code, newglobals)
        return Answer.make(parsed_code.ask())

class Answer(object):
    """ object returned by ask() """
    def __init__(self, name, arity, answers):
        self.name = name
        self.arity = arity
        self.answers = answers

    @classmethod
    def make(cls, answers):
        if answers is True:
            answer = Answer('_pyD_query', 0, True)
        elif answers:
            answer = Answer('_pyD_query', len(answers), answers)
        else:
            answer = None
        if pyEngine.Auto_print: 
            print(answers)
        return answer        

    def __eq__ (self, other):
        return other == True if self.answers is True \
            else other == set(self.answers) if self.answers \
            else other is None
    def __str__(self):
        return 'True' if self.answers is True \
            else str(set(self.answers)) if self.answers is not True \
            else 'True'
