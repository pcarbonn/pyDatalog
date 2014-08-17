"""
This file is part of pyDatalog, a datalog engine for python.

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
"""

"""
This file contains the port of the datalog engine of J. D. Ramsdell, 
from lua to python, with many enhancements.

See also doc.py for additional source code documentation.

Some notable differences between python and lua:
* lua indices start at 1, not 0
* any variable is true in lua if it is not nil or false.
* lua tables contain both a list and a dictionary --> need to change them to objects
* lua variables are global by default, python ones are local by default
* variable bindings in a closure.  See http://tiny.cc/7837cw, http://tiny.cc/rq47cw
"""
from collections import OrderedDict
import gc
import logging
import re
import threading
import weakref

from . import util

Logging = False # True --> logging is activated.  Kept for performance reason
Auto_print = False # True => automatically prints the result of a query

Python_resolvers = {} # dictionary  of python functions that can resolve a predicate
Logic = None # place holder for Logic class from Logic module

# Keep a dictionary of classes with datalog capabilities.  
Class_dict = {}


#       DATA TYPES          #####################################

class Term(object):
    @classmethod
    def of(cls, atom):
        """ factory function for Term """
        if isinstance(atom, (Term, Operation)):
            return atom
        elif isinstance(atom, (list, tuple, util.xrange)):
            return VarTuple(tuple(Term.of(element) for element in atom))
        else:
            return Const(atom)
    def __eq__(self, other):
        return self.id == other.id
    def __ne__(self, other):
        return self.id != other.id
    def is_const(self): # for backward compatibility with custom resolvers
        return self.is_constant
        
class Fresh_var(Term): 
    """ a variable created by the search algorithm """
    __slots__ = ['id']
    counter = util.Counter()
    is_constant = False # it's faster than is_const()
    def __init__(self):
        self.id = ('f', Fresh_var.counter.next()) #id
    
    if not util.PY2: # do not slow down python 2
        def __hash__(self): # needed for Python 3
            return id(self.id) # id() is fastest

    def is_const(self):
        return False
    def get_tag(self, env): #id
        return env.setdefault(self, ('v', len(env)))

    def subst(self, env): #unify
        return env.get(self, self)
    def shuffle(self, env): #shuffle
        env.setdefault(self, Fresh_var())
    def chase(self, env): #unify
        # try ... except is not faster
        return env[self].chase(env) if self in env else self
    
    def unify(self, term, env): #unify
        return term.unify_with_var(self, env)
    def unify_with_const(self, const, env): #unify
        env[self] = const
        return env
    def unify_with_var(self, var, env): #unify
        env[var] = self
        return env
    def unify_with_tuple(self, a_tuple, env): #unify
        env[self] = a_tuple
        return env
    
    def match(self, const, env):
        if self not in env:
            env[self] = const
            return env
        elif env[self] == const: # dead code ?
            return env
        else: # dead code ?
            return None
    
    def __str__(self): 
        return "variable_%s" % self.id[1]
    def equals_primitive(self, term, subgoal):
        """ by default, self==term fails """
        return None


class Var(Fresh_var):
    """ A variable in a clause or query """
    __slots__ = ['id']
    def __init__(self, name):
        self.id = ('f', name) #id

    def __hash__(self):
        return hash(self.id)

    def __str__(self): 
        return self.id[1]


class Const(Term):
    """ a constant """
    __slots__ = ['id']
    is_constant = True

    def __init__(self, _id):
        self.id = _id
    
    def __hash__(self):
        return hash(self.id)

    def is_const(self): # for backward compatibility with custom resolvers
        return True

    def get_tag(self, env): #id
        return self.id
    
    def subst(self, env): #unify
        return self
    def shuffle(self, env): #shuffle
        pass
    def chase(self, env): #unify
        return self
    
    def unify(self, term, env): #unify
        return term.unify_with_const(self, env)
    def unify_with_const(self, const, env): #unify
        return None
    def unify_with_var(self, var, env): #unify
        return var.unify_with_const(self, env)
    def unify_with_tuple(self, a_tuple, env): #unify
        return None
    
    def match(self, const, env):
        return None
    
    def __str__(self): 
        return "'%s'" % self.id
    def equals_primitive(self, term, subgoal):
        if self == term:          # Both terms are constant and equal.
            literal = Literal("==", (self, self))
            return fact(subgoal, literal)


class VarTuple(Term):
    """ a tuple / list of variables, constants or tuples """
    __slots__ = ['_id', 'id', 'is_constant']
    
    def __init__(self, _id):
        self._id = _id
        self.id =  tuple(e.id for e in _id) #id
        self.is_constant = all(element.is_constant for element in _id)
    
    def __hash__(self): # no gain by caching it
        return hash(self.id)

    def __len__(self):
        return len(self._id)        
    
    def get_tag(self, env): #id
        if self.is_constant: # can use lazy property only for constants
            return self.id
        return tuple(t.get_tag(env) for t in self._id) #id
    
    def subst(self, env): #unify
        if self.is_constant: # can use lazy property only for constants
            return self
        return VarTuple(tuple(element.subst(env) for element in self._id))
    def shuffle(self, env): #shuffle
        if not self.is_constant:
            for element in self._id:
                element.shuffle(env)
    def chase(self, env): #unify
        if self.is_constant:
            return self
        return VarTuple(tuple(element.chase(env) for element in self._id))
    
    def unify(self, term, env): #unify
        return term.unify_with_tuple(self, env)
    def unify_with_const(self, const, env): #unify
        return None
    def unify_with_var(self, var, env): #unify
        return var.unify_with_tuple(self, env)
    def unify_with_tuple(self, a_tuple, env): #unify
        if len(self) != len(a_tuple):
            return None
        for e1, e2 in zip(self._id, a_tuple._id):
            if e1 != e2:
                env = e1.unify(e2, env)
                if env == None: return env
        return env
    
    # def match is not needed here

    def __str__(self): 
        return "'%s'" % str([str(e) for e in self.id])
    def equals_primitive(self, term, subgoal):
        if self == term:          # Both terms are constant and equal.
            literal = Literal("==", (self, self))
            return fact(subgoal, literal)


class Operation(object):
    """ an arithmetic operation, a slice or a lambda """
    counter = util.Counter()
    def __init__(self, lhs, operator, rhs):
        self.operator = operator
        self.operator_id = 'l' + str(Operation.counter.next()) if isinstance(self.operator, type(lambda: None)) else str(self.operator)
        self.lhs = lhs
        self.rhs = rhs
        self.is_constant = False
        self.id = (self.lhs.id, self.operator_id, self.rhs.id) #id
    
    def get_tag(self, env): #id
        return (self.lhs.get_tag(env), self.operator_id, self.rhs.get_tag(env))
    
    def subst(self, env): #unify
        lhs = self.lhs.subst(env)
        rhs = self.rhs.subst(env)
        try:
            if self.operator == '[' and isinstance(lhs, (VarTuple, Const)) and rhs.is_constant:
                v = lhs._id if isinstance(lhs, VarTuple) else lhs.id
                if isinstance(rhs, VarTuple): # a slice
                    return Term.of(v.__getitem__(slice(*rhs.id)))
                return Term.of(v.__getitem__(rhs.id))
            if self.operator == '#' and isinstance(rhs, VarTuple):
                return Term.of(len(rhs))
            if self.operator == '..' and rhs.is_constant:
                return Term.of(range(rhs.id))
            if lhs.is_constant and rhs.is_constant:
                # calculate expression of constants
                if self.operator == '+':
                    return Term.of(lhs.id + rhs.id)
                elif self.operator == '-':
                    return Term.of(lhs.id - rhs.id)
                elif self.operator == '*':
                    return Term.of(lhs.id * rhs.id)
                elif self.operator == '/':
                    return Term.of(lhs.id / rhs.id)
                elif self.operator == '//':
                    return Term.of(lhs.id // rhs.id)
                elif self.operator == '**':
                    return Term.of(lhs.id ** rhs.id)
                elif self.operator == '%':
                    return Term.of(lhs.id.format(*(rhs.id)))
                elif isinstance(self.operator, type(lambda: None)):
                    return Term.of(self.operator(*(rhs.id)))
                elif self.operator == '.':
                    v = lhs.id
                    for attribute in rhs.id.split(".") :
                        v = getattr(v, attribute)
                    return Term.of(v)
                elif self.operator == '(':
                    return Term.of(lhs.id.__call__(*(rhs.id)))
                assert False # dead code
            return Operation(lhs, self.operator, rhs)
        except Exception as e:
            return Term.of(e)
            
    def shuffle(self, env): #shuffle
        self.lhs.shuffle(env)
        self.rhs.shuffle(env)

    def chase(self, env): #unify
        return Operation(self.lhs.chase(env), self.operator, self.rhs.chase(env))
    
    def unify(self, term, env): #unify
        return None
    def unify_with_const(self, const, env): #unify
        return None
    def unify_with_var(self, var, env): #unify
        return None
    def unify_with_tuple(self, a_tuple, env): #unify
        return None

    def __str__(self): 
        return "(%s%s%s)" % (str(self.lhs), str(self.operator), str(self.rhs))

class Interned(object):
    """ Abstract class for objects having only one instance in memory """
    notFound = object()
    def __eq__(self, other):
        return self is other
    def __hash__(self): 
        return id(self)
    def __ne__(self, other):
        return not self is other

class Pred(Interned):
    """ A predicate symbol has a name, an arity, and a database table.  
        It can also have a function used to implement a primitive."""
    lock = threading.RLock()
    def __new__(cls, pred_name, arity):
        assert isinstance(pred_name, util.string_types)
        _id = '%s/%i' % (pred_name, arity)
        with Pred.lock:
            o = Logic.tl.logic.Pred_registry.get(_id, Interned.notFound)
            if o is Interned.notFound: 
                o = object.__new__(cls) # o is the ref that keeps it alive
                o.id = _id
                o.name = pred_name
                o.arity = arity
                o.prearity = None
                words = o.name.split('.')
                o.prefix, o.suffix = (words[0], words[1].split('[')[0]) if 1 < len(words) else ('','')
                words = pred_name.split(']')
                o.comparison = words[1] if 1 < len(words) else '' # for f[X]<Y
    
                o.db = OrderedDict()
                o.clauses = OrderedDict()
                # one index per term. An index is a dictionary of sets
                o.index = [{} for i in range(int(o.arity))]
                o.prim = None
                o.expression = None
                Logic.tl.logic.Pred_registry[_id] = o
        return o
    
    @classmethod
    def is_known(cls, pred_id):
        #TODO look at _pyD_ methods in classes, to detect more
        prefix = pred_id.split('.')[0] # we assumes it has a '.'
        if prefix in Class_dict:
            for cls in Class_dict[prefix].__mro__:
                rebased = pred_id.replace(prefix, cls.__name__, 1) # only the first occurrence
                if rebased in Logic.tl.logic.Db:
                    return True
        return False        
    
    def _class(self):
        """determine the python class for a prefixed predicate (and caches it)"""
        # cannot be done at initialization, because the global Class_dict is not filled in yet
        if not hasattr(self, '_cls'): 
            self._cls = Class_dict.get(self.prefix, '') if self.prefix else None
        return self._cls
    
    def parent_classes(self):
        " iterator of the parent classes that have pyDatalog capabilities"
        if self._class():
            for cls in self._cls.__mro__:
                if cls.__name__ in Class_dict and cls.__name__ not in ('Mixin', 'object'):
                    yield cls
        else:
            yield None
    
    def reset_clauses(self):
        """ clears the database of clauses for the predicate """
        for clause in self.clauses.values():
            retract(clause)
    
    def __str__(self): 
        return "%s()" % self.name


class Literal(object):
    """ A literal is a predicate and a sequence of terms, 
        the number of which must match the predicate's arity.
    """
    __slots__ = ['terms', 'pred', 'id', 'tag', 'aggregate']
    def __init__(self, pred, terms, prearity=None, aggregate=None):
        self.terms = terms
        self.aggregate = aggregate
        if isinstance(pred, util.string_types):
            self.pred = Pred(pred, len(terms))
            self.pred.prearity = len(terms) if prearity is None else prearity
            if pred.startswith('~'): #pred
                self.pred.base_pred = Pred(pred[1:], len(terms))
                self.pred.base_pred.prearity = self.pred.prearity
        else:
            self.pred = pred
            # TODO assert self.pred.prearity == (prearity or len(terms)), "Error: Incorrect mix of predicates and functions : %s" % str(self)
    
    def _renamed(self, new_name):
        _id = '%s/%i' % (new_name, len(self.terms))
        pred= Logic.tl.logic.Pred_registry.get(_id, new_name)
        return Literal(pred, list(self.terms), prearity=self.pred.prearity, aggregate=self.aggregate)
        
    def rebased(self, parent_class): 
        """ returns a literal prefixed by parent class """
        pred = self.pred
        if not parent_class or not pred._class() or parent_class == pred._class(): 
            return self
        return self._renamed(re.sub('^((~)?)'+pred._class().__name__+'.', r'\1'+parent_class.__name__+'.', pred.name))
    
    def equalized(self): 
        """ returns a new literal with '==' instead of comparison """
        return self._renamed(self.pred.name.replace(self.pred.comparison, '=='))
    
    def subst_first(self, env): #prefixed
        """ substitute the first term of prefixed literal, using env """
        if self.pred.prefix:
            self.terms[0] = self.terms[0].subst(env)
                
    def __str__(self): 
        return "%s(%s)" % (self.pred.name, ','.join([str(term) for term in self.terms])) 

    def get_id(self): #id
        """ The id's encoding ensures that two literals are structurally the
            same if they have the same id.  """
        if not hasattr(self, 'id'): # cached
            self.id = (self.pred.id,) + tuple(term.id for term in self.terms)
        return self.id        

    def get_fact_id(self): #id
        """ The id of a known fact is limited by its prearity
            Prearity is used to ensure unicity of results of functions like pred[2]==1 """
        return self.get_id()[:1+self.pred.prearity]

    def get_tag(self): #id
        """ the tag is used as a key by the subgoal table """
        if not hasattr(self, 'tag'): # cached
            env = {}
            self.tag = (self.pred.id,) + tuple(term.get_tag(env) for term in self.terms)
        return self.tag       

    def subst(self, env): #unify
        if not env: return self
        return Literal(self.pred, [term.subst(env) for term in self.terms], aggregate=self.aggregate)

    def shuffle(self, env): #shuffle
        for term in self.terms:
            term.shuffle(env)

    def rename(self): #shuffle
        env={}
        self.shuffle(env)
        return self.subst(env)

    def unify(self, other): #unify
        if self.pred != other.pred: return None
        env = {}
        for term, otherterm in zip(self.terms, other.terms):
            literal_i = term.chase(env)
            other_i = otherterm.chase(env)
            if literal_i != other_i:
                env = literal_i.unify(other_i, env)
                if env == None: return env
        return env

    def match(self, fact):
        """ Does a fact unify with a fact known to contain only constant terms? """
        env = {}
        for term, factterm in zip(self.terms, fact.terms):
            if term != factterm:
                env = term.match(factterm, env)
                if env == None: return env
        return env


class Clause(object):
    """ A clause asserts that its head is true if every literal in its body is
        true.  If there are no literals in the body, the clause is a fact
    """
    def __init__(self, head, body):
        self.head = head
        self.body = body
    def __str__(self):  
        return "%s <= %s" % (str(self.head), '&'.join([str(literal) for literal in self.body]))
    def __neg__(self):
        """retract clause"""
        retract(self) 

    def get_id(self): #id
        """ The id's encoding ensures that two clauses are structurally equal
            if they have the same id.  A clause's id is used as a key into the
            clause database. """
        if not hasattr(self, 'id'): # cached
            if not self.body: #if it is a fact
                self.id = (self.head.get_fact_id(),)
            else:
                self.id = (self.head.get_id(),) + tuple(bodi.get_id() for bodi in self.body)
        return self.id
    
    def subst(self, env, parent_class=None):
        """ apply the env mapping and rebase to parent_class, if any """
        if not env and not parent_class: return self
        if not parent_class:
            return Clause(self.head.subst(env),
                           [bodi.subst(env) for bodi in self.body])
        return Clause(self.head.subst(env).rebased(parent_class),
                        [bodi.subst(env).rebased(parent_class) for bodi in self.body])
    
    def rename(self):
        """ returns the clause with fresh variables """
        env = {}
        self.head.shuffle(env)
        for bodi in self.body:
            bodi.shuffle(env)
        return self.subst(env)

def add_class(cls, name):
    """ Update the list of pyDatalog-capable classes, and update clauses accordingly"""
    Class_dict[name] = cls
    #prefixed replace the first term of each functional comparison literal for that class..
    env = {Var(name): Const('_pyD_class')}
    for pred in Logic.tl.logic.Db.values():
        for clause in pred.db.values():
            clause.head.subst_first(env)
            for literal in clause.body:
                literal.subst_first(env)


# DATABASE  #####################################################

# The database stores predicates that contain clauses.  

def insert(pred):
    Logic.tl.logic.Db[pred.id] = pred
    return pred

def remove(pred):
    if pred.id in Logic.tl.logic.Db : 
        del Logic.tl.logic.Db[pred.id]
    return pred
    
def assert_(clause):
    """ Add a safe clause to the database """
    pred = clause.head.pred

    if not pred.prim:                   # Ignore assertions for primitives.
        id_ = clause.get_id()
        retract(clause) # to ensure unicity of functions
        pred.db[id_] = clause
        if not clause.body: # if it is a fact, update indexes
            for i, term in enumerate(clause.head.terms):
                clauses = pred.index[i].setdefault(term.id, set()) # create a set if needed
                clauses.add(clause)
        else:
            pred.clauses[id_] = clause
        insert(pred)
    return clause

def retract(clause):
    """ retract a clause from the database"""
    pred = clause.head.pred
    id_ = clause.get_id()
    
    if id_ in pred.db: 
        if not clause.body: # if it is a fact, update indexes
            clause = pred.db[id_] # make sure it is identical to the one in the index
            for i, term in enumerate(clause.head.terms):
                pred.index[i][term.id].remove(clause)
                # TODO del pred.index[i][term] if the set is empty
        else:
            del pred.clauses[id_]
        del pred.db[id_]  # remove clause from pred.db
    # delete it completely if it's a temporary query predicate
    if pred.name.startswith('_pyD_query') and len(pred.db) == 0 and pred.prim == None:
        remove(pred)
    return clause
    
def relevant_clauses(literal):
    """ returns matching clauses for a literal query, using index """
    result = None
    for i, term in enumerate(literal.terms):
        if term.is_constant:
            facts = literal.pred.index[i].get(term.id, set()) # default : a set
            result = facts if result == None else result.intersection(facts)
    if result == None: # no constants found
        return list(literal.pred.db.values())
    else:
        return list(literal.pred.clauses.values()) + list(result)
    
"""
The remaining functions in this file is based on the tabled logic
programming algorithm described in "Efficient Top-Down Computation of
Queries under the Well-Founded Semantics", Chen, W., Swift, T., and
Warren, D. S., J. Logic Prog. Vol. 24, No. 3, pp. 161-199.  Another
important reference is "Tabled Evaluation with Delaying for General
Logic Programs", Chen, W., and Warren, D. S., J. ACM, Vol. 43, No. 1,
Jan. 1996, pp. 20-74.

It should be noted that a simplified version of the algorithm of 
"Efficient top-down computation" is implemented : negations are  
supported with another algorithm. 
"""

# The subgoal table is a map from the tag of a subgoal's
# literal to a subgoal.

def find(literal):
    tag = literal.get_tag()
    return Logic.tl.logic.Subgoals.get(tag)


class Subgoal(object):
    """
    A subgoal has a literal, a set of facts, and an array of waiters.
    A waiter is a pair containing a subgoal and a clause.
    """
    def __init__(self, literal):
        self.literal = literal
        self.facts = {}
        self.waiters = []
        # subgoal is done when a partial literal is true 
        # or when one fact is found for a function of constants
        self.is_done = False
    

def resolve(clause, literal):
    """
    Resolve the selected literal of a clause with a literal.  The
    selected literal is the first literal in body of a rule.  If the
    two literals unify, a new clause is generated that has a body with
    one less literal.
    """
    env = clause.body[0].unify(literal.rename())
    return Clause(clause.head.subst(env), [bodi.subst(env) for bodi in clause.body[1:] ])
 
################# Task management ###############################

# A stack of thunks is used to avoid the stack overflow problem
# by delaying the evaluation of some functions

# op codes
SEARCH = 1
ADD_CLAUSE = 2

# Schedule a task for later invocation

class Thunk(object):
    def __init__(self, thunk):
        self.thunk = thunk
    def do(self):
        self.thunk()
        
def schedule(task):
    if not isinstance(task, Thunk) and task[0] is SEARCH:
        Logic.tl.logic.Subgoals[task[1].literal.get_tag()] = task[1]
    return Logic.tl.logic.Tasks.append(task)

def complete(subgoal, post_thunk):
    """makes sure that thunk() is completed before calling post_thunk and resuming processing of other thunks"""
    Ts = Logic.tl.logic
    Ts.Stack.append((Ts.Subgoals, Ts.Tasks, Ts.Goal)) # save the environment to the stack. Invoke will eventually do the Stack.pop().
    Ts.Subgoals, Ts.Tasks, Ts.Goal = {}, list(), subgoal
    schedule((SEARCH, subgoal))
    # prepend post_thunk at one level lower in the Stack, 
    # so that it is run immediately by invoke() after the search() thunk is complete
    if Logging: logging.debug('push')
    Ts.Stack[-1][1].append(Thunk(post_thunk)) 

def ask(literal):
    """ Invoke the tasks. Each task may append new tasks on the schedule."""
    Ts = Logic.tl.logic
    Ts.Tasks, Ts.Subgoals, Ts.Goal = list(), {}, Subgoal(literal)
    schedule((SEARCH, Ts.Goal))
    while (Ts.Tasks or Ts.Stack) and not Ts.Goal.is_done:
        while Ts.Tasks and not Ts.Goal.is_done:
            todo = Ts.Tasks.pop()
            if isinstance(todo, Thunk):
                todo.do() # get the thunk and execute it
            elif todo[0] is SEARCH:
                search(todo[1])
            elif todo[0] is ADD_CLAUSE:
                add_clause(todo[1], todo[2])
        if Ts.Stack: 
            Ts.Subgoals, Ts.Tasks, Ts.Goal = Ts.Stack.pop()
            if Logging: logging.debug('pop')

    Ts.Tasks, Ts.Subgoals = None, {}
    if Ts.Goal.facts is True:
        return True
    return [ tuple(term.id for term in literal.terms) for literal in list(Ts.Goal.facts.values())]    
Literal.ask = ask

################## add derived facts and use rules ##############

def fact(subgoal, literal):
    """ 
    Store a derived fact, and inform all waiters of the fact too. 
    SLG_ANSWER in the reference article
    """
    if isinstance(literal, Literal) and not all(t.is_constant for t in literal.terms):
        literal = True # a partial fact is True
    if literal is True:
        if subgoal.facts != True: # if already True, do not advise its waiters again
            if Logging: logging.info("New fact : %s is True" % str(subgoal.literal))
            subgoal.facts, subgoal.is_done = True, True
            for waiter in subgoal.waiters:
                resolvent = Clause(waiter.clause.head, waiter.clause.body[1:])
                schedule((ADD_CLAUSE, waiter.subgoal, resolvent))
    elif subgoal.facts is not True and not subgoal.facts.get(literal.get_fact_id()):
        if Logging: logging.info("New fact : %s" % str(literal))
        subgoal.facts[literal.get_fact_id()] = literal
        for waiter in subgoal.waiters:
            resolvent = resolve(waiter.clause, literal)
            if resolvent != None:
                schedule((ADD_CLAUSE, waiter.subgoal, resolvent))
        if len(subgoal.facts)==1 \
        and all(subgoal.literal.terms[i].is_constant 
                for i in range(subgoal.literal.pred.prearity)):
            subgoal.is_done = True # one fact for a function of constant

def fact_candidate(subgoal, class0, result):
    """ add result as a candidate fact of class0 for subgoal"""
    if result is True:
        return fact(subgoal, True)
    result = [Term.of(r) for r in result]
    if len(result) != len(subgoal.literal.terms):
        return
    if class0 and result[1].id and not isinstance(result[1].id, class0): #prefixed
        return
    result = Literal(subgoal.literal.pred.name, result)
    if subgoal.literal.match(result) != None:
        fact(subgoal, result)

class Waiter(object):
    def __init__(self, subgoal, clause):
        self.subgoal = subgoal
        self.clause = clause
        
def rule(subgoal, clause, selected):
    """ 
    Use a newly derived rule. 
    SLG_POSITIVE in the reference article
    """
    sg = find(selected)
    if sg != None:
        sg.waiters.append(Waiter(subgoal, clause))
        todo = []
        if sg.facts is True:
            resolvent = Clause(clause.head, clause.body[1:])
            schedule((ADD_CLAUSE, subgoal, resolvent))
        else:
            for fact in sg.facts.values():
                resolvent = resolve(clause, fact)
                if resolvent != None: 
                    schedule((ADD_CLAUSE, subgoal, resolvent))
    else:
        sg = Subgoal(selected)
        sg.waiters.append(Waiter(subgoal, clause))
        return schedule((SEARCH, sg))
    
def add_clause(subgoal, clause):
    """ SLG_NEWCLAUSE in the reference article """
    if subgoal.is_done or Logic.tl.logic.Goal.is_done:
        return # no need to keep looking if THE answer is found already
    if not clause.body:
        return fact(subgoal, clause.head)
    else:
        return rule(subgoal, clause, clause.body[0])
    
###############     SEARCH     ##################################

def search(subgoal):
    """ 
    Search for derivations of the literal associated with this subgoal 
    aka SLG_SUBGOAL in the reference article
    """
    literal0 = subgoal.literal
    class0 = literal0.pred._class()
    terms = literal0.terms
    
    if class0 and terms[1].is_constant and terms[1].id is None: return
    if hasattr(literal0.pred, 'base_pred'): # this is a negated literal
        if Logging: logging.debug("pyDatalog will search negation of %s" % literal0)
        base_literal = Literal(literal0.pred.base_pred, terms)
        """ the rest of the processing essentially performs the following, 
        but in its own environment, and with precautions to avoid stack overflow :
            result = ask(base_literal)
            if result is None or 0 == len(result.answers):
                return fact(subgoal, literal)
        """
        #TODO check that literal is not one of the subgoals already in the stack, to prevent infinite loop
        # example : p(X) <= ~q(X); q(X) <= ~ p(X); creates an infinite loop
        base_subgoal = Subgoal(base_literal)
        complete(base_subgoal,
                lambda base_subgoal=base_subgoal, subgoal=subgoal:
                    fact(subgoal, True) if not base_subgoal.facts else None)
        return
    
    for _class in literal0.pred.parent_classes():
        literal = literal0.rebased(_class)
        
        if Python_resolvers:
            resolver = literal.pred.id if literal.pred.id in Python_resolvers \
                    else literal.pred.id.replace(r'/', str(literal.pred.arity)+r"/")
            if resolver in Python_resolvers:
                if Logging : logging.debug("pyDatalog uses python resolvers for %s" % literal)
                for result in Python_resolvers[resolver](*terms):
                    fact_candidate(subgoal, class0, result)
                return
        if _class: 
            # TODO add special method for custom comparison
            method_name = '_pyD_%s%i' % (literal.pred.suffix, int(literal.pred.arity - 1)) #prefixed
            if literal.pred.suffix and method_name in _class.__dict__:
                if Logging : logging.debug("pyDatalog uses class resolvers for %s" % literal)
                for result in getattr(_class, method_name)(*(terms[1:])): 
                    fact_candidate(subgoal, class0, (terms[0],) + result)
                return        
            try: # call class._pyD_query
                resolver = _class._pyD_query
                if not _class.has_SQLAlchemy : gc.collect() # to make sure pyDatalog.metaMixin.__refs__[cls] is correct
                for result in resolver(literal.pred.name, terms[1:]):
                    fact_candidate(subgoal, class0, (terms[0],) + result)
                if Logging : logging.debug("pyDatalog has used _pyD_query resolvers for %s" % literal)
                return
            except:
                pass
        if literal.pred.prim: # X==Y, X < Y+Z
            if Logging : logging.debug("pyDatalog uses comparison primitive for %s" % literal)
            literal.pred.prim(literal, subgoal)
            return
        elif literal.aggregate:
            if Logging : logging.debug("pyDatalog uses aggregate primitive for %s" % literal)
            base_terms = list(terms[:-1])
            for i in literal.aggregate.slice_to_variabilize:
                base_terms[i] = Fresh_var()
            base_literal = Literal(literal.pred.name, base_terms) # without aggregate to avoid infinite loop
            base_subgoal = Subgoal(base_literal)
            complete(base_subgoal,
                    lambda base_subgoal=base_subgoal, subgoal=subgoal, aggregate=literal.aggregate:
                        aggregate.complete(base_subgoal, subgoal))
            return
        elif literal.pred.id in Logic.tl.logic.Db: # has a datalog definition, e.g. p(X), p[X]==Y
            for clause in relevant_clauses(literal):
                renamed = clause.rename()
                env = literal.unify(renamed.head)
                if env != None:
                    clause = renamed.subst(env, class0)
                    if Logging : logging.debug("pyDatalog will use clause : %s" % clause)
                    schedule((ADD_CLAUSE, subgoal, clause))
            return
        elif literal.pred.comparison: # p[X]<=Y => consider translating to (p[X]==Y1) & (Y1<Y)
            literal1 = literal.equalized()
            if literal1.pred.id in Logic.tl.logic.Db: # equality has a datalog definition
                Y1 = Fresh_var()
                literal1.terms[-1] = Y1
                literal2 = Literal(literal.pred.comparison, (Y1, terms[-1]))
                clause = Clause(literal, (literal1, literal2))
                renamed = clause.rename()
                env = literal.unify(renamed.head)
                if env != None:
                    renamed = renamed.subst(env, class0)
                    if Logging : logging.debug("pyDatalog will use clause for comparison: %s" % renamed)
                    schedule((ADD_CLAUSE, subgoal, renamed))
                return
            
    if class0: # a.p[X]==Y, a.p[X]<y, to access instance attributes
        try: 
            resolver = class0.pyDatalog_search
            if not class0.has_SQLAlchemy : gc.collect() # to make sure pyDatalog.metaMixin.__refs__[cls] is correct
            if Logging : logging.debug("pyDatalog uses pyDatalog_search for %s" % literal)
            for result in resolver(literal):
                fact_candidate(subgoal, class0, result)
            return
        except AttributeError:
            pass
    elif literal.pred.comparison and len(terms)==3 and terms[0].is_constant \
    and terms[0].id != '_pyD_class' and terms[1].is_constant: # X.a[1]==Y
        # do not use pyDatalog_search as the variable may not be in a pyDatalog class
        v = getattr(terms[0].id, literal.pred.suffix)
        if isinstance(terms[1], VarTuple): # a slice
            v = v.__getitem__(slice(*terms[1].id))
        else:
            v = v.__getitem__(terms[1].id)
        if terms[2].is_constant and compare(v, literal.pred.comparison, terms[2].id):
            fact_candidate(subgoal, class0, (terms[0], terms[1], terms[2]))
        elif literal.pred.comparison == "==" and not terms[2].is_constant:
            fact_candidate(subgoal, class0, (terms[0], terms[1], v))
        else:
            raise util.DatalogError("Error: right hand side of comparison must be bound: %s" 
                                % literal.pred.id, None, None)
        return

    raise AttributeError("Predicate without definition (or error in resolver): %s" % literal.pred.id)
            

# PRIMITIVES   ##################################################

"""

A primitive predicate, also called a built-in predicate, is
implemented by code.  Assertions about a primitive predicate are
ignored, as the code takes precedence.  

The behavior of a primitive predicate is defined by adding a function
to the predicate's prim field.  The function takes a literal and a
subgoal.  The typical primitive derives a set of facts from the
literal, and for each derived fact f, reports the result by invoking
fact(subgoal, f).

The equals primitive defined below is protected from garage collection
because the primitive is bound to a local variable.  A primitive not
stored in a Lua data structure can be protected by entering it into
the predicate database used by assert and retract.  For primitives
created from C, protection may be provided by entering the predicate
into the Lua registry.

Use the add_iter_prim function to add a primitive predicate that can
defined by an iterator which when given a literal, generates a
sequence of answers, each answer being an array of strings or numbers.

"""

def equals_primitive(literal, subgoal):
    x = literal.terms[0]
    y = literal.terms[1]
    env = x.unify(y, {})# Both terms must unify,
    if env != None:                     # and at least one of them
        x = x.subst(env)          # must be a constant.
        y = y.subst(env)
    #unbound: can't raise error if both are still unbound, because split(a,b,c) would fail (see test.py)
    return x.equals_primitive(y, subgoal)

def compare_primitive(literal, subgoal):
    x = literal.terms[0]
    y = literal.terms[1]
    if not x.is_constant:
        if literal.pred.name == '_pyD_in':
            if not y.is_const:
                raise util.DatalogError("Error: right hand side must be bound: %s" % literal, None, None)
            for v in y.id:
                literal = Literal(literal.pred.name, (Term.of(v), y))
                fact(subgoal, literal)
        else:
            raise util.DatalogError("Error: left hand side of comparison must be bound: %s" 
                                    % literal.pred.id, None, None)
    elif y.is_constant:
        if compare(x.id, literal.pred.name, y.id):
            fact(subgoal, True)
    else:
        raise util.DatalogError("Error: right hand side of comparison must be bound: %s" 
                                % literal.pred.id, None, None)

# generic comparison function
def compare(l,op,r):
    return l in r if op=='_pyD_in' else l not in r if op=='_pyD_not_in' else l==r if op=='==' else l!=r if op=='!=' else l<r if op=='<' \
        else l<=r if op=='<=' else l>=r if op=='>=' else l>r if op=='>' else None
def compare2(l,op,r):
    return l._in(r) if op=='_pyD_in' else l._not_in(r) if op=='_pyD_not_in' else compare(l,op,r)

def clear():
    """ clears the logic """
    Logic.tl.logic.Db = {}
    Logic.tl.logic.Pred_registry = weakref.WeakValueDictionary()
    Logic.tl.logic.Subgoals = {}
    Logic.tl.logic.Tasks = None
    Logic.tl.logic.Stack = []
    Logic.tl.logic.Goal = None       

    insert(Pred("==", 2)).prim = equals_primitive
    insert(Pred("<" , 2)).prim = compare_primitive
    insert(Pred("<=", 2)).prim = compare_primitive
    insert(Pred("!=", 2)).prim = compare_primitive
    insert(Pred(">=", 2)).prim = compare_primitive
    insert(Pred(">" , 2)).prim = compare_primitive
    insert(Pred("_pyD_in", 2)).prim = compare_primitive
    insert(Pred("_pyD_not_in", 2)).prim = compare_primitive
