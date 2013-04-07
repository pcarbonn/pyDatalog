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
from lua to python, with some enhancements:
* indexing of facts,
* support for lambda
* evaluation of arithmetic expression and comparison.

See also doc.py for additional source code documentation.

Some notable differences between python and lua:
* lua indices start at 1, not 0
* any variable is true in lua if it is not nil or false.
* lua tables contain both a list and a dictionary --> need to change them to objects
* lua variables are global by default, python ones are local by default
* variable bindings in a closure.  See http://tiny.cc/7837cw, http://tiny.cc/rq47cw
"""
from collections import deque
from decimal import Decimal
import gc
from itertools import groupby
import logging
import re
import six
from six.moves import xrange
import weakref

try:
    from . import util
except ValueError:
    import util

Logging = False # True --> logging is activated.  Kept for performance reason
Auto_print = False # True => automatically prints the result of a query

Python_resolvers = {} # dictionary  of python functions that can resolve a predicate

#       DATA TYPES          #####################################

# Internalize objects based on an identifier.

# To make comparisons between items of the same type efficient, each
# item is internalized so there is at most one of them associated
# with an identifier.

# For example, after internalization, there is one constant for each
# string used to name a constant.

class Interned(object):
    """ Abstract class for objects having only one instance in memory """
    @classmethod
    def of(cls, atom):
        """ factory function for interned objects """
        if isinstance(atom, (Interned, Fresh_var, Operation)):
            return atom
        elif isinstance(atom, (list, tuple, xrange)):
            return VarTuple(tuple([Interned.of(element) for element in atom]))
        else:
            return Const(atom)
    notFound = object()
    def __eq__(self, other):
        return self is other
    def __hash__(self): 
        return id(self)
    def __ne__(self, other):
        return not self is other
    def is_const(self): # for backward compatibility with custom resolvers
        return self.is_constant


class Fresh_var(object): 
    """ a variable created by the search algorithm """
    counter = util.Counter()  
    def __init__(self):
        self.id = 'f' + str(Fresh_var.counter.next()) #id
        self.key = self.id #id
    
    def is_const(self):
        return False
    is_constant = False
    def get_tag(self, env): #id
        return env.setdefault(self, 'v%i' % len(env))

    def subst(self, env): #unify
        return env.get(self, self)
    def shuffle(self, env): #shuffle
        env.setdefault(self, Fresh_var())
    def chase(self, env): #unify
        return env[self].chase(env) if self in env else self
    
    def unify(self, term, env): #unify
        return term.unify_var(self, env)
    def unify_const(self, const, env): #unify
        env[self] = const
        return env
    def unify_var(self, var, env): #unify
        env[var] = self
        return env
    def unify_tuple(self, tuple, env): #unify
        env[self] = tuple
        return env
    
    def __str__(self): 
        return "variable_%s" % self.id
    def equals_primitive(self, term, subgoal):
        """ by default, self==term fails """
        return None


class Var(Fresh_var, Interned):
    """ A variable in a clause or query """
    registry = weakref.WeakValueDictionary()
    counter = util.Counter()
    def __new__(cls,  _id):
        o = cls.registry.get(_id, Interned.notFound)
        if o is Interned.notFound:
            o = object.__new__(cls) # o is the ref that keeps it alive
            o.id = _id #id
            o.key = 'v' + str(Var.counter.next()) #id
            cls.registry[_id] = o
        return o
    def __init__(self, name):
        pass
    def __str__(self): 
        return self.id 


class Const(Interned):
    """ a constant """
    registry = weakref.WeakValueDictionary()
    counter = util.Counter()
    def __new__(cls,  _id):
        r = repr(_id) if isinstance(_id, (six.string_types, float, Decimal)) else _id
        o = cls.registry.get(r, Interned.notFound)
        if o is Interned.notFound: 
            o = object.__new__(cls) # o is the ref that keeps it alive
            o.id = _id #id
            o.key = 'c' + str(Const.counter.next())
            cls.registry[r] = o
        return o
    is_constant = True
    def get_tag(self, env): #id
        return self.key
    
    def subst(self, env): #unify
        return self
    def shuffle(self, env): #shuffle
        pass
    def chase(self, env): #unify
        return self
    
    def unify(self, term, env): #unify
        return term.unify_const(self, env)
    def unify_const(self, const, env): #unify
        return None
    def unify_var(self, var, env): #unify
        return var.unify_const(self, env)
    def unify_tuple(self, tuple, env): #unify
        return None
    
    def __str__(self): 
        return "'%s'" % self.id
    def equals_primitive(self, term, subgoal):
        if self == term:          # Both terms are constant and equal.
            literal = Literal("==", (self, self))
            return fact(subgoal, literal)

class VarTuple(Interned):
    """ a tuple / list of variables, constants or tuples """
    registry = weakref.WeakValueDictionary()
    def __new__(cls,  _id):
        o = cls.registry.get(_id, Interned.notFound)
        if o is Interned.notFound: 
            o = object.__new__(cls) # o is the ref that keeps it alive
            o._id = _id #id
            o.key = '('+ ''.join([e.key for e in _id]) + ')' #id
            o.id = tuple([element.id for element in _id])
            o.is_constant = all(element.is_constant for element in _id)
            cls.registry[_id] = o
        return o
    
    def __len__(self):
        return len(self._id)        
    
    def get_tag(self, env): #id
        if self.is_constant: # can use lazy property only for constants
            return self.key
        return repr([t.get_tag(env) for t in self._id])#TODO
    
    def subst(self, env): #unify
        if self.is_constant: # can use lazy property only for constants
            return self
        return VarTuple(tuple([element.subst(env) for element in self._id]))
    def shuffle(self, env): #shuffle
        if not self.is_constant:
            for element in self._id:
                element.shuffle(env)
    def chase(self, env): #unify
        if self.is_constant:
            return self
        return VarTuple(tuple([element.chase(env) for element in self._id]))
    
    def unify(self, term, env): #unify
        return term.unify_tuple(self, env)
    def unify_const(self, const, env): #unify
        return None
    def unify_var(self, var, env): #unify
        return var.unify_tuple(self, env)
    def unify_tuple(self, tuple, env): #unify
        if len(self) != len(tuple):
            return None
        for e1, e2 in zip(self._id, tuple._id):
            if e1 != e2:
                env = e1.unify(e2, env)
                if env == None: return env
        return env

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
        self.id = "(%s%s%s)" % (self.lhs.id, self.operator_id, self.rhs.id) #id
        self.key = "(%s%s%s)" % (self.lhs.key, self.operator_id, self.rhs.key) #id
    
    def get_tag(self, env): #id
        return "(%s%s%s)" % (self.lhs.get_tag(env), self.operator_id, self.rhs.get_tag(env))
    
    def subst(self, env): #unify
        lhs = self.lhs.subst(env)
        rhs = self.rhs.subst(env)
        if self.operator == '[' and isinstance(lhs, VarTuple) and rhs.is_constant:
            if isinstance(rhs, VarTuple):
                return Interned.of(lhs._id.__getitem__(slice(*rhs.id)))
            return Interned.of(lhs._id.__getitem__(rhs.id))
        if self.operator == '#' and isinstance(rhs, VarTuple):
            return Interned.of(len(rhs))
        if self.operator == '.' and rhs.is_constant:
            return Interned.of(range(rhs.id))
        if lhs.is_constant and rhs.is_constant:
            # calculate expression of constants
            if self.operator == '+':
                return Interned.of(lhs.id + rhs.id)
            elif self.operator == '-':
                return Interned.of(lhs.id - rhs.id)
            elif self.operator == '*':
                return Interned.of(lhs.id * rhs.id)
            elif self.operator == '/':
                return Interned.of(lhs.id / rhs.id)
            elif self.operator == '//':
                return Interned.of(lhs.id // rhs.id)
            elif isinstance(self.operator, type(lambda: None)):
                return Interned.of(self.operator(*(rhs.id)))
            assert False # dead code
        return Operation(lhs, self.operator, rhs)
            
    def shuffle(self, env): #shuffle
        self.lhs.shuffle(env)
        self.rhs.shuffle(env)
    def chase(self, env): #unify
        return Operation(self.lhs.chase(env), self.operator, self.rhs.chase(env))
    
    def unify(self, term, env): #unify
        return None
    def unify_const(self, const, env): #unify
        return None
    def unify_var(self, var, env): #unify
        return None
    def unify_tuple(self, tuple, env): #unify
        return None

    def __str__(self): 
        return "(%s%s%s)" % (str(self.lhs), str(self.operator), str(self.rhs))


class Pred(Interned):
    """ A predicate symbol has a name, an arity, and a database table.  
        It can also have a function used to implement a primitive."""
    registry = weakref.WeakValueDictionary()
    def __new__(cls, pred_name, arity, aggregate=None):
        assert isinstance(pred_name, six.string_types)
        _id = '%s/%i' % (pred_name, arity)
        o = cls.registry.get(_id, Interned.notFound)
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

            o.db = {}
            o.clauses = set([])
            # one index per term. An index is a dictionary of sets
            o.index = [{} for i in range(int(o.arity))]
            o.prim = None
            o.expression = None
            o.aggregate = aggregate
            cls.registry[_id] = o
        return o
    
    def _class(self):
        """determine the python class for a prefixed predicate (and caches it)"""
        # cannot be done at initialization, because the global Class_dict is not filled in yet
        if not hasattr(self, '_cls'): 
            self._cls = Class_dict.get(self.prefix, '') if self.prefix else None
        return self._cls
    
    def reset_clauses(self):
        """ clears the database of clauses for the predicate """
        for clause in list(self.clauses):
            retract(clause)
    
    def __str__(self): 
        return "%s()" % self.name


class Literal(object):
    """ A literal is a predicate and a sequence of terms, 
        the number of which must match the predicate's arity.
    """
    def __init__(self, pred, terms, prearity=None, aggregate=None):
        self.terms = terms
        if isinstance(pred, six.string_types):
            self.pred = Pred(pred, len(terms), aggregate)
            self.pred.prearity = prearity or len(terms)
            if pred[:1] == '~': #pred
                self.pred.base_pred = Pred(pred[1:], len(terms))
        else:
            self.pred = pred
            # TODO assert self.pred.prearity == (prearity or len(terms)), "Error: Incorrect mix of predicates and functions : %s" % str(self)
    
    def _renamed(self, new_name):
        _id = '%s/%i' % (new_name, len(self.terms))
        pred= Pred.registry.get(_id, new_name)
        return Literal(pred, list(self.terms), prearity=self.pred.prearity)
        
    def rebased(self, parent_class): 
        """ returns a literal prefixed by parent class """
        pred = self.pred
        if not parent_class or not pred._class() or parent_class == pred._class(): 
            return self
        return self._renamed(re.sub('^((~)?)'+pred._class().__name__+'.', r'\1'+parent_class.__name__+'.', pred.name))
    
    def equalized(self): 
        """ returns a new literal with '==' instead of comparison """
        return self._renamed(self.pred.name.replace(self.pred.comparison, '=='))
    
    def __str__(self): 
        return "%s(%s)" % (self.pred.name, ','.join([str(term) for term in self.terms])) 


def get_id(literal): #id
    """ The id's encoding ensures that two literals are structurally the
        same if they have the same id. """
    if not hasattr(literal, 'id'): # cached
        literal.id = literal.pred.id + ''.join([term.key for term in literal.terms])
    return literal.id


def get_key(literal): #id
    """ A literal's key is similar to get_id, but only uses the terms up to the prearity. 
        It is used to ensure unicity of results of functions like "pred[k]=v" """
    if not hasattr(literal, 'key'): # cached
        terms = literal.terms
        if len(terms) == literal.pred.prearity:
            literal.key = get_id(literal)
        else:
            literal.key = literal.pred.id + ''.join([terms[i].key for i in range(literal.pred.prearity)])
    return literal.key
    

def get_tag(literal): #id
    """ the tag is used as a key by the subgoal table """
    if not hasattr(literal, 'tag'): # cached
        env = {}
        literal.tag = literal.pred.id + ''.join([term.get_tag(env) for term in literal.terms])
    return literal.tag
     

def subst(literal, env): #unify
    if not env: return literal
    return Literal(literal.pred, [term.subst(env) for term in literal.terms])


def shuffle(literal, env): #shuffle
    for term in literal.terms:
        term.shuffle(env)

def rename(literal): #shuffle
    env={}
    shuffle(literal, env)
    return subst(literal, env)

def unify(literal, other): #unify
    if literal.pred != other.pred: return None
    env = {}
    for term, otherterm in zip(literal.terms, other.terms):
        literal_i = term.chase(env)
        other_i = otherterm.chase(env)
        if literal_i != other_i:
            env = literal_i.unify(other_i, env)
            if env == None: return env
    return env


# These methods are used to handle a set of facts.
def is_member(literal, tbl):
    return tbl.get(get_key(literal))

def adjoin(literal, tbl):
    tbl[get_key(literal)] = literal
    

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


def get_clause_id(clause): #id
    """ The id's encoding ensures that two clauses are structurally equal
        if they have the same id.  A clause's id is used as a key into the
        clause database. """
    if not hasattr(clause, 'id'): # cached
        clause.id = get_key(clause.head) + '<=' + '&'.join([get_key(bodi) for bodi in clause.body])
    return clause.id
    
def subst_in_clause(clause, env, parent_class=None):
    """ apply the env mapping and rebase to parent_class, if any """
    if not env and not parent_class: return clause
    return Clause(subst(clause.head, env).rebased(parent_class),
                       [subst(bodi, env).rebased(parent_class) for bodi in clause.body])
    
def rename_clause(clause):
    """ returns the clause with fresh variables """
    env = {}
    shuffle(clause.head, env)
    for bodi in clause.body:
        shuffle(bodi, env)
    return subst_in_clause(clause, env)

# DATABASE  #####################################################

# The database stores predicates that contain clauses.  Predicates
# not in the database are subject to garbage collection.

db = {}
def insert(pred):
    global db
    db[pred.id] = pred
    return pred

def remove(pred):
    global db
    if pred.id in db : del db[pred.id]
    return pred
    

def assert_(clause):
    """ Add a safe clause to the database """
    pred = clause.head.pred
    if not pred.prim:                   # Ignore assertions for primitives.
        if pred.aggregate and get_clause_id(clause) in pred.db:
            raise util.DatalogError("Error: Duplicate definition of aggregate function.", None, None)
        retract(clause) # to ensure unicity of functions
        pred.db[get_clause_id(clause)] = clause
        if not clause.body: # if it is a fact, update indexes
            for i, term in enumerate(clause.head.terms):
                clauses = pred.index[i].setdefault(term, set([])) # create a set if needed
                clauses.add(clause)
        else:
            pred.clauses.add(clause)
        insert(pred)
    return clause

def retract(clause):
    """ retract a clause from the database"""
    pred = clause.head.pred
    id_ = get_clause_id(clause)
    
    if id_ in pred.db: 
        if not clause.body: # if it is a fact, update indexes
            clause = pred.db[id_] # make sure it is identical to the one in the index
            for i, term in enumerate(clause.head.terms):
                pred.index[i][term].remove(clause)
                # TODO del pred.index[i][term] if the set is empty
        else:
            pred.clauses.remove(pred.db[id_])
        del pred.db[id_]  # remove clause from pred.db
    """ TODO retract last fact removes pred ??  problem with assert function
    if len(pred.db) == 0 and pred.prim == None: # if no definition left
        remove(pred)
    """
    return clause

def relevant_clauses(literal):
    """ returns matching clauses for a literal query, using index """
    result = None
    for i, term in enumerate(literal.terms):
        if term.is_constant:
            facts = literal.pred.index[i].get(term) or set({})
            result = facts if result == None else result.intersection(facts)
    if result == None: # no constants found
        return list(literal.pred.db.values())
    else:
        #result= [ literal.pred.db[id_] for id_ in result ] + [ literal.pred.db[id_] for id_ in literal.pred.clauses]
        return list(result) + list(literal.pred.clauses)
    
"""
The remaining functions in this file implement the tabled logic
programming algorithm described in "Efficient Top-Down Computation of
Queries under the Well-Founded Semantics", Chen, W., Swift, T., and
Warren, D. S., J. Logic Prog. Vol. 24, No. 3, pp. 161-199.  Another
important reference is "Tabled Evaluation with Delaying for General
Logic Programs", Chen, W., and Warren, D. S., J. ACM, Vol. 43, No. 1,
Jan. 1996, pp. 20-74.

It should be noted that a simplified version of the algorithm of 
"Efficient top-down computation" is implemented : negations are not 
supported. 
"""

# The subgoal table is a map from the tag of a subgoal's
# literal to a subgoal.

subgoals = {}

def find(literal):
    tag = get_tag(literal)
    return subgoals.get(tag)

def merge(subgoal):
    global subgoals
    subgoals[get_tag(subgoal.literal)] = subgoal


class Subgoal(object):
    """
    A subgoal has a literal, a set of facts, and an array of waiters.
    A waiter is a pair containing a subgoal and a clause.
    """
    def __init__(self, literal):
        self.literal = literal
        self.facts = {}
        self.waiters = []
    

def resolve(clause, literal):
    """
    Resolve the selected literal of a clause with a literal.  The
    selected literal is the first literal in body of a rule.  If the
    two literals unify, a new clause is generated that has a body with
    one less literal.
    """
    env = unify(clause.body[0], rename(literal))
    return Clause(subst(clause.head, env), [subst(bodi, env) for bodi in clause.body[1:] ])
 
################# Task management ###############################

# A stack of thunks is used to avoid the stack overflow problem
# by delaying the evaluation of some functions

Fast = None
tasks = None
Stack = []       

# Schedule a task for later invocation

class Thunk(object):
    def __init__(self, thunk):
        self.thunk = thunk
    def do(self):
        self.thunk()
        
def schedule(task):
    global tasks
    if Fast: return task.do()
    return tasks.append(task)

def complete(thunk, post_thunk):
    """makes sure that thunk() is completed before calling post_thunk and resuming processing of other thunks"""
    global Fast, subgoals, tasks, Stack
    Stack.append((subgoals, tasks)) # save the environment to the stack. Invoke will eventually do the Stack.pop().
    subgoals, tasks = {}, deque()
    schedule(Thunk(thunk))
    if Fast: 
        Stack.pop() # to recover memory space
        return post_thunk() # don't bother with thunks if Fast
    # prepend post_thunk at one level lower in the Stack, 
    # so that it is run immediately by invoke() after the search() thunk is complete
    if Logging: logging.debug('push')
    Stack[-1][1].appendleft(Thunk(post_thunk)) 
    

def invoke(thunk):
    """ Invoke the tasks. Each task may append new tasks on the schedule."""
    global tasks, subgoals
    if Fast: return thunk()
    tasks = deque([Thunk(thunk),])
    while tasks or Stack:
        while tasks:
            tasks.popleft().do() # get the thunk and execute it
        if Stack: 
            subgoals, tasks = Stack.pop()
            if Logging: logging.debug('pop')
    tasks = None

# dedicated objects give better performance than thunks 
class Search(object):
    def __init__(self, subgoal):
        self.subgoal = subgoal
    def do(self):
        search(self.subgoal)

class Add_clause(object):
    def __init__(self, subgoal, clause):
        self.subgoal = subgoal
        self.clause = clause
    def do(self):
        add_clause(self.subgoal, self.clause)
        
################## add derived facts and use rules ##############

def fact(subgoal, literal):
    """ 
    Store a derived fact, and inform all waiters of the fact too. 
    SLG_ANSWER in the reference article
    """
    if isinstance(literal, Literal) and not all(t.is_constant for t in literal.terms):
        literal = True
    if literal is True:
        if Logging: logging.info("New fact : %s is True" % str(subgoal.literal))
        subgoal.facts = True
        for waiter in reversed(subgoal.waiters):
            resolvent = Clause(waiter.clause.head, [bodi for bodi in waiter.clause.body[1:] ])
            schedule(Add_clause(waiter.subgoal, resolvent))
    elif subgoal.facts is not True and not is_member(literal, subgoal.facts):
        if Logging: logging.info("New fact : %s" % str(literal))
        adjoin(literal, subgoal.facts)
        for waiter in reversed(subgoal.waiters):
            resolvent = resolve(waiter.clause, literal)
            if resolvent != None:
                schedule(Add_clause(waiter.subgoal, resolvent))


class Waiter(object):
    def __init__(self, subgoal, clause):
        self.subgoal = subgoal
        self.clause = clause
        
def rule(subgoal, clause, selected):
    """ 
    Use a newly derived rule. 
    SLG_POSITIVE in the reference article
    """
    if len(subgoal.facts)==1 \
    and all(subgoal.literal.terms[i].is_constant 
            for i in range(subgoal.literal.pred.prearity)):
        return # no need to keep looking if THE answer is found already
    sg = find(selected)
    if sg != None:
        sg.waiters.append(Waiter(subgoal, clause))
        todo = []
        for fact in list(sg.facts.values()):
            resolvent = resolve(clause, fact)
            if resolvent != None: 
                todo.append(resolvent)
        for t in todo:
            schedule(Add_clause(subgoal, t))
    else:
        sg = Subgoal(selected)
        sg.waiters.append(Waiter(subgoal, clause))
        merge(sg)
        return schedule(Search(sg))
    
def add_clause(subgoal, clause):
    """ SLG_NEWCLAUSE in the reference article """
    if not clause.body:
        return fact(subgoal, clause.head)
    else:
        return rule(subgoal, clause, clause.body[0])
    
def fact_candidate(subgoal, class0, result):
    """ add result as a candidate fact of class0 for subgoal"""
    if result is True:
        return fact(subgoal, True)
    result = [Interned.of(r) for r in result]
    if class0 and result[0].id and not isinstance(result[0].id, class0): 
        return
    result = Literal(subgoal.literal.pred.name, result)
    env = unify(subgoal.literal, result)
    if env != None:
        fact(subgoal, result)

###############     SEARCH     ##################################

def _(self):
    " iterator of the parent classes that have pyDatalog capabilities"
    if self._class():
        for cls in self._cls.__mro__:
            if cls.__name__ in Class_dict and cls.__name__ not in ('Mixin', 'object'):
                yield cls
    else:
        yield None
Pred.parent_classes = _

def search(subgoal):
    """ 
    Search for derivations of the literal associated with this subgoal 
    aka SLG_SUBGOAL in the reference article
    """
    literal0 = subgoal.literal
    class0 = literal0.pred._class()
    terms = literal0.terms
    
    if class0 and terms[0].is_constant and terms[0].id is None: return
    if hasattr(literal0.pred, 'base_pred'): # this is a negated literal
        if Logging: logging.debug("pyDatalog will search negation of %s" % literal0)
        base_literal = Literal(literal0.pred.base_pred, terms)
        """ the rest of the processing essentially performs the following, 
        but in its own environment, and with precautions to avoid stack overflow :
            result = ask(base_literal)
            if result is None or 0 == len(result.answers):
                return fact(subgoal, literal)
        """
        def _search(base_literal, subgoal, literal): # first-level thunk for ask(base_literal)
            
            #TODO check that literal is not one of the subgoals already in the stack, to prevent infinite loop
            # example : p(X) <= ~q(X); q(X) <= ~ p(X); creates an infinite loop
            
            base_subgoal = Subgoal(base_literal)

            complete(lambda base_subgoal=base_subgoal: merge(base_subgoal) or search(base_subgoal),
                     lambda base_subgoal=base_subgoal, subgoal=subgoal, literal=literal:
                        fact(subgoal, True) if not base_subgoal.facts else None)
                
        schedule(Thunk(lambda base_literal=base_literal, subgoal=subgoal, literal=literal0: 
                       _search(base_literal, subgoal, literal)))
        return

    if class0 and terms[0].is_constant and not isinstance(terms[0].id, class0):
        raise util.DatalogError("TypeError: First argument of %s must be a %s, not a %s " 
                    % (str(literal0), class0.__name__, type(terms[0].id).__name__), None, None)
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
            method_name = '_pyD_%s%i'% (literal.pred.suffix, int(literal.pred.arity)) # TODO special method for comparison
            if literal.pred.suffix and method_name in _class.__dict__:
                if Logging : logging.debug("pyDatalog uses class resolvers for %s" % literal)
                for result in getattr(_class, method_name)(*terms):
                    fact_candidate(subgoal, class0, result)
                return        
            try: # call class._pyD_query
                resolver = _class._pyD_query
                if not _class.has_SQLAlchemy : gc.collect() # to make sure pyDatalog.metaMixin.__refs__[cls] is correct
                for result in resolver(literal.pred.name, terms):
                    fact_candidate(subgoal, class0, result)
                if Logging : logging.debug("pyDatalog has used _pyD_query resolvers for %s" % literal)
                return
            except:
                pass
        if literal.pred.prim: # X==Y, X < Y+Z
            if Logging : logging.debug("pyDatalog uses comparison primitive for %s" % literal)
            return literal.pred.prim(literal, subgoal)
        elif literal.pred.aggregate:
            if Logging : logging.debug("pyDatalog uses aggregate primitive for %s" % literal)
            aggregate = literal.pred.aggregate
            base_terms = list(terms)
            del base_terms[-1]
            base_terms.extend([ Var('V%i' % i) for i in range(aggregate.arity)])
            base_literal = Literal(literal.pred.name + '!', base_terms) #pred
    
            #TODO thunking to avoid possible stack overflow
            global Fast, subgoals, tasks, Stack
            Stack.append((subgoals, tasks)) # save the environment to the stack. Invoke will eventually do the Stack.pop() when tasks is empty
            subgoals, tasks = {}, deque()
            #result = ask(base_literal)
            base_subgoal = Subgoal(base_literal)
            merge(base_subgoal)
            Fast = True # TODO why is it needed ??  Side effects !
            search(base_subgoal)
            Fast = False # TODO
            result = [ tuple(l.terms) for l in list(base_subgoal.facts.values())]
            
            if result:
                aggregate.sort_result(result)
                for k, v in groupby(result, aggregate.key):
                    aggregate.reset()
                    for r in v:
                        if aggregate.add(r):
                            break
                    k = aggregate.fact(k)
                    fact_candidate(subgoal, class0, k)
            return
        elif literal.pred.id in db: # has a datalog definition, e.g. p(X), p[X]==Y
            for clause in relevant_clauses(literal):
                renamed = rename_clause(clause)
                env = unify(literal, renamed.head)
                if env != None:
                    clause = subst_in_clause(renamed, env, class0)
                    if Logging : logging.debug("pyDatalog will use clause : %s" % clause)
                    schedule(Add_clause(subgoal, clause))
            return
        elif literal.pred.comparison: # p[X]<=Y => consider translating to (p[X]==Y1) & (Y1<Y)
            literal1 = literal.equalized()
            if literal1.pred.db: # equality has a datalog definition
                Y1 = Fresh_var()
                literal1.terms[-1] = Y1
                literal2 = Literal(literal.pred.comparison, (Y1, terms[-1]))
                clause = Clause(literal, (literal1, literal2))
                renamed = rename_clause(clause)
                env = unify(literal, renamed.head)
                if env != None:
                    renamed = subst_in_clause(renamed, env, class0)
                    if Logging : logging.debug("pyDatalog will use clause for comparison: %s" % renamed)
                    schedule(Add_clause(subgoal, renamed))
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
    raise AttributeError("Predicate without definition (or error in resolver): %s" % literal.pred.id)
            
# Sets up and calls the subgoal search procedure, and then extracts
# the answers into an easily used table.  The table has the name of
# the predicate, the predicate's arity, and an array of constant
# terms for each answer.  If there are no answers, nil is returned.

def _(literal, fast):
    global Fast, subgoals, tasks, Stack
    Fast = fast
    
    subgoals = {}
    subgoal = Subgoal(literal)
    merge(subgoal)
    invoke(lambda subgoal=subgoal: search(subgoal))
    subgoals = None
    if subgoal.facts is True:
        return True
    return [ tuple([term.id for term in literal.terms]) for literal in list(subgoal.facts.values())]    
Literal.ask = _

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

# Other parts of the Datalog system depend on the equality primitive,
# so carefully consider any modifications to it.

def equals_primitive(literal, subgoal):
    x = literal.terms[0]
    y = literal.terms[1]
    env = x.unify(y, {})# Both terms must unify,
    if env != None:                     # and at least one of them
        x = x.subst(env)          # must be a constant.
        y = y.subst(env)
    #unbound: can't raise error if both are still unbound, because split(a,b,c) would fail (see test.py)
    return x.equals_primitive(y, subgoal)

# Does a literal unify with an fact known to contain only constant
# terms?

def match(literal, fact):
    env = {}
    for term, factterm in zip(literal.terms, fact.terms):
        if term != factterm:
            env = term.match(factterm, env)
            if env == None: return env
    return env
Const.match = lambda self, const, env : None
def _(self, const, env):
    if self not in env:
        env[self] = const
        return env
    elif env[self] == const: # dead code ?
        return env
    else: # dead code ?
        return None
Var.match = _
Fresh_var.match = _

# Add a primitives that is defined by an iterator.  When given a
# literal, the iterator generates a sequences of answers.  Each
# answer is an array.  Each element in the array is either a number
# or a string.  The length of the array is equal to the arity of the
# predicate.

def add_iter_prim_to_predicate(pred, iter): # separate function to allow re-use
    def prim(literal, subgoal, pred=pred, iter=iter): # TODO consider merging with fact_candidate
        for terms in iter(literal):
            if terms is True:
                fact(subgoal, True)
            elif len(terms) == len(literal.terms):
                new = Literal(pred, [Interned.of(term) for term in terms])
                if match(literal, new) != None:
                    fact(subgoal, new)
    pred.prim = prim
    
def compare_primitive(literal):
    x = literal.terms[0]
    y = literal.terms[1]
    if not x.is_constant:
        if literal.pred.name == '_pyD_in':
            if not y.is_const:
                raise util.DatalogError("Error: right hand side must be bound: %s" % literal, None, None)
            for v in y.id:
                yield [v, y]
        else:
            raise util.DatalogError("Error: left hand side of comparison must be bound: %s" 
                                    % literal.pred.id, None, None)
    elif y.is_constant:
        if compare(x.id, literal.pred.name, y.id):
            yield True
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
    global db
    db = {}
    Pred.registry = weakref.WeakValueDictionary()

    insert(Pred("==", 2)).prim = equals_primitive
    add_iter_prim_to_predicate(insert(Pred("<" , 2)), compare_primitive)
    add_iter_prim_to_predicate(insert(Pred("<=", 2)), compare_primitive)
    add_iter_prim_to_predicate(insert(Pred("!=", 2)), compare_primitive)
    add_iter_prim_to_predicate(insert(Pred(">=", 2)), compare_primitive)
    add_iter_prim_to_predicate(insert(Pred(">" , 2)), compare_primitive)
    add_iter_prim_to_predicate(insert(Pred("_pyD_in", 2)), compare_primitive)
    add_iter_prim_to_predicate(insert(Pred("_pyD_not_in", 2)), compare_primitive)

clear()