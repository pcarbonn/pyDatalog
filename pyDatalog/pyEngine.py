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
"""

from collections import deque, OrderedDict
import gc
import logging
import re
import threading
import weakref

from . import util

Logging = False # True --> Logging is activated.  Kept for performance reason

Auto_print = False # True => automatically prints the result of a query
Slow_motion = False # True => detail print of the stack of tasks at each step

Python_resolvers = {} # dictionary  of python functions that can resolve a predicate
Logic = None # place holder for Logic class from Logic module

# Keep a dictionary of classes with datalog capabilities.  
Class_dict = {}

Logger = logging.getLogger(__name__)

#       DATA TYPES          #####################################

def Term_of(atom):
    """ factory function for Term """
    if isinstance(atom, Term):
        return atom
    elif isinstance(atom, (list, tuple, util.xrange)):
        return VarTuple(tuple(map(Term_of, atom)))
    else:
        return Const(atom)


class Term(object):
    # needed for Cython
    pass


class Fresh_var(Term):
    """ a variable created by the search algorithm """
    __slots__ = ['id']
    tl = threading.local()
    def __init__(self):
        Fresh_var.tl.counter = Fresh_var.tl.counter +1
        self.id = ('f', Fresh_var.tl.counter) #id
    
    def is_const(self):
        return False
    def get_tag(self, env): #id
        return env.setdefault(self.id, ('v', len(env)))

    def subst(self, env): #unify
        return env.get(self.id, self)
    def shuffle(self, env): #shuffle
        return env.setdefault(self.id, Fresh_var())
    def chase(self, env): #unify
        # try ... except is not faster
        return env[self.id].chase(env) if self.id in env else self
    
    def match(self, constant, env):
        if self.id not in env:
            env[self.id] = constant
            return env
        elif env[self.id] == constant: # dead code ?
            return env
        else: # dead code ?
            return None
    
    def unify(self, term, env):
        if isinstance(term, Fresh_var):
            env[term.id] = self
            return env
        if isinstance(term, (Const, VarTuple)):
            env[self.id] = term
            return env
        if isinstance(term, Operation):
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

    def __str__(self): 
        return self.id[1]

    def unify(self, term, env):
        return Fresh_var.unify(self, term, env)


class Const(Term):
    """ a constant """
    __slots__ = ['id']

    def __init__(self, _id):
        self.id = _id
    
    def is_const(self): # for backward compatibility with custom resolvers
        return True

    def get_tag(self, env): #id
        return self.id
    
    def subst(self, env): #unify
        return self
    def shuffle(self, env): #shuffle
        return self
    def chase(self, env): #unify
        return self
    
    def match(self, constant, env):
        return None
    
    def unify(self, term, env):
        if isinstance(term, Fresh_var):
            env[term.id] = self
            return env
        return None

    def __str__(self): 
        return "'%s'" % self.id
    def equals_primitive(self, term, subgoal):
        if self.id == term.id:          # Both terms are constant and equal.
            literal = Literal("==", [self, self])
            return subgoal.fact(literal)


class VarTuple(Term):
    """ a tuple / list of variables, constants or tuples """
    __slots__ = ['_id', 'id', 'is_constant']

    def __init__(self, _id): # _id is a list of Term
        self._id = _id
        self.id =  tuple(e.id for e in _id) #id
        self.is_constant = all(element.is_const() for element in _id)

    def is_const(self): # for backward compatibility with custom resolvers
        return self.is_constant

    def __len__(self):
        return len(self._id)
    
    def get_tag(self, env): #id
        if self.is_constant: # can use lazy property only for constants
            return self.id
        #Cython version for : return tuple(t.get_tag(env) for t in self._id) #id
        result = []
        for t in self._id:
            result.append(t.get_tag(env))
        return tuple(result)
    
    def subst(self, env): #unify
        if self.is_constant: # can use lazy property only for constants
            return self
        #Cython version for : return VarTuple(tuple(element.subst(env) for element in self._id))
        result = []
        for t in self._id:
            result.append(t.subst(env))
        return VarTuple(result)

    def shuffle(self, env): #shuffle
        if self.is_constant:
            return self
        else:
            result = []
            for element in self._id:
                result.append(element.shuffle(env))
            return VarTuple(result)
    def chase(self, env): #unify
        if self.is_constant:
            return self
        #Cython version for : return VarTuple(tuple(element.chase(env) for element in self._id))
        result = []
        for t in self._id:
            result.append(t.chase(env))
        return VarTuple(result)
    
    # def match is not needed here

    def unify(self, term, env):
        if isinstance(term, Fresh_var):
            env[term.id] = self
            return env
        if isinstance(term, Const):
            return None
        if isinstance(term, VarTuple):
            if len(self._id) != len(term._id):
                return None
            for e1, e2 in zip(term._id, self._id):
                if e1.id != e2.id:
                    env = e1.unify(e2, env)
                    if env is None: return env
            return env
        if isinstance(term, Operation):
            return None

    def __str__(self): 
        return "'%s'" % str([str(e) for e in self.id])
    def equals_primitive(self, term, subgoal):
        if self.id == term.id:          # Both terms are constant and equal.
            literal = Literal("==", [self, self])
            return subgoal.fact(literal)


class Operation(Term):
    """ an arithmetic operation, a slice or a lambda """
    counter = util.Counter()
    def __init__(self, lhs, operator, rhs):
        self.operator = operator
        self.operator_id = 'l' + str(Operation.counter.next()) if isinstance(self.operator, type(util.LAMBDA)) else str(self.operator)
        self.lhs = lhs
        self.rhs = rhs
        self.is_constant = False
        self.id = (self.lhs.id, self.operator_id, self.rhs.id) #id
    
    def is_const(self): # for backward compatibility with custom resolvers
        return False
    
    def get_tag(self, env): #id
        return (self.lhs.get_tag(env), self.operator_id, self.rhs.get_tag(env))
    
    def subst(self, env): #unify
        lhs = self.lhs.subst(env)
        rhs = self.rhs.subst(env)
        try:
            if self.operator == '[' and isinstance(lhs, (VarTuple, Const)) and rhs.is_const():
                v = lhs._id if isinstance(lhs, VarTuple) else lhs.id
                if isinstance(rhs, VarTuple): # a slice
                    return Term_of(v.__getitem__(slice(*rhs.id)))
                return Term_of(v.__getitem__(rhs.id))
            if self.operator == '#' and isinstance(rhs, VarTuple):
                return Term_of(len(rhs))
            if self.operator == '..' and rhs.is_const():
                return Term_of(range(rhs.id))
            if lhs.is_const() and rhs.is_const():
                # calculate expression of constants
                if self.operator == '+':
                    return Term_of(lhs.id + rhs.id)
                elif self.operator == '-':
                    return Term_of(lhs.id - rhs.id)
                elif self.operator == '*':
                    return Term_of(lhs.id * rhs.id)
                elif self.operator == '/':
                    return Term_of(lhs.id / rhs.id)
                elif self.operator == '//':
                    return Term_of(lhs.id // rhs.id)
                elif self.operator == '**':
                    return Term_of(lhs.id ** rhs.id)
                elif self.operator == '%':
                    return Term_of(lhs.id.format(*(rhs.id)))
                elif isinstance(self.operator, type(util.LAMBDA)):
                    return Term_of(self.operator(*(rhs.id)))
                elif self.operator == '.':
                    v = lhs.id
                    for attribute in rhs.id.split(".") :
                        v = getattr(v, attribute)
                    return Term_of(v)
                elif self.operator == '(':
                    return Term_of(lhs.id.__call__(*(rhs.id)))
                assert False # dead code
            return Operation(lhs, self.operator, rhs)
        except Exception as e:
            return Term_of(e)
            
    def shuffle(self, env): #shuffle
        return Operation(self.lhs.shuffle(env), self.operator, self.rhs.shuffle(env))

    def chase(self, env): #unify
        return Operation(self.lhs.chase(env), self.operator, self.rhs.chase(env))
    
    def unify(self, term, env):
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
        return self is not other


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
                o.recursive = False
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
        self.id, self.tag = None, None
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
        return "%s(%s)" % (self.pred.name, ','.join([str(term).replace("'_pyD_class'", '*') for term in self.terms])) 

    def get_id(self): #id
        """ The id's encoding ensures that two literals are structurally the
            same if they have the same id.  """
        if not self.id: # cached
            #Cython for : self.id = (self.pred.id,) + tuple(term.id for term in self.terms)
            result = [self.pred.id,]
            for term in self.terms:
                result.append(term.id)
            self.id = tuple(result)
        return self.id        

    def get_fact_id(self): #id
        """ The id of a known fact is limited by its prearity
            Prearity is used to ensure unicity of results of functions like pred[2]==1 """
        return self.get_id()[:1+self.pred.prearity]

    def get_tag(self): #id
        """ the tag is used as a key by the subgoal table """
        if not self.tag: # cached
            env = {}
            #Cython equivalent for : self.tag = (self.pred.id,) + tuple(term.get_tag(env) for term in self.terms)
            result = [self.pred.id,]
            for term in self.terms:
                result.append(term.get_tag(env))
            self.tag = tuple(result)
        return self.tag       

    def subst(self, env): #unify
        if not env: return self
        #Cython equivalent for : return Literal(self.pred, [term.subst(env) for term in self.terms], aggregate=self.aggregate)
        result = []
        for term in self.terms:
            result.append(term if isinstance(term, Const) else term.subst(env))    
        return Literal(self.pred, result, aggregate=self.aggregate)

    def shuffle(self, env): #shuffle
        result = []
        for term in self.terms:
            result.append(term if isinstance(term, Const) else term.shuffle(env))
        return Literal(self.pred, result, aggregate=self.aggregate)

    def unify(self, other): #unify
        if self.pred != other.pred: return None
        env = {}
        for term, otherterm in zip(self.terms, other.terms):
            literal_i = term if isinstance(term, Const) else term.chase(env)
            other_i = otherterm if isinstance(otherterm, Const) else otherterm.chase(env)
            if literal_i.id != other_i.id:
                env = literal_i.unify(other_i, env)
                if env is None: return env
        todo = True
        while todo:
            todo = False
            for key, value in env.items(): # 2nd pass for issue #14
                if not isinstance(value, Const):
                    new_value = value.chase(env)
                    if env[key].id != new_value.id:
                        env[key]= new_value
                        todo = True
        return env

    def match(self, terms):
        """ Does a fact unify with a fact known to contain only constant terms? """
        env = {}
        for term, factterm in zip(self.terms, terms):
            if term.id != factterm.id:
                env = term.match(factterm, env)
                if env is None: return env
        return env

    def ask(self):
        """ Invoke the tasks. Each task may append new tasks on the schedule."""
        Ts = Logic.tl.logic
        saved_environment = Ts.Tasks, Ts.Recursive_Tasks, Ts.Recursive, Ts.Subgoals, Ts.Goal
        Ts.Tasks, Ts.Recursive_Tasks, Ts.Subgoals, Ts.Goal = list(), deque(), {}, Subgoal(self)
        Ts.gc_uncollected, Ts.Recursive = True, False
        todo, arg = (SEARCH, (Ts.Goal, ))
        while todo:
            todo, arg = todo(*arg)
    
        if Ts.Goal.facts is True:
            return True
        result = [ tuple(term.id for term in literal.terms) for literal in list(Ts.Goal.facts.values())]
        Ts.Tasks, Ts.Recursive_Tasks, Ts.Recursive, Ts.Subgoals, Ts.Goal = saved_environment
        return result    


class Clause(object):
    """ A clause asserts that its head is true if every literal in its body is
        true.  If there are no literals in the body, the clause is a fact
    """
    __slots__ = ['head', 'body', 'id']

    def __init__(self, head, body):
        self.head = head
        self.body = body
        self.id = None
    def __str__(self):  
        return "%s <= %s" % (str(self.head), '&'.join(str(literal) for literal in self.body))
    def __repr__(self):  
        return ("%s <= %s" % (str(self.head), '&'.join(str(literal) for literal in self.body)))[:50]
    def __neg__(self):
        """retract clause"""
        retract(self) 

    def get_id(self): #id
        """ The id's encoding ensures that two clauses are structurally equal
            if they have the same id.  A clause's id is used as a key into the
            clause database. """
        if self.id is None: # cached
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
        return Clause(self.head.shuffle(env),
                           [bodi.shuffle(env) for bodi in self.body])


def add_class(cls, name):
    """ Update the list of pyDatalog-capable classes, and update clauses accordingly"""
    # this is needed because class definition could occur after clause definition
    Class_dict[name] = cls
    #prefixed replace the first term of each functional comparison literal for that class..
    env = {Var(name).id: Const('_pyD_class')}
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
    if pred.id in Logic.tl.logic.Pred_registry:
        del Logic.tl.logic.Pred_registry[pred.id]
    if pred.id in Logic.tl.logic.Db: 
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
            if any(literal.pred.id == pred.id for literal in clause.body):
                pred.recursive = True
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
        if term.is_const():
            facts = literal.pred.index[i].get(term.id, set()) # default : a set
            result = facts if result is None else result.intersection(facts)
    if result is None: # no constants found in literal, thus could not filter literal.pred.deb
        for v in literal.pred.db.values(): 
            yield v
    else:
        for v in literal.pred.clauses.values(): 
            yield v
        for v in result: 
            yield v
    
################# Subgoals ###############################

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

class Subgoal(object):
    """
    A subgoal has a literal, a set of facts, and an array of waiters.
    A waiter is a pair containing a subgoal and a clause.
    """
    __slots__ = ['literal', 'facts', 'waiters', 'tasks', 'clauses', 'recursive', 'is_done', 'on_completion_']
    def __init__(self, literal):
        self.literal = literal
        self.facts = {}
        self.waiters = []
        self.recursive = literal.pred.recursive
        self.tasks = deque() if self.recursive else list()
        self.clauses = []
        # subgoal is done when a partial literal is true 
        # or when one fact is found for a function of constants
        self.is_done = False
        self.on_completion_ = []
    
    def __repr__(self):
        return str(self.literal)
    
    def search(self):
        """ 
        Search for derivations of the literal associated with this subgoal 
        aka SLG_SUBGOAL in the reference article
        """
        literal0 = self.literal
        class0 = literal0.pred._class()
        terms = literal0.terms
        
        if class0 and terms[1].is_const() and terms[1].id is None: return self.next_step()
        if hasattr(literal0.pred, 'base_pred'): # this is a negated literal
            if Logging and Logger.isEnabledFor(logging.DEBUG):
                Logger.debug("pyDatalog will search negation of %s" % literal0)
            base_literal = Literal(literal0.pred.base_pred, terms)
            self.complete(base_literal)
            return self.next_step()
        
        for _class in literal0.pred.parent_classes():
            literal = literal0.rebased(_class)
            
            if Python_resolvers:
                resolver = literal.pred.id if literal.pred.id in Python_resolvers \
                        else literal.pred.id.replace(r'/', str(literal.pred.arity)+r"/")
                if resolver in Python_resolvers:
                    if Logging and Logger.isEnabledFor(logging.DEBUG):
                        Logger.debug("pyDatalog uses python resolvers for %s" % literal)
                    for result in Python_resolvers[resolver](*terms):
                        self.fact_candidate(class0, result)
                    return self.next_step()
            if _class: 
                # TODO add special method for custom comparison
                method_name = '_pyD_%s%i' % (literal.pred.suffix, int(literal.pred.arity - 1)) #prefixed
                if literal.pred.suffix and method_name in _class.__dict__:
                    if Logging and Logger.isEnabledFor(logging.DEBUG):
                        Logger.debug("pyDatalog uses class resolvers for %s" % literal)
                    for result in getattr(_class, method_name)(*(terms[1:])): 
                        self.fact_candidate(class0, (terms[0],) + result)
                    return self.next_step()
                if '_pyD_query' in _class.__dict__:        
                    try: # call class._pyD_query
                        if Logic.tl.logic.gc_uncollected and not _class.has_SQLAlchemy: 
                            gc.collect() # to make sure pyDatalog.metaMixin.__refs__[cls] is correct
                            Logic.tl.logic.gc_uncollected = False
                        results = list(_class._pyD_query(literal.pred.name, terms[1:]))
                    except AttributeError:
                        pass
                    else:
                        if Logging and Logger.isEnabledFor(logging.DEBUG):
                            Logger.debug("pyDatalog uses _pyD_query resolvers for %s" % literal)
                        for result in results:
                            self.fact_candidate(class0, (terms[0],) + result)
                        return self.next_step()
            if literal.pred.prim: # X==Y, X < Y+Z
                if Logging and Logger.isEnabledFor(logging.DEBUG):
                    Logger.debug("pyDatalog uses comparison primitive for %s" % literal)
                literal.pred.prim(literal, self)
                return self.next_step()
            elif literal.aggregate:
                if Logging and Logger.isEnabledFor(logging.DEBUG):
                    Logger.debug("pyDatalog uses aggregate primitive for %s" % literal)
                base_terms = list(terms[:-1])
                for i in literal.aggregate.slice_to_variabilize:
                    base_terms[i] = Fresh_var()
                base_literal = Literal(literal.pred.name, base_terms) # without aggregate to avoid infinite loop
                self.complete(base_literal, literal.aggregate)
                return self.next_step()
            elif literal.pred.id in Logic.tl.logic.Db: # has a datalog definition, e.g. p(X), p[X]==Y
                assert self.clauses == []
                for clause in relevant_clauses(literal):
                    renamed = clause.rename()
                    env = literal.unify(renamed.head)
                    if env != None:
                        clause = renamed.subst(env, class0)
                        if Logging and Logger.isEnabledFor(logging.DEBUG):
                            Logger.debug("pyDatalog will use clause : %s" % clause)
                        self.clauses.append((ADD_CLAUSE, (self, clause)))
                if self.clauses:
                    self.schedule((NEXT_CLAUSE, (self,)))
                return self.next_step()
            elif literal.pred.comparison: # p[X]<=Y => consider translating to (p[X]==Y1) & (Y1<Y)
                literal1 = literal.equalized()
                if literal1.pred.id in Logic.tl.logic.Db: # equality has a datalog definition
                    Y1 = Fresh_var()
                    literal1.terms[-1] = Y1
                    literal2 = Literal(literal.pred.comparison, [Y1, terms[-1]])
                    clause = Clause(literal, [literal1, literal2])
                    renamed = clause.rename()
                    env = literal.unify(renamed.head)
                    if env != None:
                        renamed = renamed.subst(env, class0)
                        if Logging and Logger.isEnabledFor(logging.DEBUG):
                            Logger.debug("pyDatalog will use clause for comparison: %s" % renamed)
                        self.schedule((ADD_CLAUSE, (self, renamed)))
                    return self.next_step()
                
        if class0: # a.p[X]==Y, a.p[X]<y, to access instance attributes
            try: 
                if Logic.tl.logic.gc_uncollected and not class0.has_SQLAlchemy: 
                    gc.collect() # to make sure pyDatalog.metaMixin.__refs__[cls] is correct
                    Logic.tl.logic.gc_uncollected = False
                results = tuple(class0.pyDatalog_search(literal))
            except AttributeError:
                pass
            else:
                if Logging and Logger.isEnabledFor(logging.DEBUG):
                    Logger.debug("pyDatalog uses pyDatalog_search for %s" % literal)
                for result in results:
                    self.fact_candidate(class0, result)
                return self.next_step()
        elif literal.pred.comparison and len(terms)==3 and terms[0].is_const() \
        and terms[0].id != '_pyD_class' and terms[1].is_const(): # X.a[1]==Y
            # do not use pyDatalog_search as the variable may not be in a pyDatalog class
            v = getattr(terms[0].id, literal.pred.suffix)
            if isinstance(terms[1], VarTuple): # a slice
                v = v.__getitem__(slice(*terms[1].id))
            else:
                v = v.__getitem__(terms[1].id)
            if terms[2].is_const() and compare(v, literal.pred.comparison, terms[2].id):
                self.fact_candidate(class0, (terms[0], terms[1], terms[2]))
            elif literal.pred.comparison == "==" and not terms[2].is_const():
                self.fact_candidate(class0, (terms[0], terms[1], v))
            else:
                raise util.DatalogError("Error: right hand side of comparison must be bound: %s" 
                                    % literal.pred.id, None, None)
            return self.next_step()
    
        raise AttributeError("Predicate without definition (or error in resolver): %s" % literal.pred.id)


    ################## add derived facts and use rules ##############

    def add_clause(self, clause):
        """ SLG_NEWCLAUSE in the reference article """
        if self.is_done: # for speed
            if Slow_motion: print("Already completed !")
            return self.next_step() # no need to keep looking if THE answer is found already
        if not clause.body:
            self.fact(clause.head)
        else:
            self.rule(clause, clause.body[0])
        return self.next_step()
    
    def fact(self, literal):
        """ 
        Store a derived fact, and inform all waiters of the fact too. 
        SLG_ANSWER in the reference article
        """
        all_const = True # Cython equivalent for all(t.is_const() for t in literal.terms)
        if literal is not True:
            for t in literal.terms:
                if not(isinstance(t, Const) or t.is_const()): # use isinstance for speed
                    all_const = False
                    break
        if literal is True or not all_const:
            if self.facts != True: # if already True, do not advise its waiters again
                if Logging and Logger.isEnabledFor(logging.INFO):
                    Logger.info("New fact : %s is True" % str(self.literal))
                self.facts, self.is_done = True, True
                for subgoal, clause in self.waiters:
                    resolvent = Clause(clause.head, clause.body[1:])
                    subgoal.schedule((ADD_CLAUSE, (subgoal, resolvent)))
                self.waiters = []
        elif self.facts is not True:
            fact_id = literal.get_fact_id()
            if not self.facts.get(fact_id):
                self.facts[fact_id] = literal
                if Logging and Logger.isEnabledFor(logging.INFO):
                    Logger.info("New fact : %s" % str(literal))
                for subgoal, clause in self.waiters:
                    # Resolve the selected literal of a clause with a literal.
                    # The selected literal is the first literal in body of a rule.
                    # A new clause is generated that has a body with one less literal.
                    env = clause.body[0].unify(literal)
                    assert env != None
                    resolvent = Clause(clause.head.subst(env), 
                                       [bodi.subst(env) for bodi in clause.body[1:] ])
                    subgoal.schedule((ADD_CLAUSE, (subgoal, resolvent)))
                if len(self.facts)==1:  # stop if one fact for a function of constant
                    all_const = True # Cython equivalent for all(self.literal.terms[i].is_const() for i in range(self.literal.pred.prearity)
                    for i in range(self.literal.pred.prearity):
                        t = self.literal.terms[i]
                        if not(isinstance(t, Const) or t.is_const()): # use isinstance for speed
                            all_const = False
                            break
                    if all_const:
                        if Slow_motion: print("is done !")
                        self.is_done = True
                        self.waiters = []

    def fact_candidate(self, class0, result):
        """ add result as a candidate fact of class0 for subgoal"""
        if result is True:
            self.fact(True)
            return
        if len(result) != len(self.literal.terms):
            return
        result = [Term_of(r) for r in result]
        if class0 and result[1].id and not isinstance(result[1].id, class0): #prefixed
            return
        if self.literal.match(result) != None:
            self.fact(Literal(self.literal.pred.name, result))

    def rule(self, clause, selected):
        """ Use a newly derived rule. SLG_POSITIVE in the reference article """
        sg = Logic.tl.logic.Subgoals.get(selected.get_tag())
        if sg != None: # selected subgoal exists already
            if sg.facts is True:
                resolvent = Clause(clause.head, clause.body[1:])
                self.schedule((ADD_CLAUSE, (self, resolvent)))
            else:
                for fact in sg.facts.values(): # catch-up with facts already established
                    env = clause.body[0].unify(fact)
                    assert env != None
                    resolvent = Clause(clause.head.subst(env), 
                                       [bodi.subst(env) for bodi in clause.body[1:] ])
                    self.schedule((ADD_CLAUSE, (self, resolvent)))
            if sg.tasks and sg != self: # no need to say that I'm searching myself
                self.schedule((SEARCHING, (self, sg )))
            if not sg.is_done:
                sg.waiters.append((self, clause)) # add me to sg's waiters
        else: # new subgoal --> create it and launch it
            sg = Subgoal(selected)
            sg.waiters.append((self, clause))
            self.schedule_search(sg)
            

    # state machine of the engine :
    # A stack of thunks is used to avoid the stack overflow problem
    # by delaying the evaluation of some functions
    
    def schedule(self, task):
        """ Schedule a task for later invocation """
        if Slow_motion: print("  Add : %s" % show(task))
        if task[0] is SEARCH:
            # not done in Subgoal.complete() for speed reason
            if Slow_motion:
                sg = Logic.tl.logic.Subgoals.get(self.literal.get_tag())
                if sg != None: # selected subgoal exists already
                    assert False
                
            Logic.tl.logic.Subgoals[self.literal.get_tag()] = self
        if self.recursive:
            Logic.tl.logic.Recursive_Tasks.appendleft(task)
            self.tasks.appendleft(task)
        else:
            Logic.tl.logic.Tasks.append(task)
            self.tasks.append(task)

    def schedule_search(self, subgoal):
        """ schedule SEARCH before SEARCHING, if possible """
        Logic.tl.logic.Recursive = subgoal.recursive
        if self.recursive:
            if subgoal.recursive:
                subgoal.schedule((SEARCH, (subgoal, ))) # first
                self.schedule((SEARCHING, (self, subgoal ))) # last
            else:
                self.schedule((SEARCHING, (self, subgoal ))) # last
                subgoal.schedule((SEARCH, (subgoal, ))) # first
        else:
            if subgoal.recursive:
                self.schedule((SEARCHING, (self, subgoal ))) # first !
                subgoal.schedule((SEARCH, (subgoal, ))) # last !
            else:
                self.schedule((SEARCHING, (self, subgoal ))) # last
                subgoal.schedule((SEARCH, (subgoal, ))) # first
            
    def next_step(self):
        """ returns the next step in the resolution """
        Ts = Logic.tl.logic
        # self.print_()
        if Slow_motion:
            if self.facts is not True:
                print("Facts of:" + str(self))
                for fact in self.facts:
                    print("  " + str(fact))
            print("Clauses of:" + str(self))
            for task in self.clauses:
                print("  " + show(task))
            if self.on_completion_:
                print("On completion : ")
                for task in self.on_completion_:
                    print("  " + show(task))
            print("STACK :" + ("<---" if not Ts.Recursive else ""))
            for task in reversed(Ts.Tasks):
                print("  " + show(task))
            print("RECURSIVE STACK :" + ("<---" if Ts.Recursive else ""))
            for task in reversed(Ts.Recursive_Tasks):
                print("  " + show(task))
            print(" ")
            
        task = (None, None); tasks = None
        if not Ts.Goal.is_done:
            if Ts.Recursive:
                if Ts.Recursive_Tasks:
                    task = Ts.Recursive_Tasks.pop()
                    tasks = task[1][0].tasks
                elif Ts.Tasks:
                    Ts.Recursive = False
                    task = Ts.Tasks.pop()
                    tasks = task[1][0].tasks
            else:
                if Ts.Tasks:
                    task = Ts.Tasks.pop()
                    tasks = task[1][0].tasks
                elif Ts.Recursive_Tasks:
                    Ts.Recursive = True
                    task = Ts.Recursive_Tasks.pop()
                    tasks = task[1][0].tasks
            task2 = tasks.pop() if tasks else (None, None)
            assert task == task2 # make sure we are in sync, and not lose tasks
        if Slow_motion:
            print("Processing : %s" % show(task))
        return task

    def next_clause(self):
        task = None
        if self.tasks: # still some tasks to do --> postpone next_clause
            task = self.next_step()
        elif self.clauses:
            task = self.clauses.pop()
        elif self.on_completion_:
            task = self.on_completion_.pop()

        if task:
            self.schedule((NEXT_CLAUSE, (self,)))
            return task
        return self.next_step()
                
    def searching(self, subgoal):
        """ task used to relaunch searching of subgoal"""
        Ts = Logic.tl.logic
        if not(subgoal.tasks) and not(subgoal.clauses): # subgoal is completed
            if subgoal.on_completion_: # find the completion task for self
                for pos, task in enumerate(subgoal.on_completion_):
                    if task[1][1] == self:
                        subgoal.on_completion_.pop(pos) # don't do it again
                        return task
            return self.next_step()
        # restart searching of subgoal, in correct recursive mode
        # unless the subgoal is a waiter of self (to prevent infinite loop)
        if all(subgoal != waiter[0] for waiter in self.waiters):
            task = (SEARCHING, (self, subgoal))
            if task not in self.tasks:
                self.schedule(task)
            if subgoal.tasks: # place first subgoal task on top of stack, for immediate use
                task = subgoal.tasks[-1]
                if Slow_motion:
                    print("Moving task forward : %s" % show(task))
                if subgoal.recursive: # find the last occurence of task
                    Ts.Recursive_Tasks.reverse()
                    Ts.Recursive_Tasks.remove(task) #TODO this may raise an error ?
                    Ts.Recursive_Tasks.reverse()
                    Ts.Recursive_Tasks.append(task)
                else:
                    Ts.Tasks.remove(task) #TODO this may raise an error ?
                    Ts.Tasks.append(task)
            elif subgoal.clauses:
                assert False #TODO needs a test case
            Ts.Recursive = subgoal.recursive
        return self.next_step()
    
    def complete(self, literal, aggregate=None):
        """makes sure that subgoal is completed before calling post_thunk and resuming processing"""
        #TODO check for infinite loops
        # example : p(X) <= ~q(X); q(X) <= ~ p(X); creates an infinite loop
        subgoal = Logic.tl.logic.Subgoals.get(literal.get_tag())
        if subgoal is None: # selected subgoal does not exist yet
            subgoal = Subgoal(literal)
            self.schedule_search(subgoal)
        else:
            self.schedule((SEARCHING, (self, subgoal)))
        subgoal.on_completion_.append( (ON_COMPLETION, (subgoal, self, aggregate)) )
    
    def on_completion(self, parent, aggregate):
        if Logging and Logger.isEnabledFor(logging.DEBUG):
            Logger.debug('Processing aggregate or negation')

        if aggregate:
            aggregate.complete(self, parent)
        else:
            assert hasattr(parent.literal.pred, 'base_pred') # parent is a negation
            if not self.facts:
                parent.fact(True)
        return self.next_step()
            
# op codes are defined after class Subgoal is defined
SEARCHING = Subgoal.searching
SEARCH = Subgoal.search
ADD_CLAUSE = Subgoal.add_clause
NEXT_CLAUSE = Subgoal.next_clause
ON_COMPLETION = Subgoal.on_completion

def show(task):
    if task == (None, None):
        return
    result = {SEARCHING: "Searching", 
              SEARCH: "Search", 
              ADD_CLAUSE: "Add clause", 
              NEXT_CLAUSE: "Next clause",
              ON_COMPLETION: "On completion"}
    result = "{:40s}.. : ".format(str(task[1][0])[:40]) + result[task[0]] + " " + str(task[1][1:])
    return result if len(result)<110 else result[:110]

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
    if not x.is_const():
        if literal.pred.name == '_pyD_in':
            if not y.is_const:
                raise util.DatalogError("Error: right hand side must be bound: %s" % literal, None, None)
            for v in y.id:
                literal = Literal(literal.pred.name, [Term_of(v), y])
                subgoal.fact(literal)
        else:
            raise util.DatalogError("Error: left hand side of comparison must be bound: %s" 
                                    % literal.pred.id, None, None)
    elif y.is_const():
        if compare(x.id, literal.pred.name, y.id):
            subgoal.fact(True)
    else:
        raise util.DatalogError("Error: right hand side of comparison must be bound: %s" 
                                % literal.pred.id, None, None)

# generic comparison function
def compare(l,op,r):
    return l in r if op=='_pyD_in' else l not in r if op=='_pyD_not_in' else l==r if op=='==' else l!=r if op=='!=' else l<r if op=='<' \
        else l<=r if op=='<=' else l>=r if op=='>=' else l>r if op=='>' else None
def compare2(l,op,r):
    return l._in(r) if op=='_pyD_in' else l._not_in(r) if op=='_pyD_not_in' else compare(l,op,r)

# Initialization   ##################################################

def clear():
    """ clears the logic """
    Logic.tl.logic.Db = {}
    Logic.tl.logic.Pred_registry = weakref.WeakValueDictionary()
    Logic.tl.logic.Subgoals = {}
    Logic.tl.logic.Tasks = None
    Logic.tl.logic.Recursive_Tasks = None
    Logic.tl.logic.Recursive = False
    Logic.tl.logic.Goal = None
    Logic.tl.logic.gc_uncollected = False
    Fresh_var.tl.counter = 0

    insert(Pred("==", 2)).prim = equals_primitive
    insert(Pred("<" , 2)).prim = compare_primitive
    insert(Pred("<=", 2)).prim = compare_primitive
    insert(Pred("!=", 2)).prim = compare_primitive
    insert(Pred(">=", 2)).prim = compare_primitive
    insert(Pred(">" , 2)).prim = compare_primitive
    insert(Pred("_pyD_in", 2)).prim = compare_primitive
    insert(Pred("_pyD_not_in", 2)).prim = compare_primitive
