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
* support for lambda.

The original code of the lua engine is available in luaEngine.py.

Some differences between python and lua:
* lua indices start at 1, not 0
* any variable is true in lua if it is not nil or false.
* lua tables contain both a list and a dictionary --> need to change them to objects
* lua variables are global by default, python ones are local by default
* variable bindings in a closure.  See http://tiny.cc/7837cw, http://tiny.cc/rq47cw
"""
import copy
from itertools import groupby
import re
import six
import weakref

pyDatalog = None #circ: later set by pyDatalog to avoid circular import

Trace = False # True --> show new facts when they are established
Debug = False # for deeper traces
Auto_print = False # True => automatically prints the result of a query

Python_resolvers = {} # dictionary  of python functions that can resolve a predicate

# DATA TYPES

# Internalize objects based on an identifier.

# To make comparisons between items of the same type efficient, each
# item is internalized so there is at most one of them associated
# with an identifier.  An identifier is always a string.

# For example, after internalization, there is one constant for each
# string used to name a constant.


class Interned(object):
    # beware, they should be one registry per class --> copy the following line in the child class !!
    # registry = weakref.WeakValueDictionary() 
    notFound = object()
    def __eq__(self, other):
        return self is other
    def __hash__(self): return id(self)
    def __ne__(self, other):
        return not self is other

# A term is either a variable or a constant.

# Variables as simple objects.

def add_size(s):
    return "%i:%s" % (len(s), s)

class Var(Interned):
    registry = weakref.WeakValueDictionary()
    def __new__(cls,  _id):
        o = cls.registry.get(_id, Interned.notFound)
        if o is Interned.notFound:
            o = object.__new__(cls) # o is the ref that keeps it alive
            o.id = _id
            o.key = add_size('v' + _id)
            cls.registry[_id] = o
        return o
    def is_const(self):
        return False
    def __str__(self): return self.id 

class Fresh_var(object): # no interning needed
    fresh_var_state = 0  
    def __init__(self):
        self.id = str(Fresh_var.fresh_var_state)
        self.key = add_size('v' + self.id)
        Fresh_var.fresh_var_state += 1 # ensures freshness
    def is_const(self):
        return False
    def __str__(self): return "variable_%s" % self.id 

# Constants as simple objects.

class Const(Interned):
    registry = weakref.WeakValueDictionary()
    def __new__(cls,  _id):
        o = cls.registry.get(_id, Interned.notFound)
        if o is Interned.notFound: 
            o = object.__new__(cls) # o is the ref that keeps it alive
            o.id = _id
            o.key = add_size( 'c' + str(o.id) if isinstance(o.id, (six.string_types, int)) 
                    else 'o' + str(id(o)) )
            cls.registry[_id] = o
        return o
    def is_const(self):
        return True
    def __str__(self): return "'%s'" % self.id 

# Predicate symbols

# A predicate symbol has a name, an arity, and a database table.  It
# can also have a function used to implement a primitive.

class Pred(Interned):
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
        for clause in list(self.clauses):
            retract(clause)
    
    def __str__(self): return "%s()" % self.name

# Literals

# A literal is a predicate and a sequence of terms, the number of
# which must match the predicate's arity.

class Literal(object):
    def __init__(self, pred, terms, prearity=None, aggregate=None):
        if isinstance(pred, six.string_types):
            self.pred = Pred(pred, len(terms), aggregate)
            if pred[:1] == '~':
                self.pred.base_pred = Pred(pred[1:], len(terms))
        else:
            self.pred = pred
            assert self.pred.prearity == prearity or len(terms), "Error: Incorrect mix of predicates and functions : %s" & str(self)
        self.terms = terms
        self.pred.prearity = prearity or len(terms)
    
    def _renamed(self, new_name):
        _id = '%s/%i' % (new_name, len(self.terms))
        pred= Pred.registry.get(_id, new_name)
        new = Literal(pred, list(self.terms), prearity=self.pred.prearity)
        return new
        
    def rebased(self, parent_class): # returns a literal prefixed by parent class
        pred = self.pred
        if not parent_class or not pred._class() or parent_class == pred._class(): 
            return self
        return self._renamed(re.sub('^((~)?)'+pred._class().__name__+'.', r'\1'+parent_class.__name__+'.', pred.name))
    
    def equalized(self): # returns a new literal with '==' instead of comparison
        return self._renamed(self.pred.name.replace(self.pred.comparison, '=='))
    
    def __str__(self): return "%s(%s)" % (self.pred.name, ','.join([str(term) for term in self.terms])) 

# A literal's id is computed on demand, but then cached.  It is used
# by a clause when creating its id.

# The id's encoding ensures that two literals are structurally the
# same if they have the same id.

def get_id(literal):
    if not hasattr(literal, 'id'):
        literal.id = add_size(literal.pred.id) + ''.join([term.key for term in literal.terms])
    return literal.id

# A literal's key is similar to get_id, but only uses the terms up to the prearity. 
# It is used to ensure unicity of results of functions like "pred[k]=v"

def get_key(literal):
    if not hasattr(literal, 'key'):
        terms = literal.terms
        if literal.pred.prearity and len(terms) == literal.pred.prearity:
            literal.key = get_id(literal)
        else:
            literal.key = add_size(literal.pred.id) + ''.join([terms[i].key for i in range(literal.pred.prearity or len(terms))])
    return literal.key
    
# Variant tag

# Two literal's variant tags are the same if there is a one-to-one
# mapping of variables to variables, such that when the mapping is
# applied to one literal, the result is a literal that is the same as
# the other one, when compared using structural equality.  The
# variant tag is used as a key by the subgoal table.

def get_tag(literal):
    if not hasattr(literal, 'tag'):
        literal.tag = add_size(literal.pred.id)
        env = {}
        literal.tag += ''.join([add_size(term.get_tag(i, env)) for i, term in enumerate(literal.terms)])
    return literal.tag
     
Const.get_tag = lambda self, i, env : self.key
Var.get_tag = lambda self, i, env : env[self] if self in env else env.setdefault(self, 'v%i' % i)
Fresh_var.get_tag = lambda self, i, env : env[self] if self in env else env.setdefault(self, 'v%i' % i)

# Substitution

# An environment is a map from variables to terms.

def subst(literal, env):
    if len(env) == 0: return literal
    return Literal(literal.pred, [term.subst(env) for term in literal.terms])

Const.subst = lambda self, env : self
Var.subst = lambda self, env : env.get(self, self)
Fresh_var.subst = lambda self, env : env.get(self, self)

# Shuffle creates an environment in which all variables are mapped to
# freshly generated variables.

def shuffle(literal, env={}):
    map_ = env
    for term in literal.terms:
        term.shuffle(map_)
    return map_
Const.shuffle = lambda self, env : None
Var.shuffle = lambda self, env : env[self] if self in env else env.setdefault(self, Fresh_var())
Fresh_var.shuffle = lambda self, env : env[self] if self in env else env.setdefault(self, Fresh_var())

# Renames a literal using an environment generated by shuffle.

def rename(literal):
    return subst(literal, shuffle(literal))

# Unify two literals.  The result is either an environment or nil.
# Nil is returned when the two literals cannot be unified.  When they
# can, applying the substitutions defined by the environment on both
# literals will create two literals that are structurally equal.

def unify(literal, other):
    if literal.pred != other.pred: return None
    env = {}
    for term, otherterm in zip(literal.terms, other.terms):
        literal_i = term.chase(env)
        other_i = otherterm.chase(env)
        if literal_i != other_i:
            env = literal_i.unify(other_i, env)
            if env == None: return env
    return env

# Chase returns a constant or an unbound variable.

Const.chase =  lambda self, env : self
Var.chase = lambda self, env : env[self].chase(env) if self in env else self
Fresh_var.chase = lambda self, env : env[self].chase(env) if self in env else self

# The case analysis for unifying two terms is handled by method
# dispatch.

Const.unify = lambda self, term, env : term.unify_const(self, env)
Const.unify_const = lambda self, const, env : None
def _(self, const, env):
    env[self] = const
    return env
Var.unify_const = _
Fresh_var.unify_const = _

Var.unify = lambda self, term, env : term.unify_var(self, env)
Fresh_var.unify = lambda self, term, env : term.unify_var(self, env)
Const.unify_var = lambda self, var, env : var.unify_const(self, env)
def _(self, var, env):
    env[var] = self
    return env
Var.unify_var = _
Fresh_var.unify_var = _

# Does a literal have a given term?  Internalizing terms ensures an
# efficient implementation of this operation.

def is_in(term, literal):
    return any([term2==term for term2 in literal.terms])

# These methods are used to handle a set of facts.
def is_member(literal, tbl):
    return tbl.get(get_key(literal))

def adjoin(literal, tbl):
    tbl[get_key(literal)] = literal
    
# Clauses

# A clause has a head literal, and a sequence of literals that form
# its body.  If there are no literals in its body, the clause is
# called a fact.  If there is at least one literal in its body, it is
# called a rule.

# A clause asserts that its head is true if every literal in its body is
# true.

class Clause(object):
    def __init__(self, head, body):
        self.head = head
        self.body = body
    def __str__(self):  
        return "%s <= %s" % (str(self.head), '&'.join([str(literal) for literal in self.body]))
    def __neg__(self):
        """retract clause"""
        retract(self) 

# A clause's id is computed on demand, but then cached.

# The id's encoding ensures that two clauses are structurally equal
# if they have the same id.  A clause's id is used as a key into the
# clause database.

def get_clause_id(clause):
    if not hasattr(clause, 'id'):
        clause.id = add_size(get_key(clause.head)) + ''.join([add_size(get_key(bodi)) for bodi in clause.body])
    return clause.id
    
# Clause substitution in which the substitution is applied to each
# literal that makes up the clause.

def subst_in_clause(clause, env, parent_class=None):
    if len(env) == 0 and not parent_class: return clause
    return Clause(subst(clause.head, env).rebased(parent_class),
                       [subst(bodi, env).rebased(parent_class) for bodi in clause.body])
    
# Renames the variables in a clause.  Every variable in the head is
# in the body, so the head can be ignored while generating an
# environment.

def rename_clause(clause):
    env = {}
    for bodi in clause.body:
        env = shuffle(bodi, env)
    if len(env) == 0: return clause
    return subst_in_clause(clause, env)

# A clause is safe if every variable in its head is in its body.

def is_safe(clause):
    return all([term.is_safe(clause) for term in clause.head.terms])
Const.is_safe = lambda self, clause : True

def _(self, clause):
    return any([is_in(self, bodi) for bodi in clause.body])
Var.is_safe = _
Fresh_var.is_safe = _

# DATABASE

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
    
# Add a safe clause to the database.

def assert_(clause):
    if not is_safe(clause): return None  # An unsafe clause was detected.
    pred = clause.head.pred
    if not pred.prim:                   # Ignore assertions for primitives.
        if pred.aggregate and get_clause_id(clause) in pred.db:
            raise pyDatalog.DatalogError("Error: Duplicate definition of aggregate function.", None, None)
        retract(clause) # to ensure unicity of functions
        pred.db[get_clause_id(clause)] = clause
        if len(clause.body) == 0: # if it is a fact, update indexes
            for i, term in enumerate(clause.head.terms):
                clauses = pred.index[i].setdefault(term, set([])) # create a set if needed
                clauses.add(clause)
        else:
            pred.clauses.add(clause)
        insert(pred)
    return clause

def retract(clause):
    pred = clause.head.pred
    id_ = get_clause_id(clause)
    
    if id_ in pred.db: 
        if len(clause.body) == 0: # if it is a fact, update indexes
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
    #returns matching clauses for a literal query, using index
    result = None
    for i, term in enumerate(literal.terms):
        if term.is_const():
            facts = literal.pred.index[i].get(term) or set({})
            result = facts if result == None else result.intersection(facts)
    if result == None: # no constants found
        return list(literal.pred.db.values())
    else:
        #result= [ literal.pred.db[id_] for id_ in result ] + [ literal.pred.db[id_] for id_ in literal.pred.clauses]
        return list(result) + list(literal.pred.clauses)
    
# PROVER

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

# The subgoal table is a map from the variant tag of a subgoal's
# literal to a subgoal.

subgoals = {}

def find(literal):
    tag = get_tag(literal)
    return subgoals.get(tag)

def merge(subgoal):
    global subgoals
    subgoals[get_tag(subgoal.literal)] = subgoal

# A subgoal is the item that is tabled by this algorithm.

# A subgoal has a literal, a set of facts, and an array of waiters.
# A waiter is a pair containing a subgoal and a clause.

class Subgoal(object):
    def __init__(self, literal):
        self.literal = literal
        self.facts = {}
        self.waiters = []
def make_subgoal(literal):
    return Subgoal(literal)
    
# Resolve the selected literal of a clause with a literal.  The
# selected literal is the first literal in body of a rule.  If the
# two literals unify, a new clause is generated that has a body with
# one less literal.

def resolve(clause, literal):
    env = unify(clause.body[0], rename(literal))
    if env == None: 
        return None # dead code ?
    return Clause(subst(clause.head, env), [subst(bodi, env) for bodi in clause.body[1:] ])
 
# A stack of thunks used to avoid the stack overflow problem
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
    subgoals, tasks = {}, []
    schedule(Thunk(thunk))
    if Fast: 
        #TODO pop to recover memory space
        return post_thunk() # don't bother with thunks if Fast
    # prepend post_thunk at one level lower in the Stack, 
    # so that it is run immediately by invoke() after the search() thunk is complete
    Stack[-1][1].insert(0, Thunk(post_thunk)) 
    
# Invoke the tasks. Each task may append new tasks on the schedule.

def invoke(thunk):
    global tasks, subgoals
    if Fast: return thunk()
    tasks = [Thunk(thunk),]
    while tasks or Stack:
        while tasks:
            tasks.pop(0).do() # get the thunk and execute it
        if Stack: 
            subgoals, tasks = Stack.pop()
            if Debug: print('pop')
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
        
# Store a fact, and inform all waiters of the fact too.

def fact(subgoal, literal):
    if not is_member(literal, subgoal.facts):
        if Trace: print("New fact : %s" % str(literal))
        adjoin(literal, subgoal.facts)
        for waiter in reversed(subgoal.waiters):
            resolvent = resolve(waiter.clause, literal)
            if resolvent != None:
                schedule(Add_clause(waiter.subgoal, resolvent))

# Use a newly derived rule.

class Waiter(object):
    def __init__(self, subgoal, clause):
        self.subgoal = subgoal
        self.clause = clause
        
def rule(subgoal, clause, selected):
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
        sg = make_subgoal(selected)
        sg.waiters.append(Waiter(subgoal, clause))
        merge(sg)
        return schedule(Search(sg))
    
def add_clause(subgoal, clause):
    if len(clause.body) == 0:
        return fact(subgoal, clause.head)
    else:
        return rule(subgoal, clause, clause.body[0])
    
# Search for derivations of the literal associated with this subgoal.

def _(self):
    " iterator of the parent classes that have pyDatalog capabilities"
    if self._class():
        for cls in self._cls.__mro__:
            if cls.__name__ in Class_dict and cls not in (pyDatalog.Mixin, object):
                yield cls
    else:
        yield None
Pred.parent_classes = _

def fact_candidate(subgoal, class0, result):
    result = [Const(r) if not isinstance(r, Const) else r for r in result]
    if class0 and result[0].id and not isinstance(result[0].id, class0): 
        return
    result = Literal(subgoal.literal.pred.name, result)
    env = unify(subgoal.literal, result)
    if env != None:
        fact(subgoal, result)

def search(subgoal):
    literal0 = subgoal.literal
    class0 = literal0.pred._class()
    terms = literal0.terms
    
    if class0 and terms[0].is_const() and terms[0].id is None: return
    if hasattr(literal0.pred, 'base_pred'): # this is a negated literal
        if Debug : print("pyDatalog will search negation of %s" % literal0)
        """ 
        for term in terms:
            if not term.is_const(): # all terms of a negated predicate must be bound
                raise RuntimeError('Terms of a negated literal must be bound : %s' % str(literal0))
        """
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
            
            base_subgoal = make_subgoal(base_literal)

            complete(lambda base_subgoal=base_subgoal: merge(base_subgoal) or search(base_subgoal),
                     lambda base_subgoal=base_subgoal, subgoal=subgoal, literal=literal:
                        # TODO deal with variable terms in result 
                        fact(subgoal, literal) if 0 == len(list(base_subgoal.facts.values())) else None)
                
        schedule(Thunk(lambda base_literal=base_literal, subgoal=subgoal, literal=literal0: 
                       _search(base_literal, subgoal, literal)))
        return

    assert not class0 or not terms[0].is_const() or isinstance(terms[0].id, class0), \
        "TypeError: First argument of %s must be a %s, not a %s " % (str(literal0), class0.__name__, type(terms[0].id).__name__)
    for _class in literal0.pred.parent_classes():
        literal = literal0.rebased(_class)
        method_name = '_pyD_%s%i'% (literal.pred.suffix, int(literal.pred.arity)) # TODO special method for comparison
        
        if literal.pred.id in Python_resolvers:
            if Debug : print("pyDatalog uses python resolvers for %s" % literal)
            for result in Python_resolvers[literal.pred.id](*terms):
                fact_candidate(subgoal, class0, result)
            return
        elif _class and literal.pred.suffix and method_name in _class.__dict__:
            if Debug : print("pyDatalog uses class resolvers for %s" % literal)
            for result in getattr(_class, method_name)(*terms):
                fact_candidate(subgoal, class0, result)
            return        
        try: # call class._pyD_query
            for result in _class._pyD_query(literal.pred.name, terms):
                fact_candidate(subgoal, class0, result)
            if Debug : print("pyDatalog has used _pyD_query resolvers for %s" % literal)
            return
        except:
            pass
        if literal.pred.prim: # X==Y, X < Y+Z
            if Debug : print("pyDatalog uses comparison primitive for %s" % literal)
            return literal.pred.prim(literal, subgoal)
        elif literal.pred.aggregate:
            if Debug : print("pyDatalog uses aggregate primitive for %s" % literal)
            aggregate = literal.pred.aggregate
            base_terms = list(terms)
            del base_terms[-1]
            base_terms.extend([ Var('V%i' % i) for i in range(aggregate.arity)])
            base_literal = Literal(literal.pred.name + '!', base_terms)
    
            #TODO thunking to avoid possible stack overflow
            global Fast, subgoals, tasks, Stack
            Stack.append((subgoals, tasks)) # save the environment to the stack. Invoke will eventually do the Stack.pop() when tasks is empty
            subgoals, tasks = {}, []
            #result = ask(base_literal)
            base_subgoal = make_subgoal(base_literal)
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
                    if Debug : print("pyDatalog will use clause : %s" % clause)
                    schedule(Add_clause(subgoal, clause))
            return
        elif literal.pred.comparison: # p[X]<=Y => consider translating to (p[X]==Y1) & (Y1<Y)
            literal1 = literal.equalized()
            if literal1.pred.db: # equality has a datalog definition
                Y1 = Fresh_var()
                literal1.terms[-1] = Y1
                literal2 = Literal('='+Y1.id, (Y1, terms[-1]))
                literal2.pred.operator = literal.pred.comparison
                add_expr_to_predicate(literal2.pred, literal.pred.comparison, literal0.pred.expression)
                clause = Clause(literal, (literal1, literal2))
                renamed = rename_clause(clause)
                env = unify(literal, renamed.head)
                if env != None:
                    renamed = subst_in_clause(renamed, env, class0)
                    if Debug : print("pyDatalog will use clause for comparison: %s" % renamed)
                    schedule(Add_clause(subgoal, renamed))
                return
            
    if class0: # a.p[X]==Y, a.p[X]<y, to access instance attributes
        try: 
            iterator = class0.pyDatalog_search(literal)
            if Debug : print("pyDatalog uses pyDatalog_search for %s" % literal)
            for result in iterator:
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
    subgoal = make_subgoal(literal)
    merge(subgoal)
    invoke(lambda subgoal=subgoal: search(subgoal))
    subgoals = None
    return [ tuple([term.id for term in literal.terms]) for literal in list(subgoal.facts.values())]    
Literal.ask = _

def toAnswer(literal, answers):
    if answers:
        transposed = list(zip(*(answers))) # transpose result
        result = []
        for i, arg in enumerate(literal.terms):
            if not isinstance(arg, Var) or not arg.id.startswith('_pyD_'):
                result.append(transposed[i])
        answers = list(zip(*result)) if result else answers
    if 0 < len(answers):
        answer = pyDatalog.Answer(literal.pred.name, literal.pred.arity, answers)
    else:
        answer = None
    if Auto_print: 
        print(answers)
    return answer
    
# PRIMITIVES

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

binary_equals_pred = Pred("=", 2)

def equals_primitive(literal, subgoal):
    x = literal.terms[0]
    y = literal.terms[1]
    env = x.unify(y, {})# Both terms must unify,
    if env != None:                     # and at least one of them
        x = x.subst(env)          # must be a constant.
        y = y.subst(env)
    #unbound: can't raise error if both are still unbound, because split(a,b,c) would fail (see test.py)
    return x.equals_primitive(y, subgoal)
binary_equals_pred.prim = equals_primitive

Var.equals_primitive = lambda self, term, subgoal: None
Fresh_var.equals_primitive = lambda self, term, subgoal: None

def _(self, term, subgoal):
    if self == term:          # Both terms are constant and equal.
        literal = Literal(binary_equals_pred, (self, self))
        return fact(subgoal, literal)
Const.equals_primitive = _

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
            if len(terms) == len(literal.terms):
                new = Literal(pred, [Const(term) for term in terms])
                if match(literal, new) != None:
                    fact(subgoal, new)
    pred.prim = prim
    
# Support for expression

# Expressions (such as X = Y-1) are represented by a predicate (P(X,Y)) with :
#    an attached expression Y-1 (stored in pred.expression)
#    an operator (=, <=, >=, !=) (stored in pred.operator)
#    and an iterative function (stored in pred.prim)

# An Operand is either a constant or an index in the list of arguments of a literal
# e.g. in P(X,Y) for X = Y-1, Y has an index of 1

class Operand(object):
    def __init__(self, type, value):
        self.value = value if type == 'constant' else None
        self.index = value if type != 'constant' else None
    def eval(self, environment):
        return environment[self.index-1] if self.index != None else self.value
        
def make_operand(type, value):
    return Operand(type, value)

class Expression(object):
    def __init__(self, operator, operand1, operand2):
        self.operator = operator
        self.operand1 = operand1
        self.operand2 = operand2
    def eval(self, env):
        if self.operator == '+':
            return self.operand1.eval(env) + self.operand2.eval(env)
        elif self.operator == '-':
            return self.operand1.eval(env) - self.operand2.eval(env)
        elif self.operator == '*':
            return self.operand1.eval(env) * self.operand2.eval(env)
        elif self.operator == '/':
            return self.operand1.eval(env) / self.operand2.eval(env)
        elif self.operator == '//':
            return self.operand1.eval(env) // self.operand2.eval(env)
        assert False # dead code
        
def make_expression(operator, operand1, operand2):
    return Expression(operator, operand1, operand2)

"""
lambda
"""
class Lambda(object):
    def __init__(self, lambda_object, operands):
        self.lambda_object = lambda_object
        self.operands = operands
    def eval(self, env):
        operands = [operand.eval(env) for operand in self.operands]
        return self.lambda_object(*operands)
        
def make_lambda(lambda_object, operands):
    return Lambda(lambda_object, operands)

# generic comparison function
def compare(l,op,r):
    return l in r if op=='in' else l not in r if op=='not in' else l==r if op=='==' else l!=r if op=='!=' else l<r if op=='<' \
        else l<=r if op=='<=' else l>=r if op=='>=' else l>r if op=='>' else None
def compare2(l,op,r):
    return l._in(r) if op=='in' else l._not_in(r) if op=='not in' else compare(l,op,r)

# this functions adds an expression to an existing predicate

def add_expr_to_predicate(pred, operator, expression):
    def expression_iter(literal):
        x = literal.terms[0]
        args = []
        assert operator in ('==', 'in') or x.is_const(), "Error: left hand side of comparison must be bound: %s" % literal.pred.id
        for term in literal.terms[1:]:
            if not term.is_const():
                #unbound: can't raise error if right side is unbound, because split(a,b,c) would fail (see test.py)
                assert operator == '==', "Error: right hand side of comparison must be unbound: %s" % literal.pred.id
                return
            args.append(term.id)
            
        Y = literal.pred.expression.eval(args) if literal.pred.expression else args[0]
        if literal.pred.operator == "==" and (not x.is_const() or x.id == Y):
            args.insert(0,Y)
            yield args
        elif x.is_const() and compare(x.id, literal.pred.operator, Y) :
            args.insert(0,x.id)
            yield args
        elif (literal.pred.operator == "in" and not x.is_const()):
            for v in Y:
                values = list(args) # makes a copy
                values.insert(0,v)
                yield values

    add_iter_prim_to_predicate(pred, expression_iter)
    pred.operator = operator
    pred.expression = expression
    insert(pred)

def clear():
    global binary_equals_pred, db
    db = {}
    Pred.registry = weakref.WeakValueDictionary()
    binary_equals_pred = Pred("=", 2)
    binary_equals_pred.prim = equals_primitive

clear()