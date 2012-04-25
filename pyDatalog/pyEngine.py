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
Some differences between python and lua:
* lua indices start at 1, not 0
* env is true in lua if it is not nil or false.
* lua tables contain both a list and a dictionary --> need to change them to objects
* lua variables are global by default, python ones are local by default
* variable bindings in a closure.  See http://tiny.cc/7837cw, http://tiny.cc/rq47cw
"""
import copy
import six
import weakref
from six.moves import queue

Debug = False

# DATA TYPES

# Internalize objects based on an identifier.

# To make comparisons between items of the same type efficient, each
# item is internalized so there is at most one of them associated
# with an identifier.  An identifier is always a string.

# For example, after internalization, there is one constant for each
# string used to name a constant.

"""
local weak_mt = {__mode = "v"}

local function mk_intern(maker)
    local tbl = {}
    setmetatable(tbl, weak_mt)
    local function intern(id)
        local value = tbl[id]
        if not value then
            value = maker(id)
            tbl[id] = value
        end
        return value
    end
    return intern
end
"""

class Interned (object):
    # beware, they should be one registry per class --> copy the following line in the child class !!
    # registry = weakref.WeakValueDictionary() 
    def __new__(cls, *args, **kwargs):
        assert 0 < len(args)
        if not args[0] in cls.registry: # don't use setdefault to avoid creating unnecessary objects
            o = object.__new__(cls, *args, **kwargs) # o is the ref that keeps it alive
            cls.registry[args[0]] = o
        return cls.registry[args[0]]
    def __eq__(self, other):
        return self is other
    def __hash__(self): return id(self)
    def __ne__(self, other):
        return not self is other

# A term is either a variable or a constant.

# Variables as simple objects.

"""
local Var = {}
Var.__index = Var

local function mk_var(id)
    local tbl = {id = id}
    return setmetatable(tbl, Var)
end

local make_var = mk_intern(mk_var)

local fresh_var_state = 0

local function mk_fresh_var()
    local id = tostring(fresh_var_state)
    fresh_var_state = fresh_var_state + 1 # To ensure freshness,
    return mk_var(id);                     # don't intern this variable.
end

function Var:is_const()
    return false
end
"""

class Var(Interned):
    registry = weakref.WeakValueDictionary()
    def __init__(self, _id):
        self.id = _id
    def is_const(self):
        return False
    def __str__(self): return self.id 
def make_var(_id):
    return Var(_id)

class Fresh_var(object): # no interning needed
    fresh_var_state = 0  
    def __init__(self):
        self.id = str(Fresh_var.fresh_var_state)
        Fresh_var.fresh_var_state += 1 # ensures freshness
    def is_const(self):
        return False
    def __str__(self): return self.id 
def mk_fresh_var():
    return Fresh_var()

# Constants as simple objects.

"""
local Const = {}
Const.__index = Const

local function mk_const(id)
    local tbl = {id = id}
    return setmetatable(tbl, Const)
end

local make_const = mk_intern(mk_const)

function Const:is_const()
    return true
end
"""

class Const(Interned):
    registry = weakref.WeakValueDictionary()
    def __init__(self, _id):
        self.id = _id
    def is_const(self):
        return True
    def __str__(self): return "'%s'" % self.id 
def make_const(_id):
    return Const(_id)

# Predicate symbols

# A predicate symbol has a name, an arity, and a database table.  It
# can also have a function used to implement a primitive.

"""
local function mk_pred(id)
    return {id = id, db = {}}
end

local intern_pred = mk_intern(mk_pred)

local function mk_pred_id(pred_name, arity)
    return pred_name .. "/" .. arity
end

local function make_pred(pred_name, arity)
    return intern_pred(mk_pred_id(pred_name, arity))
end
"""

class Pred(Interned):
    registry = weakref.WeakValueDictionary()
    def __new__(cls, *args, **kwargs):
        assert 1 < len(args)
        assert isinstance(args[0], six.string_types)
        _id = '%s/%i' % args[:2]
        if not _id in cls.registry: # don't use setdefault to avoid creating unnecessary objects
            o = object.__new__(cls, *args, **kwargs) # o is the ref that keeps it alive
            o.id = _id
            o.db = {}
            o.prim = None
            o.expression = None
            cls.registry[_id] = o
        return cls.registry[_id]
    def __init__(self, pred_name, arity):
        pass
    def __str__(self): return "%s()" % get_name(self)
def make_pred(pred_name, arity):
    return Pred(pred_name, arity)

"""
local function last_slash(s)          # Returns the location of the last slash
    local i = 0                                # in a string or 0.
    while true do
        local j = string.find(s, "/", i + 1)
        if not j then
            return i
        else
            i = j
        end
    end
end

local function get_name(pred)
    local i = last_slash(pred.id)
    return string.sub(pred.id, 1, i - 1)
end

local function get_arity(pred)
    local i = last_slash(pred.id)
    return tonumber(string.sub(pred.id, i + 1))
end
"""
def get_name(pred):
    return '/'.join(pred.id.split('/')[:-1]) # Who knows, it could contain slashes !?
def get_arity(pred):
    return pred.id.split('/')[-1]

# Duplicates a predicate.  Used to clone databases.
"""
local function dup(pred)
    local db = {}
    for k,v in pairs(pred.db) do
        db[k] = v
    end
    return {id = pred.id, db = db, prim = pred.prim, expression = pred.expression}
end
"""
class Fresh_pred(object):
    def __init__(self, pred):
        self.id = pred.id
        self.db = copy.copy(pred.db)
        self.prim = pred.prim
        self.expression = pred.expression
def dup(pred):
    return Fresh_pred(pred)

# Literals

# A literal is a predicate and a sequence of terms, the number of
# which must match the predicate's arity.

"""
local function make_literal(pred_name, terms)
    local arity = #terms
    local pred = make_pred(pred_name, arity)
    local literal = {pred = pred}
    for i=1,arity do
        literal[i] = terms[i]
    end
    return literal
end
"""
class Literal(object):
    def __init__(self, pred, terms):
        if isinstance(pred, six.string_types):
            self.pred = make_pred(pred, len(terms))
        else:
            self.pred = pred
        self.terms = terms
    def __str__(self): return "%s(%s)" % (get_name(self.pred), ','.join([str(term) for term in self.terms])) 
def make_literal(pred_name, terms):
    return Literal(pred_name, terms)

"""
local function add_size(str)
    return tostring(string.len(str)) .. ":" .. str
end
"""
def add_size(s):
    return "%i:%s" % (len(s), s)

# A literal's id is computed on demand, but then cached.  It is used
# by a clause when creating its id.

# The id's encoding ensures that two literals are structurally the
# same if they have the same id.

"""
local function get_id(literal)
    local id = literal.id
    if not id then
        id = add_size(literal.pred.id)
        for i=1,#literal do
            id = id .. add_size(literal[i]:get_id())
        end
        literal.id = id
    end
    return id
end
"""
def get_id(literal):
    if not hasattr(literal, 'id'):
        literal.id = add_size(literal.pred.id) + ''.join([add_size(term.get_id()) for term in literal.terms])
    return literal.id

"""
function Const:get_id()
    return "c" .. self.id
end

function Var:get_id()
    return "v" .. self.id
end
"""
Const.get_id = lambda self : 'c' + str(self.id)
Var.get_id = lambda self : 'v' + self.id
Fresh_var.get_id = lambda self : 'v' + self.id

# Variant tag

# Two literal's variant tags are the same if there is a one-to-one
# mapping of variables to variables, such that when the mapping is
# applied to one literal, the result is a literal that is the same as
# the other one, when compared using structural equality.  The
# variant tag is used as a key by the subgoal table.

"""
local function get_tag(literal)
    local tag = literal.tag
    if not tag then
        local env = {}
        tag = add_size(literal.pred.id)
        for i=1,#literal do
            tag = tag .. add_size(literal[i]:get_tag(i, env))
        end
        literal.tag = tag
    end
    return tag
end
"""
def get_tag(literal):
    if not hasattr(literal, 'tag'):
        literal.tag = add_size(literal.pred.id)
        env = {}
        temp = []
        literal.tag += ''.join([add_size(term.get_tag(i, env)) for i, term in enumerate(literal.terms)])
    return literal.tag
     
"""
function Const:get_tag(i, env)
    return "c" .. self.id
end

function Var:get_tag(i, env)
    local tag = env[self]
    if not tag then
        tag = "v" .. tostring(i)
        env[self] = tag
    end
    return tag
end
"""
Const.get_tag = lambda self, i, env : 'c' + str(self.id)
Var.get_tag = lambda self, i, env : env[self] if self in env else env.setdefault(self, 'v%i' % i)
Fresh_var.get_tag = lambda self, i, env : env[self] if self in env else env.setdefault(self, 'v%i' % i)

# Substitution

# An environment is a map from variables to terms.

"""
local function subst(literal, env)
    if not next(env) then          # Found an empty map.
        return literal
    end
    local arity = #literal
    local new = {pred = literal.pred}
    for i=1,arity do
        new[i] = literal[i]:subst(env)
    end
    return new
end
"""
def subst(literal, env):
    if len(env) == 0: return literal
    return make_literal(literal.pred, [term.subst(env) for term in literal.terms])

"""
function Const:subst(env)
    return self
end

function Var:subst(env)
    local term = env[self]
    if term then
        return term
    else
        return self
    end
end
"""
Const.subst = lambda self, env : self
Var.subst = lambda self, env : env[self] if self in env else self
Fresh_var.subst = lambda self, env : env[self] if self in env else self

# Shuffle creates an environment in which all variables are mapped to
# freshly generated variables.

"""
local function shuffle(literal, env)
    local map = env or {}
    for i=1,#literal do
        literal[i]:shuffle(map)
    end
    return map
end
"""
def shuffle(literal, env={}):
    map_ = env
    for term in literal.terms:
        term.shuffle(map_)
    return map_
"""
function Const:shuffle(env)
end

function Var:shuffle(env)
    if not env[self] then
        env[self] = mk_fresh_var()
    end
end
"""
Const.shuffle = lambda self, env : None
Var.shuffle = lambda self, env : env[self] if self in env else env.setdefault(self, mk_fresh_var())
Fresh_var.shuffle = lambda self, env : env[self] if self in env else env.setdefault(self, mk_fresh_var())

# Renames a literal using an environment generated by shuffle.

"""
local function rename(literal)
    return subst(literal, shuffle(literal))
end
"""
def rename(literal):
    return subst(literal, shuffle(literal))

# Unify two literals.  The result is either an environment or nil.
# Nil is returned when the two literals cannot be unified.  When they
# can, applying the substitutions defined by the environment on both
# literals will create two literals that are structurally equal.

"""
local function unify(literal, other)
    if literal.pred ~= other.pred then
        return nil
    else
        local env = {}
        for i=1,#literal do
            local literal_i = literal[i]:chase(env)
            local other_i = other[i]:chase(env)
            if literal_i ~= other_i then
                env = literal_i:unify(other_i, env)
                if not env then
                    return env
                end
            end
        end
        return env
    end
end
"""
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

"""
function Const:chase(env)
    return self
end

function Var:chase(env)
    local term = env[self]
    if term then
        return term:chase(env)
    else
        return self
    end
end
"""
Const.chase =  lambda self, env : self
Var.chase = lambda self, env : env[self].chase(env) if self in env else self
Fresh_var.chase = lambda self, env : env[self].chase(env) if self in env else self

# The case analysis for unifying two terms is handled by method
# dispatch.

"""
function Const:unify(term, env)
    return term:unify_const(self, env)
end

function Const:unify_const(const, env)
    return nil
end

function Var:unify_const(const, env)
    env[self] = const
    return env
end
"""
Const.unify = lambda self, term, env : term.unify_const(self, env)
Const.unify_const = lambda self, const, env : None
def _(self, const, env):
    env[self] = const
    return env
Var.unify_const = _
def _(self, const, env):
    env[self] = const
    return env
Fresh_var.unify_const = _

"""
function Var:unify(term, env)
    return term:unify_var(self, env)
end

function Const:unify_var(var, env)
    return var:unify_const(self, env)
end

function Var:unify_var(var, env)
    env[var] = self
    return env
end
"""
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

"""
local function is_in(term, literal)
    for i=1,#literal do
        if literal[i] == term then
            return true
        end
    end
    return false
end
"""
def is_in(term, literal):
    for term2 in literal.terms:
        if term2 == term:
            return True
    return False

# These methods are used to handle a set of facts.
"""
local function is_member(literal, tbl)
    return tbl[get_id(literal)]
end

local function adjoin(literal, tbl)
    tbl[get_id(literal)] = literal
end
"""
def is_member(literal, tbl):
    id_ = get_id(literal)
    return tbl[id_] if id_ in tbl else None

def adjoin(literal, tbl):
    tbl[get_id(literal)] = literal
    
# Clauses

# A clause has a head literal, and a sequence of literals that form
# its body.  If there are no literals in its body, the clause is
# called a fact.  If there is at least one literal in its body, it is
# called a rule.

# A clause asserts that its head is true if every literal in its body is
# true.

"""
local function make_clause(head, body)
    local clause = {head = head}
    for i=1,#body do
        clause[i] = body[i]
    end
    return clause
end
"""
class Clause(object):
    def __init__(self, head, body):
        self.head = head
        self.body = body
    def __str__(self):  return "%s <= %s" % (str(self.head), '&'.join([str(literal) for literal in self.body])) 
def make_clause(head, body):
    return Clause(head, body)

# A clause's id is computed on demand, but then cached.

# The id's encoding ensures that two clauses are structurally equal
# if they have the same id.  A clause's id is used as a key into the
# clause database.

"""
local function get_clause_id(clause)
    local id = clause.id
    if not id then
        id = add_size(get_id(clause.head))
        for i=1,#clause do
            id = id .. add_size(get_id(clause[i]))
        end
        clause.id = id
    end
    return id
end
"""
def get_clause_id(clause):
    if not hasattr(clause, 'id'):
        clause.id = add_size(get_id(clause.head)) + ''.join([add_size(get_id(bodi)) for bodi in clause.body])
    return clause.id
    
# Clause substitution in which the substitution is applied to each
# each literal that makes up the clause.

"""
local function subst_in_clause(clause, env)
    if not next(env) then          # Found an empty map.
        return clause
    end
    local new = {head = subst(clause.head, env)}
    for i=1,#clause do
        new[i] = subst(clause[i], env)
    end
    return new
end
"""
def subst_in_clause(clause, env):
    if len(env) == 0: return clause
    return make_clause(subst(clause.head, env), [subst(bodi, env) for bodi in clause.body])
    
# Renames the variables in a clause.  Every variable in the head is
# in the body, so the head can be ignored while generating an
# environment.

"""
local function rename_clause(clause)
    local env = {}
    for i=1,#clause do
        env = shuffle(clause[i], env)
    end
    if not next(env) then
        return clause
    else
        return subst_in_clause(clause, env)
    end
end
"""
def rename_clause(clause):
    env = {}
    for bodi in clause.body:
        env = shuffle(bodi, env)
    if len(env) == 0: return clause
    return subst_in_clause(clause, env)

# A clause is safe if every variable in its head is in its body.

"""
local function is_safe(clause)
    for i=1,#clause.head do
        if not clause.head[i]:is_safe(clause) then
            return false
        end
    end
    return true
end
"""
def is_safe(clause):
    for term in clause.head.terms:
        if not term.is_safe(clause): return False
    return True
"""

function Const:is_safe(clause)
    return true
end

function Var:is_safe(clause)
    for i=1,#clause do
        if is_in(self, clause[i]) then
            return true
        end
    end
    return false
end
"""
Const.is_safe = lambda self, clause : True
def _(self, clause):
    for bodi in clause.body:
        if is_in(self, bodi): return True
    return False
Var.is_safe = _
Fresh_var.is_safe = _

# DATABASE

# The database stores predicates that contain clauses.  Predicates
# not in the database are subject to garbage collection.

"""
local db = {}

local function insert(pred)
    db[pred.id] = pred
    return pred
end

local function remove(pred)
    db[pred.id] = nil
    return pred
end
"""
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

"""
local function assert(clause)
    if not is_safe(clause) then
        return nil                     # An unsafe clause was detected.
    else
        local pred = clause.head.pred
        if not pred.prim then          # Ignore assertions for primitives.
            pred.db[get_clause_id(clause)] = clause
            insert(pred)
        end
        return clause
    end
end
"""
def assert_(clause):
    if not is_safe(clause): return None  # An unsafe clause was detected.
    pred = clause.head.pred
    if not pred.prim:                   # Ignore assertions for primitives.
        pred.db[get_clause_id(clause)] = clause
        insert(pred)
    return clause

"""
local function retract(clause)
    local pred = clause.head.pred
    pred.db[get_clause_id(clause)] = nil
    if not next(pred.db) and not pred.prim then
        remove(pred)
    end
    return clause
end
"""
def retract(clause):
    pred = clause.head.pred
    id_ = get_clause_id(clause)
    if id_ in pred.db: del pred.db[id_]  # remove clause from pred.db
    if len(pred.db) == 0 and not pred.prim: # if no definition left
        remove(pred)
    return clause

# DATABASE CLONING

# A database can be saved and then later restored.  With copy and
# revert, one can use one copy of a database multiple times to revert
# to a previous database.  These two functions are not exposed in the
# C API.

# Returns a fresh copy of the current database or copies the one
# given as an argument.

""" TODO
local function copy(src)
    local clone = {}
    for k,v in pairs(src or db) do
        clone[k] = dup(v)
    end
    return clone
end

# Reverts datalog to a previously cloned database.  The database is
# copied so that the clone can be used more than once.

local function revert(clone)
    db = copy(clone)
end

# DATABASE STORE

# A database can be saved and then later restored.

local store = {}

local function save()
    table.insert(store, copy())
end

local function restore()
    db = table.remove(store)
    db = db or {}
end
"""
# PROVER

"""
The remaining functions in this file implement the tabled logic
programming algorithm described in "Efficient Top-Down Computation of
Queries under the Well-Founded Semantics", Chen, W., Swift, T., and
Warren, D. S., J. Logic Prog. Vol. 24, No. 3, pp. 161-199.  Another
important reference is "Tabled Evaluation with Delaying for General
Logic Programs", Chen, W., and Warren, D. S., J. ACM, Vol. 43, No. 1,
Jan. 1996, pp. 20-74.
"""

# The subgoal table

# local subgoals
subgoals = {}

# The subgoal table is a map from the variant tag of a subgoal's
# literal to a subgoal.

"""
local function find(literal)
    return subgoals[get_tag(literal)]
end

local function merge(subgoal)
    subgoals[get_tag(subgoal.literal)] = subgoal
end
"""
def find(literal):
    tag = get_tag(literal)
    return subgoals[tag] if tag in subgoals else None

def merge(subgoal):
    global subgoals
    subgoals[get_tag(subgoal.literal)] = subgoal

# A subgoal is the item that is tabled by this algorithm.

# A subgoal has a literal, a set of facts, and an array of waiters.
# A waiter is a pair containing a subgoal and a clause.

"""
local function make_subgoal(literal)
    return {literal = literal, facts = {}, waiters = {}}
end
"""
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

"""
local function resolve(clause, literal)
    local n = #clause
    if n == 0 then
        return nil
    end
    local env = unify(clause[1], rename(literal))
    if not env then
        return nil
    end
    n = n - 1
    local new = {head = subst(clause.head, env)}
    for i=1,n do
        new[i] = subst(clause[i + 1], env)
    end
    return new
end
"""
def resolve(clause, literal):
    if len(clause.body) == 0: return None
    env = unify(clause.body[0], rename(literal))
    if env == None: return None
    return make_clause(subst(clause.head, env), [subst(bodi, env) for bodi in clause.body[1:] ])
 
# A stack of thunks used to delay the evaluation of some expressions

"""
local Fast=nil
local tasks
"""
Fast = None
tasks = None

# Schedule a task for later invocation

"""
local function sched(thunk)
    if Fast then return thunk() end
    return table.insert(tasks, thunk)
end
"""
def sched(thunk):
    global tasks
    if Fast: return thunk()
    return tasks.put(thunk)

# Invoke the scheduled tasks

"""
local function invoke(thunk)
    if Fast then return thunk() end
    tasks = {thunk}
    while true do
        local task = table.remove(tasks)
        if task then
            task()
        else
            break
        end
    end
    tasks = nil
end
"""
def invoke(thunk):
    global tasks
    if Fast: return thunk()
    tasks = queue.Queue()
    tasks.put(thunk)
    while not tasks.empty():
        tasks.get()() # get the thunk and execute it
    tasks = None
    
# Store a fact, and inform all waiters of the fact too.

"""
local fact, rule, add_clause, search

function fact(subgoal, literal)
    if not is_member(literal, subgoal.facts) then
        adjoin(literal, subgoal.facts)
        for i=#subgoal.waiters,1,-1 do
            local waiter = subgoal.waiters[i]
            local resolvent = resolve(waiter.clause, literal)
            if resolvent then
                sched(function () add_clause(waiter.subgoal, resolvent) end)
            end
        end
    end
end
"""
def fact(subgoal, literal):
    if not is_member(literal, subgoal.facts):
        if Debug: print("New fact : %s" % str(literal))
        adjoin(literal, subgoal.facts)
        for waiter in reversed(subgoal.waiters):
            resolvent = resolve(waiter.clause, literal)
            if resolvent != None:
                sched(lambda waiter=waiter, resolvent=resolvent: add_clause(waiter.subgoal, resolvent))

# Use a newly derived rule.

"""
function rule(subgoal, clause, selected)
    local sg = find(selected)
    if sg then
        table.insert(sg.waiters, {subgoal = subgoal, clause = clause})
        local todo = {}
        for id,fact in pairs(sg.facts) do
            local resolvent = resolve(clause, fact)
            if resolvent then
                table.insert(todo, resolvent)
            end
        end
        for i=1,#todo do
            sched(function () add_clause(subgoal, todo[i]) end)
        end
    else
        sg = make_subgoal(selected)
        table.insert(sg.waiters, {subgoal = subgoal, clause = clause})
        merge(sg)
        return sched(function () search(sg) end)
    end
end
"""
class Waiter(object):
    def __init__(self, subgoal, clause):
        self.subgoal = subgoal
        self.clause = clause
        
        
def rule(subgoal, clause, selected):
    sg = find(selected)
    if sg != None:
        if Debug: print("Rule, subgoal found : %s" % str(sg.literal))
        sg.waiters.append(Waiter(subgoal, clause))
        todo = []
        for fact in list(sg.facts.values()):
            resolvent = resolve(clause, fact)
            if resolvent != None: 
                todo.append(resolvent)
        for t in todo:
            sched(lambda subgoal=subgoal, t=t: add_clause(subgoal, t))
    else:
        if Debug: print("Rule, subgoal not found : %s" % str(subgoal.literal))
        sg = make_subgoal(selected)
        sg.waiters.append(Waiter(subgoal, clause))
        merge(sg)
        return sched(lambda sg=sg: search(sg))
    
"""
function add_clause(subgoal, clause)
    if #clause == 0 then
        return fact(subgoal, clause.head)
    else
        return rule(subgoal, clause, clause[1])
    end
end
"""
def add_clause(subgoal, clause):
    if len(clause.body) == 0:
        return fact(subgoal, clause.head)
    else:
        return rule(subgoal, clause, clause.body[0])
    
# Search for derivations of the literal associated with this subgoal.

"""
function search(subgoal)
    local literal = subgoal.literal
    if literal.pred.prim then
        return literal.pred.prim(literal, subgoal)
    else
        for id,clause in pairs(literal.pred.db) do
            local renamed = rename_clause(clause)
            local env = unify(literal, renamed.head)
            if env then
                add_clause(subgoal, subst_in_clause(renamed, env))
            end
        end
    end
end
"""

def search(subgoal):
    if Debug: print("search : %s" % str(subgoal.literal))
    literal = subgoal.literal
    if literal.pred.prim:
        return literal.pred.prim(literal, subgoal)
    else:
        for clause in list(literal.pred.db.values()):
            renamed = rename_clause(clause)
            env = unify(literal, renamed.head)
            if env != None: # lua considers {} as true
                add_clause(subgoal, subst_in_clause(renamed, env))
            
# Sets up and calls the subgoal search procedure, and then extracts
# the answers into an easily used table.  The table has the name of
# the predicate, the predicate's arity, and an array of constant
# terms for each answer.  If there are no answers, nil is returned.

"""
local function ask2(literal, fast)
    Fast = fast
    subgoals = {}
    local subgoal = make_subgoal(literal)
    merge(subgoal)
    invoke(function () search(subgoal) end)
    subgoals = nil
    local answers = {}
    for id,literal in pairs(subgoal.facts) do
        local answer = {}
        for i=1,#literal do # Each term in an answer will be
            table.insert(answer, literal[i].id) # a constant.
        end
        table.insert(answers, answer)
    end
    if #answers > 0 then
        answers.name = get_name(literal.pred)
        answers.arity = get_arity(literal.pred)
        return answers
    else
        return nil
    end
end
"""
class Answer(object):
    def __init__(self, name, arity, answers):
        self.name = name
        self.arity = arity
        self.answers = answers
        
def ask2(literal, fast):
    global Fast, subgoals
    Fast = fast
    subgoals = {}
    subgoal = make_subgoal(literal)
    merge(subgoal)
    invoke(lambda subgoal=subgoal: search(subgoal))
    subgoals = None
    answers = [ tuple([term.id for term in literal.terms]) for literal in list(subgoal.facts.values())]
    if 0 < len(answers):
        answer = Answer(get_name(literal.pred), get_arity(literal.pred), answers)
        return answer
    return None
    
"""
local function ask(query)
    return ask2(query, nil)
end
"""
def ask(query):
    return ask2(query, False)
    
# PRIMITIVES

"""

A primitive predicate, also called a built-in predicate, is
implemented by code.  Assertions about a primitive predicate are
ignored, as the code takes precedence.  Use the make_pred function to
access a primitive by name.

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

"""
do                                          # equals primitive
    local binary_equals_pred = make_pred("=", 2)

    local function equals_primitive(literal, subgoal)
        local x = literal[1]
        local y = literal[2]
        local env = x:unify(y, {})# Both terms must unify,
        if env then                     # and at least one of them
            x = x:subst(env)          # must be a constant.
            y = y:subst(env)
        end
        return x:equals_primitive(y, subgoal)
    end

    function Var:equals_primitive(term, subgoal)
    end

    function Const:equals_primitive(term, subgoal)
        if self == term then          # Both terms are constant and equal.
            local literal = {pred = binary_equals_pred, self, self}
            return fact(subgoal, literal)
        end
    end

    binary_equals_pred.prim = equals_primitive
end
"""
binary_equals_pred = make_pred("=", 2)

def equals_primitive(literal, subgoal):
    x = literal.terms[0]
    y = literal.terms[1]
    env = x.unify(y, {})# Both terms must unify,
    if env != None:                     # and at least one of them
        x = x.subst(env)          # must be a constant.
        y = y.subst(env)
    return x.equals_primitive(y, subgoal)
binary_equals_pred.prim = equals_primitive

Var.equals_primitive = lambda term, subgoal: None
Fresh_var.equals_primitive = lambda term, subgoal: None

def _(self, term, subgoal):
    if self == term:          # Both terms are constant and equal.
        literal = make_literal(binary_equals_pred, (self, self))
        return fact(subgoal, literal)
Const.equals_primitive = _

# Does a literal unify with an fact known to contain only constant
# terms?

"""
local function match(literal, fact)
    local env = {}
    for i=1,#literal do
        if literal[i] ~= fact[i] then
            env = literal[i]:match(fact[i], env)
            if not env then
                return env
            end
        end
    end
    return env
end
"""
def match(literal, fact):
    env = {}
    for term, factterm in zip(literal.terms, fact.terms):
        if term != factterm:
            try:
                if eval(term.id) == eval(factterm.id):
                    continue
            except:
                pass
            env = term.match(factterm, env)
            if env == None: return env
    return env
"""
function Const:match(const, env)
    return nil
end

function Var:match(const, env)
    local term = env[self]
    if not term then
        env[self] = const
        return env
    elseif term == const then
        return env
    else
        return nil
    end
end
"""
Const.match = lambda self, const, env : None
def _(self, const, env):
    if self not in env:
        env[self] = const
        return env
    elif env[self] == const:
        return env
    else:
        return None
Var.match = _
Fresh_var.match = _

# Add a primitives that is defined by an iterator.  When given a
# literal, the iterator generates a sequences of answers.  Each
# answer is an array.  Each element in the array is either a number
# or a string.  The length of the array is equal to the arity of the
# predicate.

"""
local function add_iter_prim(name, arity, iter)
    local pred = make_pred(name, arity)
    local function prim(literal, subgoal)
        for terms in iter(literal) do
            local n = #terms
            if n == arity then
                local new = {pred = pred}
                for i=1,n do
                    new[i] = make_const(tostring(terms[i]))
                end
                if match(literal, new) then
                    fact(subgoal, new)
                end
            end
        end
    end
    pred.prim = prim
    return insert(pred)
end
"""
def add_iter_prim_to_predicate(pred, iter): # separate function to allow re-use
    def prim(literal, subgoal, pred=pred, iter=iter):
        for terms in iter(literal):
            if len(terms) == len(literal.terms):
                new = make_literal(pred, [make_const(term) for term in terms])
                if match(literal, new) != None:
                    fact(subgoal, new)
    pred.prim = prim
    
def add_iter_prim(name, arity, iter): # TODO
    pred = make_pred(name, arity)
    add_iter_prim_to_predicate(pred, iter)
    return insert(pred)

"""
--[[

# Example of a primitive defined by an iterator.

add_iter_prim("three", 1,
    function(literal)
        return function(s, v)
                   if v then
                       return nil
                   else
                       return {3}
                   end
                end
    end
)

--]]
"""

# Support for expression

# Expressions (such as X = Y-1) are represented by a predicate (P(X,Y)) with :
#    an attached expression Y-1 (stored in pred.expression)
#    an operator (=, <=, >=, !=) (stored in pred.operator)
#    and an iterative function (stored in pred.prim)

# An Operand is either a constant or an index in the list of arguments of a literal
# e.g. in P(X,Y) for X = Y-1, Y has an index of 1

"""
local Operand = {}
Operand.__index = Operand
function make_operand(type, value)
    local operand = {}
    if type == 'constant' then
        operand.value = value
    else
        operand.index = value
    end
    setmetatable(operand, Operand)
    return operand
end

function Operand:eval(environment)
    if self.index then
        return environment[self.index]
    else
        return self.value
    end
end
"""
class Operand:
    def __init__(self, type, value):
        self.value = value if type == 'constant' else None
        self.index = value if type != 'constant' else None
    def eval(self, environment):
        return environment[self.index-1] if self.index != None else self.value
        
def make_operand(type, value):
    return Operand(type, value)

"""
# an expression is specified by an operator and 2 operands

local Expression = {}
Expression.__index = Expression
function make_expression(operator, operand1, operand2)
    local expression = {operator=operator, operand1=operand1, operand2=operand2}
    setmetatable(expression, Expression)
    return expression
end

function Expression:eval(env)
    if self.operator == '+' then
        return self.operand1:eval(env) + self.operand2:eval(env)
    elseif self.operator == '-' then
        return self.operand1:eval(env) - self.operand2:eval(env)
    elseif self.operator == '*' then
        return self.operand1:eval(env) * self.operand2:eval(env)
    elseif self.operator == '/' then
        return self.operand1:eval(env) / self.operand2:eval(env)
    end
end
"""
class Expression:
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
        assert False
        
def make_expression(operator, operand1, operand2):
    return Expression(operator, operand1, operand2)

# this functions adds an expression to an existing predicate

"""
function add_expr_to_predicate(pred, operator, expression)
    local function expression_iter(literal) # modified from add() in e-mail from John
        return function(s, v)
            if v then
                return nil
            else
                local x = literal[1]
                local args = {}
                for i, y in ipairs(literal) do
                    if 1 < i then --ignore x
                        if not y.is_const() then return nil    end
                        table.insert(args, tonumber(y.id))
                    end
                end

                X = literal.pred.expression:eval(args)
                if literal.pred.operator == "=" and not x:is_const() then
                    table.insert(args, 1, X)
                    return args
                elseif literal.pred.operator == "<" and x:is_const() and x.id < X
                  or  literal.pred.operator == ">" and x:is_const() and x.id > X
                  or  literal.pred.operator == "<=" and x:is_const() and x.id <= X
                  or  literal.pred.operator == ">=" and x:is_const() and x.id >= X
                  or  literal.pred.operator == "~=" and x:is_const() and tonumber(x.id) ~= X
                then
                    table.insert(args, 1, x.id)
                    return args
                end
                return nil
            end
      end
    end
    """
def add_expr_to_predicate(pred, operator, expression):
    def expression_iter(literal):
        x = literal.terms[0]
        args = []
        for term in literal.terms[1:]:
            if not term.is_const():
                return
            args.append(term.id)
            
        X = literal.pred.expression.eval(args)
        if literal.pred.operator == "=" and not x.is_const():
            args.insert(0,X)
            yield args
        elif ((literal.pred.operator == "<" and x.is_const() and x.id < X)
          or  (literal.pred.operator == ">" and x.is_const() and x.id > X)
          or  (literal.pred.operator == "<=" and x.is_const() and x.id <= X)
          or  (literal.pred.operator == ">=" and x.is_const() and x.id >= X)
          or  (literal.pred.operator == "~=" and x.is_const() and x.id != X)):
            args.insert(0,x.id)
            yield args


    """
    local function prim(literal, subgoal)
        for terms in expression_iter(literal) do
            local n = #terms
            if n == #literal then
                local new = {pred = pred}
                for i=1,n do
                    new[i] = datalog.make_const(tostring(terms[i]))
                end
                if match(literal, new) then
                    fact(subgoal, new)
                end
            end
        end
    end
    pred.prim = prim
    pred.operator = operator
    pred.expression = expression
    datalog.insert(pred)
end
"""
    add_iter_prim_to_predicate(pred, expression_iter)
    pred.operator = operator
    pred.expression = expression
    insert(pred)

"""

# The Lua API

datalog = {
    make_var = make_var,
    make_const = make_const,
    make_pred = make_pred,
    get_name = get_name,
    get_arity = get_arity,
    make_literal = make_literal,
    make_clause = make_clause,
    insert = insert,
    remove = remove,
    assert = assert,
    retract = retract,
    save = save,
    restore = restore,
    copy = copy,
    revert = revert,
    ask = ask,
    ask2 = ask2,
    add_iter_prim = add_iter_prim,

    make_operand = make_operand,
    make_expression = make_expression,
    add_expr_to_predicate = add_expr_to_predicate
}

return datalog
"""