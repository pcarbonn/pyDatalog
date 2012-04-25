--[[                                 Emacs -*- mode: lua -*-

@PACKAGE_NAME@ @VERSION@

A small Datalog interpreter written in Lua designed to be used via a
simple C API.

John D. Ramsdell
Copyright (C) 2004 The MITRE Corporation
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

]]

-- This file exports its contents for Lua users in package datalog.
-- The package is created at the end of the file.  All definitions
-- before the exports should be local definitions.

-- DATA TYPES

-- Internalize objects based on an identifier.

-- To make comparisons between items of the same type efficient, each
-- item is internalized so there is at most one of them associated
-- with an identifier.  An identifier is always a string.

-- For example, after internalization, there is one constant for each
-- string used to name a constant.

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

-- A term is either a variable or a constant.

-- Variables as simple objects.

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
    fresh_var_state = fresh_var_state + 1 -- To ensure freshness,
    return mk_var(id);                     -- don't intern this variable.
end

function Var:is_const()
    return false
end

-- Constants as simple objects.

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

-- Predicate symbols

-- A predicate symbol has a name, an arity, and a database table.  It
-- can also have a function used to implement a primitive.

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

local function last_slash(s)          -- Returns the location of the last slash
    local i = 0                                -- in a string or 0.
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

-- Duplicates a predicate.  Used to clone databases.
local function dup(pred)
    local db = {}
    for k,v in pairs(pred.db) do
        db[k] = v
    end
    return {id = pred.id, db = db, prim = pred.prim, expression = pred.expression}
end

-- Literals

-- A literal is a predicate and a sequence of terms, the number of
-- which must match the predicate's arity.

local function make_literal(pred_name, terms)
    local arity = #terms
    local pred = make_pred(pred_name, arity)
    local literal = {pred = pred}
    for i=1,arity do
        literal[i] = terms[i]
    end
    return literal
end

local function add_size(str)
    return tostring(string.len(str)) .. ":" .. str
end

-- A literal's id is computed on demand, but then cached.  It is used
-- by a clause when creating its id.

-- The id's encoding ensures that two literals are structurally the
-- same if they have the same id.

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

function Const:get_id()
    return "c" .. self.id
end

function Var:get_id()
    return "v" .. self.id
end

-- Variant tag

-- Two literal's variant tags are the same if there is a one-to-one
-- mapping of variables to variables, such that when the mapping is
-- applied to one literal, the result is a literal that is the same as
-- the other one, when compared using structural equality.  The
-- variant tag is used as a key by the subgoal table.

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

-- Substitution

-- An environment is a map from variables to terms.

local function subst(literal, env)
    if not next(env) then          -- Found an empty map.
        return literal
    end
    local arity = #literal
    local new = {pred = literal.pred}
    for i=1,arity do
        new[i] = literal[i]:subst(env)
    end
    return new
end

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

-- Shuffle creates an environment in which all variables are mapped to
-- freshly generated variables.

local function shuffle(literal, env)
    local map = env or {}
    for i=1,#literal do
        literal[i]:shuffle(map)
    end
    return map
end

function Const:shuffle(env)
end

function Var:shuffle(env)
    if not env[self] then
        env[self] = mk_fresh_var()
    end
end

-- Renames a literal using an environment generated by shuffle.

local function rename(literal)
    return subst(literal, shuffle(literal))
end

-- Unify two literals.  The result is either an environment or nil.
-- Nil is returned when the two literals cannot be unified.  When they
-- can, applying the substitutions defined by the environment on both
-- literals will create two literals that are structurally equal.

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

-- Chase returns a constant or an unbound variable.

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

-- The case analysis for unifying two terms is handled by method
-- dispatch.

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

-- Does a literal have a given term?  Internalizing terms ensures an
-- efficient implementation of this operation.

local function is_in(term, literal)
    for i=1,#literal do
        if literal[i] == term then
            return true
        end
    end
    return false
end

-- These methods are used to handle a set of facts.

local function is_member(literal, tbl)
    return tbl[get_id(literal)]
end

local function adjoin(literal, tbl)
    tbl[get_id(literal)] = literal
end

-- Clauses

-- A clause has a head literal, and a sequence of literals that form
-- its body.  If there are no literals in its body, the clause is
-- called a fact.  If there is at least one literal in its body, it is
-- called a rule.

-- A clause asserts that its head is true if every literal in its body is
-- true.

local function make_clause(head, body)
    local clause = {head = head}
    for i=1,#body do
        clause[i] = body[i]
    end
    return clause
end

-- A clause's id is computed on demand, but then cached.

-- The id's encoding ensures that two clauses are structurally equal
-- if they have the same id.  A clause's id is used as a key into the
-- clause database.

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

-- Clause substitution in which the substitution is applied to each
-- each literal that makes up the clause.

local function subst_in_clause(clause, env)
    if not next(env) then          -- Found an empty map.
        return clause
    end
    local new = {head = subst(clause.head, env)}
    for i=1,#clause do
        new[i] = subst(clause[i], env)
    end
    return new
end

-- Renames the variables in a clause.  Every variable in the head is
-- in the body, so the head can be ignored while generating an
-- environment.

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

-- A clause is safe if every variable in its head is in its body.

local function is_safe(clause)
    for i=1,#clause.head do
        if not clause.head[i]:is_safe(clause) then
            return false
        end
    end
    return true
end

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

-- DATABASE

-- The database stores predicates that contain clauses.  Predicates
-- not in the database are subject to garbage collection.

local db = {}

local function insert(pred)
    db[pred.id] = pred
    return pred
end

local function remove(pred)
    db[pred.id] = nil
    return pred
end

-- Add a safe clause to the database.

local function assert(clause)
    if not is_safe(clause) then
        return nil                     -- An unsafe clause was detected.
    else
        local pred = clause.head.pred
        if not pred.prim then          -- Ignore assertions for primitives.
            pred.db[get_clause_id(clause)] = clause
            insert(pred)
        end
        return clause
    end
end

local function retract(clause)
    local pred = clause.head.pred
    pred.db[get_clause_id(clause)] = nil
    if not next(pred.db) and not pred.prim then
        remove(pred)
    end
    return clause
end

-- DATABASE CLONING

-- A database can be saved and then later restored.  With copy and
-- revert, one can use one copy of a database multiple times to revert
-- to a previous database.  These two functions are not exposed in the
-- C API.

-- Returns a fresh copy of the current database or copies the one
-- given as an argument.

local function copy(src)
    local clone = {}
    for k,v in pairs(src or db) do
        clone[k] = dup(v)
    end
    return clone
end

-- Reverts datalog to a previously cloned database.  The database is
-- copied so that the clone can be used more than once.

local function revert(clone)
    db = copy(clone)
end

-- DATABASE STORE

-- A database can be saved and then later restored.

local store = {}

local function save()
    table.insert(store, copy())
end

local function restore()
    db = table.remove(store)
    db = db or {}
end

-- PROVER

--[[

The remaining functions in this file implement the tabled logic
programming algorithm described in "Efficient Top-Down Computation of
Queries under the Well-Founded Semantics", Chen, W., Swift, T., and
Warren, D. S., J. Logic Prog. Vol. 24, No. 3, pp. 161-199.  Another
important reference is "Tabled Evaluation with Delaying for General
Logic Programs", Chen, W., and Warren, D. S., J. ACM, Vol. 43, No. 1,
Jan. 1996, pp. 20-74.

]]

-- The subgoal table

local subgoals

-- The subgoal table is a map from the variant tag of a subgoal's
-- literal to a subgoal.

local function find(literal)
    return subgoals[get_tag(literal)]
end

local function merge(subgoal)
    subgoals[get_tag(subgoal.literal)] = subgoal
end

-- A subgoal is the item that is tabled by this algorithm.

-- A subgoal has a literal, a set of facts, and an array of waiters.
-- A waiter is a pair containing a subgoal and a clause.

local function make_subgoal(literal)
    return {literal = literal, facts = {}, waiters = {}}
end

-- Resolve the selected literal of a clause with a literal.  The
-- selected literal is the first literal in body of a rule.  If the
-- two literals unify, a new clause is generated that has a body with
-- one less literal.

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

 
-- A stack of thunks used to delay the evaluation of some expressions

local Fast=nil
local tasks

-- Schedule a task for later invocation

local function sched(thunk)
    if Fast then return thunk() end
    return table.insert(tasks, thunk)
end

-- Invoke the scheduled tasks

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

-- Store a fact, and inform all waiters of the fact too.

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

-- Use a newly derived rule.

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

function add_clause(subgoal, clause)
    if #clause == 0 then
        return fact(subgoal, clause.head)
    else
        return rule(subgoal, clause, clause[1])
    end
end

-- Search for derivations of the literal associated with this subgoal.

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

-- Sets up and calls the subgoal search procedure, and then extracts
-- the answers into an easily used table.  The table has the name of
-- the predicate, the predicate's arity, and an array of constant
-- terms for each answer.  If there are no answers, nil is returned.

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
        for i=1,#literal do -- Each term in an answer will be
            table.insert(answer, literal[i].id) -- a constant.
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

local function ask(query)
    return ask2(query, nil)
end

-- PRIMITIVES

--[[

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

]]

-- Other parts of the Datalog system depend on the equality primitive,
-- so carefully consider any modifications to it.

do                                          -- equals primitive
    local binary_equals_pred = make_pred("=", 2)

    local function equals_primitive(literal, subgoal)
        local x = literal[1]
        local y = literal[2]
        local env = x:unify(y, {})-- Both terms must unify,
        if env then                     -- and at least one of them
            x = x:subst(env)          -- must be a constant.
            y = y:subst(env)
        end
        return x:equals_primitive(y, subgoal)
    end

    function Var:equals_primitive(term, subgoal)
    end

    function Const:equals_primitive(term, subgoal)
        if self == term then          -- Both terms are constant and equal.
            local literal = {pred = binary_equals_pred, self, self}
            return fact(subgoal, literal)
        end
    end

    binary_equals_pred.prim = equals_primitive
end

-- Does a literal unify with an fact known to contain only constant
-- terms?

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

-- Add a primitives that is defined by an iterator.  When given a
-- literal, the iterator generates a sequences of answers.  Each
-- answer is an array.  Each element in the array is either a number
-- or a string.  The length of the array is equal to the arity of the
-- predicate.

local function add_iter_prim(name, arity, iter)
    local pred = make_pred(name, arity)
    local function prim(literal, subgoal)
        for terms in iter(literal) do
            local n = #terms
            if n == arity then
                local new = {pred = pred}
                for i=1,n do
                    new[i] = make_const(terms[i])
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

--[[

-- Example of a primitive defined by an iterator.

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

-- Support for expression

-- Expressions (such as X = Y-1) are represented by a predicate (P(X,Y)) with :
--    an attached expression Y-1 (stored in pred.expression)
--    an operator (=, <=, >=, !=) (stored in pred.operator)
--    and an iterative function (stored in pred.prim)

-- An Operand is either a constant or an index in the list of arguments of a literal
-- e.g. in P(X,Y) for X = Y-1, Y has an index of 1

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

-- an expression is specified by an operator and 2 operands

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

-- this function adds a primitive to an existing predicate

local function add_iter_prim_to_predicate(pred, iter) -- adapted from add_iter_prim
    local function prim(literal, subgoal)
        for terms in iter(literal) do
            local n = #terms
            if n == #literal then
                local new = {pred = pred}
                for i=1,n do
                    new[i] = datalog.make_const(terms[i])
                end
                if match(literal, new) then
                    fact(subgoal, new)
                end
            end
        end
    end
    pred.prim = prim
end

-- this functions adds an expression to an existing predicate

function add_expr_to_predicate(pred, operator, expression)
    local function expression_literal(literal) -- modified from add() in e-mail from John
        return function(s, v)
            if v then
                return nil
            else
                local x = literal[1]
                local args = {}
                for i, y in ipairs(literal) do
                    if 1 < i then --ignore x
                        if not y.is_const() then return nil    end
                        table.insert(args, y.id)
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
                  or  literal.pred.operator == "~=" and x:is_const() and x.id ~= X
                then
                    table.insert(args, 1, x.id)
                    return args
                end
                return nil
            end
      end
    end

    local function prim(literal, subgoal)
        for terms in expression_literal(literal) do
            local n = #terms
            if n == #literal then
                local new = {pred = pred}
                for i=1,n do
                    new[i] = datalog.make_const(terms[i])
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

-- The Lua API

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
