
import cython
from cpython cimport bool

cdef class Term(object):
    cdef public object id

cdef class Fresh_var(Term):

    #def __init__(self) #can't have type
    cpdef bool is_const(self)
    
    cpdef public object get_tag(self, dict env)
    cpdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict match(self, Term constant, dict env)
    cpdef public dict unify(self, Term term, dict env)
    
cdef class Var(Fresh_var):
    cpdef public dict unify(self, Term term, dict env)
    
cdef class Const(Term):
    cpdef bool is_const(self)

    cpdef public object get_tag(self, dict env)
    cpdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict match(self, Term constant, dict env)
    cpdef public dict unify(self, Term term, dict env)
    
cdef class VarTuple(Term):
    cpdef public object _id
    cpdef public object is_constant

    cpdef bool is_const(self)
    cpdef public object get_tag(self, dict env)
    cpdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict unify(self, Term term, dict env)

cdef class Operation(Term):
    cpdef public object operator
    cpdef public object operator_id
    cpdef public object lhs
    cpdef public object rhs
    cpdef public object is_constant

    cpdef bool is_const(self)
 
    cpdef public object get_tag(self, dict env)
    cpdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict unify(self, Term term, dict env)

cdef class Literal(object):
    cpdef public list terms
    cpdef public object pred
    cpdef public tuple id
    cpdef public tuple tag
    cpdef public object aggregate 

    cpdef public tuple get_tag(self)        
    cpdef public Literal subst(self, dict env)        
    cpdef public object shuffle(self, dict env)        
    cpdef public dict unify(self, Literal other)
    
    cpdef public tuple get_id(self)
    cpdef public tuple get_fact_id(self)
    cpdef public dict match(self, list terms)
    
cdef class Clause(object):
    cpdef public Literal head
    cpdef public list body
    cpdef public tuple id

cdef class Subgoal(object):
    cpdef public Literal literal
    cpdef public object facts
    cpdef public list waiters
    cpdef public object tasks
    cpdef public list clauses
    cpdef public bool recursive
    cpdef public bool is_done
    cpdef public list on_completion_
 
    #cpdef public add_clause(self, Clause clause)
    cpdef public fact(self, object literal)
    cpdef public fact_candidate(self, object class0, object result)
    cpdef public rule(self, Clause clause, Literal selected)
    cpdef public schedule(self, tuple task)
    cpdef public schedule_search(self, Subgoal subgoal)
    #cpdef public searching(self, Subgoal subgoal)
    cpdef public complete(self, Literal literal, object aggregate=*)
    #cpdef public on_completion