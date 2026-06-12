
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
    cdef public object _id
    cdef public object is_constant

    cpdef bool is_const(self)
    cpdef public object get_tag(self, dict env)
    cpdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict unify(self, Term term, dict env)

cdef class Operation(Term):
    cdef public object operator
    cdef public object operator_id
    cdef public object lhs
    cdef public object rhs
    cdef public object is_constant

    cpdef bool is_const(self)
 
    cpdef public object get_tag(self, dict env)
    cpdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict unify(self, Term term, dict env)

cdef class Literal(object):
    cdef public list terms
    cdef public object pred
    cdef public tuple id
    cdef public tuple tag
    cdef public object aggregate 

    cpdef public tuple get_tag(self)        
    cpdef public Literal subst(self, dict env)        
    cpdef public object shuffle(self, dict env)        
    cpdef public dict unify(self, Literal other)
    
    cpdef public tuple get_id(self)
    cpdef public tuple get_fact_id(self)
    cpdef public dict match(self, list terms)

cdef class Interned(object):
    pass

cdef class Pred(Interned):
    cdef public object id
    cdef public object name
    cdef public object arity
    cdef public object prearity
    cdef public object prefix
    cdef public object suffix
    cdef public object comparison
    cdef public object db
    cdef public object clauses
    cdef public object index
    cdef public object prim
    cdef public object expression
    cdef public bool recursive
    cdef public object base_pred
    cdef public object _cls
    
cdef class Clause(object):
    cdef public Literal head
    cdef public list body
    cdef public tuple id

cdef class Subgoal(object):
    cdef public Literal literal
    cdef public object facts
    cdef public list waiters
    cdef public object tasks
    cdef public list clauses
    cdef public bool recursive
    cdef public bool is_done
    cdef public list on_completion_
 
    #cpdef public add_clause(self, Clause clause)
    cpdef public fact(self, object literal)
    cpdef public fact_candidate(self, object class0, object result)
    cpdef public rule(self, Clause clause, Literal selected)
    cpdef public schedule(self, tuple task)
    cpdef public schedule_search(self, Subgoal subgoal)
    #cpdef public searching(self, Subgoal subgoal)
    cpdef public complete(self, Literal literal, object aggregate=*)
    #cpdef public on_completion