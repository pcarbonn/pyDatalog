#TODO cpdef --> cdef

import cython
from cpython cimport bool #TODO

cdef class Term(object):
    cdef public object id
    
    cpdef bool is_const(self)


cdef class Fresh_var(Term):

    #def __init__(self) #can't have type
    cpdef bool is_const(self)
    
    cdef public object get_tag(self, dict env)
    cdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict match(self, Term constant, dict env)
    cdef public dict unify(self, Term term, dict env)
    
cdef class Var(Fresh_var):
    cdef public dict unify(self, Term term, dict env)
    
cdef class Const(Term):
    cpdef bool is_const(self)

    cdef public object get_tag(self, dict env)
    cdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cpdef public dict match(self, Term constant, dict env)
    cdef public dict unify(self, Term term, dict env)
    
cdef class VarTuple(Term):
    cpdef public object _id
    cpdef public object is_constant

    cpdef bool is_const(self)
    @cython.locals(result=list)
    cdef public object get_tag(self, dict env)
    @cython.locals(result=list)
    cdef public Term subst(self, dict env)

    cpdef public Term shuffle(self, dict env)
    cdef public dict unify(self, Term term, dict env)

cdef class Operation(Term):
    cpdef public object operator
    cpdef public object operator_id
    cpdef public object lhs
    cpdef public object rhs
    cpdef public object is_constant

    cpdef bool is_const(self)
 
    cdef public object get_tag(self, dict env)
    cdef public Term subst(self, dict env)
    cpdef public Term shuffle(self, dict env)
    cpdef public Term chase(self, dict env)
    cdef public dict unify(self, Term term, dict env)

cdef class Literal(object):
    cpdef public list terms
    cpdef public object pred
    cpdef public tuple id
    cpdef public tuple tag
    cpdef public object aggregate 

    @cython.locals(env=dict, result=list)
    cpdef public object get_tag(self)        

    @cython.locals(result=list)
    cpdef public object subst(self, dict env)        

    @cython.locals(literal_i=Term, other_i=Term)
    cpdef public dict unify(self, Literal other)        