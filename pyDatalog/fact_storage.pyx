# cython: language_level=3
# distutils: language = c++
from libcpp.unordered_map cimport unordered_map
from libcpp.unordered_set cimport unordered_set
from libcpp.vector cimport vector
from cpython.ref cimport PyObject, Py_XINCREF, Py_XDECREF

cdef extern from "<shared_mutex>" namespace "std" nogil:
    cdef cppclass shared_mutex:
        shared_mutex()
        void lock()
        void unlock()
        void lock_shared()
        void unlock_shared()

cdef extern from "<mutex>" namespace "std" nogil:
    cdef cppclass unique_lock[T]:
        unique_lock(T&)
    cdef cppclass shared_lock[T]:
        shared_lock(T&)

cdef class FactDb:
    cdef unordered_map[Py_ssize_t, unordered_set[void*]] _map
    cdef shared_mutex _mutex

    def __dealloc__(self):
        cdef unordered_map[Py_ssize_t, unordered_set[void*]].iterator it = self._map.begin()
        cdef unordered_set[void*].iterator set_it
        while it != self._map.end():
            set_it = get_second(it).begin()
            while set_it != get_second(it).end():
                Py_XDECREF(<PyObject*>deref_ptr(set_it))
                set_it_inc(set_it)
            it_inc(it)

    cpdef void add(self, Py_ssize_t key_hash, object clause):
        cdef void* ptr = <void*><PyObject*>clause
        Py_XINCREF(<PyObject*>clause)
        with nogil:
            self._mutex.lock()
            self._map[key_hash].insert(ptr)
            self._mutex.unlock()

    cpdef void remove(self, Py_ssize_t key_hash, object clause):
        cdef void* ptr = <void*><PyObject*>clause
        cdef bint found = False
        with nogil:
            self._mutex.lock()
            if self._map.count(key_hash) > 0:
                if self._map[key_hash].erase(ptr) > 0:
                    found = True
                if self._map[key_hash].empty():
                    self._map.erase(key_hash)
            self._mutex.unlock()
            
        if found:
            Py_XDECREF(<PyObject*>ptr)

    cpdef list get_candidates(self, Py_ssize_t key_hash):
        """Returns a list of clauses matching the hash. Python must filter collisions."""
        cdef vector[void*] results
        cdef unordered_set[void*].iterator set_it
        
        with nogil:
            self._mutex.lock_shared()
            if self._map.count(key_hash) > 0:
                set_it = self._map[key_hash].begin()
                while set_it != self._map[key_hash].end():
                    results.push_back(deref_ptr(set_it))
                    set_it_inc(set_it)
            self._mutex.unlock_shared()
            
        # Re-acquired GIL
        cdef list py_results = []
        cdef size_t i
        for i in range(results.size()):
            py_results.append(<object>results[i])
        return py_results

    def values(self):
        """Returns all clauses in the db."""
        cdef vector[void*] results
        cdef unordered_map[Py_ssize_t, unordered_set[void*]].iterator it
        cdef unordered_set[void*].iterator set_it
        
        with nogil:
            self._mutex.lock_shared()
            it = self._map.begin()
            while it != self._map.end():
                set_it = get_second(it).begin()
                while set_it != get_second(it).end():
                    results.push_back(deref_ptr(set_it))
                    set_it_inc(set_it)
                it_inc(it)
            self._mutex.unlock_shared()
            
        cdef list py_results = []
        cdef size_t i
        for i in range(results.size()):
            py_results.append(<object>results[i])
        return py_results

    def __len__(self):
        cdef size_t total = 0
        cdef unordered_map[Py_ssize_t, unordered_set[void*]].iterator it
        with nogil:
            self._mutex.lock_shared()
            it = self._map.begin()
            while it != self._map.end():
                total += get_second(it).size()
                it_inc(it)
            self._mutex.unlock_shared()
        return total


cdef class FactIndex:
    cdef vector[unordered_map[Py_ssize_t, unordered_set[void*]]] _indices
    cdef shared_mutex _mutex

    def __init__(self, int arity):
        self._indices.resize(arity)

    def __dealloc__(self):
        # The fact pointers are owned by FactDb, so we don't DECREF here to avoid double-free
        # wait, we DO need to DECREF if FactIndex holds a reference! 
        # But `assert_` adds to both FactDb and FactIndex. If FactIndex INCREFs, it must DECREF.
        pass

    cpdef void add(self, int index_pos, Py_ssize_t key_hash, object clause):
        cdef void* ptr = <void*><PyObject*>clause
        Py_XINCREF(<PyObject*>clause) # INCREF for the index reference
        with nogil:
            self._mutex.lock()
            self._indices[index_pos][key_hash].insert(ptr)
            self._mutex.unlock()

    cpdef void remove(self, int index_pos, Py_ssize_t key_hash, object clause):
        cdef void* ptr = <void*><PyObject*>clause
        cdef bint found = False
        with nogil:
            self._mutex.lock()
            if self._indices[index_pos].count(key_hash) > 0:
                if self._indices[index_pos][key_hash].erase(ptr) > 0:
                    found = True
                if self._indices[index_pos][key_hash].empty():
                    self._indices[index_pos].erase(key_hash)
            self._mutex.unlock()
            
        if found:
            Py_XDECREF(<PyObject*>ptr)

    cpdef set get_candidates(self, int index_pos, Py_ssize_t key_hash):
        cdef vector[void*] results
        cdef unordered_set[void*].iterator set_it
        
        with nogil:
            self._mutex.lock_shared()
            if self._indices[index_pos].count(key_hash) > 0:
                set_it = self._indices[index_pos][key_hash].begin()
                while set_it != self._indices[index_pos][key_hash].end():
                    results.push_back(deref_ptr(set_it))
                    set_it_inc(set_it)
            self._mutex.unlock_shared()
            
        cdef set py_results = set()
        cdef size_t i
        for i in range(results.size()):
            py_results.add(<object>results[i])
        return py_results

cdef extern from *:
    """
    #define it_inc(it) (++it)
    #define set_it_inc(it) (++it)
    #define deref_ptr(it) (*it)
    #define get_second(it) (it->second)
    """
    void it_inc(...) nogil
    void set_it_inc(...) nogil
    void* deref_ptr(...) nogil
    unordered_set[void*]& get_second(...) nogil
