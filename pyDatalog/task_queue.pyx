# cython: language_level=3
from libcpp.vector cimport vector
from libcpp.deque cimport deque
from cpython.ref cimport PyObject, Py_XINCREF, Py_XDECREF

cdef class TaskList:
    cdef vector[void*] tasks

    def __dealloc__(self):
        cdef int i
        for i in range(self.tasks.size()):
            Py_XDECREF(<PyObject*>self.tasks[i])

    cpdef void append(self, object task):
        Py_XINCREF(<PyObject*>task)
        self.tasks.push_back(<void*><PyObject*>task)

    cpdef object pop(self):
        if self.tasks.empty():
            raise IndexError("pop from empty list")
        cdef void* item = self.tasks.back()
        self.tasks.pop_back()
        cdef object py_item = <object>item
        Py_XDECREF(<PyObject*>item)
        return py_item

    cpdef void remove(self, object task):
        cdef int i
        cdef void* p_task = <void*><PyObject*>task
        for i in range(self.tasks.size()):
            if self.tasks[i] == p_task:
                self.tasks.erase(self.tasks.begin() + i)
                Py_XDECREF(<PyObject*>p_task)
                return
        for i in range(self.tasks.size()):
            if <object>self.tasks[i] == task:
                Py_XDECREF(<PyObject*>self.tasks[i])
                self.tasks.erase(self.tasks.begin() + i)
                return
        raise ValueError("list.remove(x): x not in list")

    cpdef void reverse(self):
        cdef vector[void*] new_tasks
        cdef int i
        for i in range(self.tasks.size()):
            new_tasks.insert(new_tasks.begin(), self.tasks[i])
        self.tasks = new_tasks

    def __bool__(self):
        return not self.tasks.empty()

    def __len__(self):
        return self.tasks.size()

    def __reversed__(self):
        cdef int i
        for i in range(self.tasks.size() - 1, -1, -1):
            yield <object>self.tasks[i]

    def __contains__(self, object task):
        cdef int i
        cdef void* p_task = <void*><PyObject*>task
        for i in range(self.tasks.size()):
            if self.tasks[i] == p_task:
                return True
        for i in range(self.tasks.size()):
            if <object>self.tasks[i] == task:
                return True
        return False

    def __iter__(self):
        cdef int i
        for i in range(self.tasks.size()):
            yield <object>self.tasks[i]

    def __getitem__(self, int index):
        cdef int size = self.tasks.size()
        if index < 0:
            index += size
        if index < 0 or index >= size:
            raise IndexError("list index out of range")
        return <object>self.tasks[index]


cdef class TaskDeque:
    cdef deque[void*] tasks

    def __dealloc__(self):
        cdef int i
        for i in range(self.tasks.size()):
            Py_XDECREF(<PyObject*>self.tasks[i])

    cpdef void append(self, object task):
        Py_XINCREF(<PyObject*>task)
        self.tasks.push_back(<void*><PyObject*>task)

    cpdef void appendleft(self, object task):
        Py_XINCREF(<PyObject*>task)
        self.tasks.push_front(<void*><PyObject*>task)

    cpdef object pop(self):
        if self.tasks.empty():
            raise IndexError("pop from empty deque")
        cdef void* item = self.tasks.back()
        self.tasks.pop_back()
        cdef object py_item = <object>item
        Py_XDECREF(<PyObject*>item)
        return py_item

    cpdef void remove(self, object task):
        cdef int i
        cdef void* p_task = <void*><PyObject*>task
        for i in range(self.tasks.size()):
            if self.tasks[i] == p_task:
                self.tasks.erase(self.tasks.begin() + i)
                Py_XDECREF(<PyObject*>p_task)
                return
        for i in range(self.tasks.size()):
            if <object>self.tasks[i] == task:
                Py_XDECREF(<PyObject*>self.tasks[i])
                self.tasks.erase(self.tasks.begin() + i)
                return
        raise ValueError("deque.remove(x): x not in deque")

    cpdef void reverse(self):
        cdef deque[void*] new_tasks
        cdef int i
        for i in range(self.tasks.size()):
            new_tasks.push_front(self.tasks[i])
        self.tasks = new_tasks

    def __bool__(self):
        return not self.tasks.empty()

    def __len__(self):
        return self.tasks.size()

    def __reversed__(self):
        cdef int i
        for i in range(self.tasks.size() - 1, -1, -1):
            yield <object>self.tasks[i]

    def __contains__(self, object task):
        cdef int i
        cdef void* p_task = <void*><PyObject*>task
        for i in range(self.tasks.size()):
            if self.tasks[i] == p_task:
                return True
        for i in range(self.tasks.size()):
            if <object>self.tasks[i] == task:
                return True
        return False

    def __iter__(self):
        cdef int i
        for i in range(self.tasks.size()):
            yield <object>self.tasks[i]

    def __getitem__(self, int index):
        cdef int size = self.tasks.size()
        if index < 0:
            index += size
        if index < 0 or index >= size:
            raise IndexError("deque index out of range")
        return <object>self.tasks[index]
