# -*- coding: utf-8 -*-
from pyDatalog.pyEngine import TaskList, TaskDeque

def test_task_queue_subscript():
    # First, test the version currently imported by the engine (which could be the C++ extension or the fallback)
    _run_subscript_tests(TaskList, TaskDeque)

    # Next, if the C++ extension is compiled/available, explicitly test it to ensure C-level subscripting works
    try:
        from pyDatalog.task_queue import TaskList as CTaskList, TaskDeque as CTaskDeque
        _run_subscript_tests(CTaskList, CTaskDeque)
    except ImportError:
        pass

def _run_subscript_tests(TaskListClass, TaskDequeClass):
    # Test TaskList
    tl = TaskListClass()
    tl.append("a")
    tl.append("b")
    assert len(tl) == 2
    assert tl[0] == "a"
    assert tl[1] == "b"
    assert tl[-1] == "b"
    assert tl[-2] == "a"
    
    try:
        tl[2]
        assert False, "IndexError not raised for positive out of bounds"
    except IndexError:
        pass
        
    try:
        tl[-3]
        assert False, "IndexError not raised for negative out of bounds"
    except IndexError:
        pass

    # Test TaskDeque
    td = TaskDequeClass()
    td.append("x")
    td.append("y")
    assert len(td) == 2
    assert td[0] == "x"
    assert td[1] == "y"
    assert td[-1] == "y"
    assert td[-2] == "x"
    
    try:
        td[2]
        assert False, "IndexError not raised for positive out of bounds"
    except IndexError:
        pass
        
    try:
        td[-3]
        assert False, "IndexError not raised for negative out of bounds"
    except IndexError:
        pass
