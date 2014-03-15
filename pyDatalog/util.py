'''
Created on 28 mars 2013

source : 
    http://anandology.com/blog/using-iterators-and-generators/
    OrderedSet: http://code.activestate.com/recipes/576694/
'''
import sys
import threading

class DatalogError(Exception):
    def __init__(self, value, lineno, function):
        self.value = value
        self.lineno = lineno
        self.function = function
    def __str__(self):
        return "%s\nin line %s of %s" % (self.value, self.lineno, self.function)        


class Counter:
    lock = threading.RLock()
    def __init__(self):
        self.i = 0

    def __iter__(self):
        return self

    def next(self):
        with Counter.lock:
            self.i += 1
            return self.i

class lazy_property(object):
    '''
    meant to be used for lazy evaluation of an object attribute.
    property should represent non-mutable data, as it replaces itself.
    '''

    def __init__(self,fget):
        self.fget = fget
        self.func_name = fget.__name__

    def __get__(self,obj,cls):
        if obj is None:
            return None
        value = self.fget(obj)
        setattr(obj,self.func_name,value)
        return value
    
    
###################### support for Python 2 and 3 ####################################
# inspired by the six library

DEFAULT_ENCODING = sys.getdefaultencoding()

def no_code(x):
    return x

def encode(u):
    return u.encode(DEFAULT_ENCODING, "replace")

PY2 = sys.version_info[0] == 2

if PY2:
    string_types = basestring,
    unicode_type = unicode
    cast_to_str = encode
    
    import __builtin__ as builtins
    xrange = builtins.xrange
else:
    string_types = str,
    unicode_type = str
    cast_to_str = no_code
    xrange = range

if PY2:
    def exec_(_code_, _globs_=None, _locs_=None):
        """Execute code in a namespace."""
        if _globs_ is None:
            frame = sys._getframe(1)
            _globs_ = frame.f_globals
            if _locs_ is None:
                _locs_ = frame.f_locals
            del frame
        elif _locs_ is None:
            _locs_ = _globs_
        exec("""exec _code_ in _globs_, _locs_""")


    exec_("""def reraise(tp, value, tb=None):
    raise tp, value, tb
    """)

else:
    import builtins
    exec_ = getattr(builtins, "exec")

    def reraise(tp, value, tb=None):
        if value.__traceback__ is not tb:
            raise value.with_traceback(tb)
        raise value
