'''
Created on 28 mars 2013

source : http://anandology.com/blog/using-iterators-and-generators/
'''

class DatalogError(Exception):
    def __init__(self, value, lineno, function):
        self.value = value
        self.lineno = lineno
        self.function = function
    def __str__(self):
        return "%s\nin line %s of %s" % (self.value, self.lineno, self.function)        


class Counter:
    def __init__(self):
        self.i = 0

    def __iter__(self):
        return self

    def next(self):
        self.i += 1
        return self.i
