'''
Created on 28 mars 2013

source : http://anandology.com/blog/using-iterators-and-generators/
'''
class Counter:
    def __init__(self):
        self.i = 0

    def __iter__(self):
        return self

    def next(self):
        self.i += 1
        return self.i
