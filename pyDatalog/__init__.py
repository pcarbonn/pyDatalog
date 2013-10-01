
try:
    from . import version
    from . import Logic
except ValueError:
    import version
    import Logic
    
Logic = Logic.Logic # give easy access to the Logic class

Logic().clear() # initialize the logic in the current thread
    
