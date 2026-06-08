# Issue # 20

## Problem Description
In version 0.17.0, the negation of a conjunction, such as `~((A==1) & (A==1))`, did not work correctly. A query like `(A==1) & (B==2) & ~((A==1) & (A==1))` should have yielded no solution because `(1==1) & (1==1)` is `True`, making the negated condition `False`. However, pyDatalog incorrectly yielded `[(1,2)]`.

## Root Cause Analysis
During query evaluation, pyDatalog translates nested operations (like the internal conjunction `(A==1) & (A==1)`) into temporary predicates such as `_pyD_query1`. The engine executes the following steps:
1. `_pyD_query1` is created, and the object is initially added to both the global clause database (`Logic.tl.logic.Db`) and the registry for predicates (`Logic.tl.logic.Pred_registry`).
2. When the clause mapping `_pyD_query1` to the conjunction is evaluated and asserted, `assert_` calls `retract(clause)` to ensure the unicity of functions. 
3. `retract(clause)` includes a specific cleanup step for temporary query predicates (when `pred.db` length is 0), which forcefully calls `remove(pred)`. This removes the predicate from *both* `Db` and `Pred_registry`.
4. `assert_` then attempts to put the predicate back by calling `insert(pred)`. 
5. However, `insert(pred)` placed the predicate *back into `Db`*, but forgot to put it back into `Pred_registry`.
6. When the negation operator `~` executes, it attempts to resolve `_pyD_query1` by calling `Pred.__new__('_pyD_query1', 1)`. 
7. `Pred.__new__` looked in `Pred_registry` but, due to the bug in step 5, it couldn't find the original predicate. Assuming it didn't exist, it instantiated a **brand new** `_pyD_query1` predicate that had an empty clause list. 
8. Since this new predicate had no clauses, `_pyD_query1` evaluated to `False` (no results found), causing `~_pyD_query1` to incorrectly evaluate to `True`.

## Fix Applied
The fix targets the `insert(pred)` function inside `pyDatalog/pyEngine.py` (and the corresponding Cython generated code in `pyDatalog/pyEngine.c`). We updated the `insert` function to correctly append the predicate back to the `Pred_registry` as well:

```python
def insert(pred):
    Logic.tl.logic.Db[pred.id] = pred
    Logic.tl.logic.Pred_registry[pred.id] = pred
    return pred
```

After modifying the source, we cythonized `pyEngine.py` back into `pyEngine.c`, and rebuilt the C extension wheel. The test reproduction for `(A==1) & (B==2) & ~((A==1) & (A==1))` now correctly yields `[]`, and the built-in test suite completed successfully.
