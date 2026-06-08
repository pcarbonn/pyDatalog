(most recent first)


# Issue # 19

## Problem Description
When a rule uses an unasserted (undefined) predicate in its body, e.g., `A(X,Y) <= has_fact(X) & other_stuff(X,Y)` where `has_fact` has no facts asserted, querying `A(X, Y)` throws an `AttributeError` instead of treating the unasserted predicate as an empty relation (which should evaluate to false/no results).

## Root Cause Analysis
During query evaluation, `pyDatalog` resolves each subgoal in the rule body. To resolve `has_fact(X)`, `Subgoal.search` is called. It checks if the predicate's ID is present in `Logic.tl.logic.Db` (which contains defined predicates). Since `has_fact` has never had any facts or rules asserted for it, it was never inserted into `Logic.tl.logic.Db`. As a result, the resolution engine falls through all checks and raises `AttributeError: Predicate without definition`.

## Fix Applied
We updated the `assert_` function in `pyDatalog/pyEngine.py` to register all unprefixed body predicates of a rule into `Logic.tl.logic.Db` when the rule is asserted (provided they are not already defined, not primitives, and the head is not a temporary query predicate). This places the unasserted predicates into the database with an empty set of clauses, allowing the subgoal resolver to find them and evaluate them as empty/false relations, rather than raising an exception. At the same time, because we skip temporary query predicates, typo detection for completely undefined predicates queried directly via `ask()` (e.g. `ask("z(a)")`) is preserved and still raises `AttributeError`.

After making this change, we compiled `pyEngine.py` to `pyEngine.c` using Cython, rebuilt the package, and verified it against the test suite, which completed successfully.


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

