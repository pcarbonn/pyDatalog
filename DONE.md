(most recent first)


# Issue # 30

## Problem Description
When using python resolvers decorated with `@pyDatalog.predicate()`, the function returned was the original Python generator function. When this function was used inside inline rules or queries (e.g. `body = p(1, Y) & p(1, Y)`), it raised a `TypeError: unsupported operand type(s) for &: 'generator' and 'generator'` because Python generators do not support logical operators.

## Root Cause Analysis
In `pyDatalog/pyDatalog.py`, `_predicate(func)` registered the function in `Python_resolvers` but returned the original generator function `func` to the caller. This bound the variable in the Python local scope to the generator function itself, which does not support the operators `&` and `<=` defined on `Term` and `Literal` classes.

## Fix Applied
We modified `_predicate(func)` in `pyDatalog/pyDatalog.py` to return `pyParser.Term(func.__name__)` instead of `func`. Because the generator function is still registered in `pyEngine.Python_resolvers` under the key `'func_name/arity'`, the resolver is correctly invoked by the engine during query evaluation. But in the local Python scope, the function name is now bound to a `Term` object, which correctly evaluates to a `Query` (Literal) when called, enabling standard inline logical rules and queries to work.

# Issue # 31

## Problem Description
Redefining a predicate to be empty via `B(X) <= None` completely deregistered it, causing `AttributeError: Predicate without definition` when subsequently queried.

## Root Cause Analysis
In `pyParser.py`, `B(X) <= None` called `pyEngine.remove(self.lua.pred)` which deleted the predicate from both `Logic.tl.logic.Db` (defined database predicates) and `Pred_registry`. Consequently, the predicate was treated as undefined on subsequent queries, raising `AttributeError`.

## Fix Applied
We modified `__le__` in `pyDatalog/pyParser.py` so that when `body` is `None`, instead of completely removing the predicate from the database, we loop through and retract each fact/rule using `pyEngine.retract(clause)`. This retains the predicate in the database but clears all facts and rules associated with it, defining it as empty. 

We preserved the behavior of `del f[X]` (which clears rules/clauses with bodies while leaving facts intact, as per the documented behavior since version 0.14.0).

### Discussion on Possible Regression
By keeping the predicate registered in `Db` after clearing it (so it returns `[]` on query instead of raising `AttributeError`), some existing programs that relied on catching `AttributeError` to detect when a predicate was cleared or undefined might change in behavior. However, this is the correct and intended Datalog behavior, treating a cleared predicate as an empty relation rather than raising an exception.

# Issue # 19

## Problem Description
When a rule uses an unasserted (undefined) predicate in its body, e.g., `A(X,Y) <= has_fact(X) & other_stuff(X,Y)` where `has_fact` has no facts asserted, querying `A(X, Y)` throws an `AttributeError` instead of treating the unasserted predicate as an empty relation (which should evaluate to false/no results).

## Root Cause Analysis
During query evaluation, `pyDatalog` resolves each subgoal in the rule body. To resolve `has_fact(X)`, `Subgoal.search` is called. It checks if the predicate's ID is present in `Logic.tl.logic.Db` (which contains defined predicates). Since `has_fact` has never had any facts or rules asserted for it, it was never inserted into `Logic.tl.logic.Db`. As a result, the resolution engine falls through all checks and raises `AttributeError: Predicate without definition`.

## Fix Applied
We updated the `assert_` function in `pyDatalog/pyEngine.py` to register all unprefixed body predicates of a rule into `Logic.tl.logic.Db` when the rule is asserted (provided they are not already defined, not primitives, and the head is not a temporary query predicate). This places the unasserted predicates into the database with an empty set of clauses, allowing the subgoal resolver to find them and evaluate them as empty/false relations, rather than raising an exception. At the same time, because we skip temporary query predicates, typo detection for completely undefined predicates queried directly via `ask()` (e.g. `ask("z(a)")`) is preserved and still raises `AttributeError`.

After making this change, we compiled `pyEngine.py` to `pyEngine.c` using Cython, rebuilt the package, and verified it against the test suite, which completed successfully.


# Issue # 4

## Problem Description

Consider the following program:
```python
(p[X] == 0) <= (X == 0)
p[0] = 1
print(p[X] == 0)
```
The query should return no results because `p[0]` is uniquely defined as `1` by the last, higher-priority rule. However, pyDatalog incorrectly returned `X = 0` by applying the first, lower-priority rule.

## Root Cause Analysis
In pyDatalog, functional unicity is enforced in `Subgoal.fact()` by registering facts in a `self.facts` dict using a `fact_id` that only contains the function name and its input arguments (excluding the output value). Subsequent values derived for the same arguments are ignored.

However, if the query itself binds the value argument to a constant (e.g., `p[X] == 0`, internally compiled as the relation query `p[1]==(..., X, 0)`), then:
1. During subgoal evaluation, the higher-priority rule `p[0] = 1` does not unify with the query because `1 != 0`. So it is ignored and never added to the subgoal's facts list.
2. The lower-priority rule `(p[X] == 0) <= (X == 0)` matches the query and derives `p[1]==(..., 0, 0)`.
3. Since the overriding fact `p[0] = 1` was never added to the subgoal's facts list, the list is empty. Therefore, `p[0] == 0` is successfully added as a fact and returned as a solution.

### Solution
For the query `p[X] == 0`, the engine translates it to:
```python
(p[X] == Y1) & (Y1 == 0)
```
where `Y1` is a fresh variable.

This resolves the issue because:
1. Since the value term in the sub-query `p[X] == Y1` is now a variable (`Y1`), the higher-priority fact `p[0] = 1` successfully unifies and matches the subgoal.
2. This fact is added to the subgoal's facts list first (under the argument key `0`).
3. When the lower-priority rule `(p[X] == 0) <= (X == 0)` is subsequently evaluated, it attempts to derive `p[0] == 0`. However, because the subgoal's facts list already contains a value for the argument `0` (which is `1`), the engine's functional unicity check is correctly triggered, and the conflicting fact `p[0] == 0` is discarded.
4. Finally, the conjoined condition `Y1 == 0` is evaluated. Since `Y1` was uniquely resolved to `1`, the equality `1 == 0` fails, and the query correctly returns no results.

## Fix Applied
We implemented dynamic query translation at execution/search time in `Subgoal.search()` of `pyDatalog/pyEngine.py`. If a subgoal represents a function query, the comparison operator is `==`, and the last term (value) is bound (a constant), we translate it using a temporary clause:
`literal0 <= [literal1, literal2]`
where `literal1` replaces the value with a fresh variable `Y1`, and `literal2` is `Y1 == value`.

For example, when resolving the query `p[X] == 0` (internally represented by the literal `p[1]==(..., X, 0)`), the engine dynamically generates and asserts the following temporary clause:
```python
p[1]==0 <= p[1]==Y1 & (Y1 == 0)
```
This temporary clause only exists dynamically in memory as a task/waiter on the query resolution stack. It is never inserted into the global database (`Logic.tl.logic.Db`), meaning it is automatically garbage-collected as soon as the query evaluation completes.

## Alternatives Considered

1. **Parse-time transformation**: Rewriting `p[X] == 0` to `(p[X] == Y1) & (Y1 == 0)` at parse time.
   - *Drawback*: This fails when the value is a variable that is bound earlier in a conjoined query, e.g. `(Y == 0) & (p[X] == Y)`. At parse time `Y` is a variable, so it wouldn't be rewritten. At execution time, `Y` gets bound first, reproducing the bug.
2. **Subgoal caching & unification modification**: Modifying `Literal.get_tag()`, `Subgoal.__init__()`, and core resolution loops to ignore the value term during subgoal caching and filter it in a second step.
   - *Drawback*: Adding check overhead to the hot path `Literal.get_tag()` would slow down all subgoal lookups (including non-functional ones). Modifying core SLG resolution assertions like `assert env != None` is also more invasive.


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

