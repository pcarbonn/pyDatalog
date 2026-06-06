# Console

<iframe style="width: 100%; height: 480px; border: none; background: #1e1e1e; border-radius: 4px;" name="embedded_python_anywhere" src="https://www.pythonanywhere.com/embedded3/"></iframe>

To start using pyDatalog in this console, please copy-paste and run this setup block first to install and load the library:

```python
import subprocess, sys
subprocess.check_call([sys.executable, "-m", "pip", "install", "--user", "pyDatalog"])
from pyDatalog import pyDatalog

```


### Cheat Sheet

* `Ctrl-Shift-V` to paste from clipboard
* `Ctrl-D`, then `y`, then `F5` to restart the console
* See [IPython help](http://www.ipython.org/) for more

#### pyDatalog syntax:

Initialize terms first:
```python
pyDatalog.create_terms('p, q, r, a, b, f, s, X, Y, Z, N')

```

Asserting facts and defining rules:
```python
+ p(a)
p(X) <= q(X) & r(X)
+ (f[a] == b)
(s[X] == len_(Y)) <= p(X, Y)

```

Running queries:
```python
print(p(X))

```

#### Aggregate functions:

* `len_(Y)`
* `sum_(Y, for_each=Z)`
* `min_(Y, order_by=Z)`
* `rank_(for_each=Y, order_by=Z)`
* `concat_(Y, order_by=Z, sep=',')`
* `running_sum_(N, for_each=Y, order_by=Z)`