# Installation

`pyDatalog` can be easily installed from the [Python Package Index (PyPI)](https://pypi.org/project/pyDatalog/).

## Quick Installation

### Using pip
You can install `pyDatalog` and its optional dependencies (like SQLAlchemy) using standard `pip`:

```bash
pip install pyDatalog SQLAlchemy
```

### Using uv
If you manage your project using [uv](https://github.com/astral-sh/uv), you can run:

```bash
uv add pyDatalog SQLAlchemy
```

To run a script in an isolated environment directly:
```bash
uv run python your_script.py
```

---

## Installation on PyPy
`pyDatalog` fully supports [PyPy](https://www.pypy.org/) for accelerated JIT performance. You can install it using PyPy's `pip` tool:

```bash
pypy -m pip install pyDatalog SQLAlchemy
```

---

## Verifying the Installation

To verify that `pyDatalog` has been installed correctly, open a Python interactive shell and run:

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X, Y')
>>> print(X == Y)
```

If it executes without errors, the installation was successful!

---

## Development Environments
Although you can write logic code in Python's standard IDLE, it is highly recommended to use a modern IDE like [PyCharm](https://www.jetbrains.com/pycharm/) or [VS Code](https://code.visualstudio.com/).