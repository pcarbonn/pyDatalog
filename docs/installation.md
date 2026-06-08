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

## Installation in WebAssembly (Pyodide)
Since `pyDatalog` is distributed with a pure Python wheel, it can be run out of the box in WebAssembly-based Python environments (like Pyodide or JupyterLite).

To load and use `pyDatalog` inside Pyodide, install it using `micropip`:

```python
import micropip
await micropip.install("pyDatalog")
```

After installation, you can import and use the library:
```python
from pyDatalog import pyDatalog
```

---

## Installation from Source

You can build and install `pyDatalog` directly from the source code. This is useful if you want to modify the source code or use development builds.

### 1. Download the Source
Clone the repository:
```bash
git clone https://github.com/pcarbonn/pyDatalog.git
cd pyDatalog
```

### 2. Install the Package
Run pip to install it in editable mode or from the local directory:
```bash
pip install -e .
```

> [!NOTE]
> During installation from source (instead of from pre-built wheels), the build system will attempt to compile the Cython speed-up extension. If compilation fails (e.g., due to a missing C compiler or missing Python header files), the installation will automatically and gracefully fall back to installing the pure Python implementation of the execution engine.

---

## Verifying the Installation

To verify that `pyDatalog` has been installed correctly, open a Python interactive shell and run:

```python
>>> from pyDatalog import pyDatalog
>>> pyDatalog.create_terms('X, Y')
>>> print(X == Y)
```

If it executes without errors, the installation was successful!

### Checking if the C extension is used
If you installed `pyDatalog` from source and want to verify whether the compiled C speed-up extension is active (instead of the pure Python fallback), import `pyEngine` and check its file path:

```python
>>> from pyDatalog import pyEngine
>>> print(pyEngine.__file__)
```

*   **Compiled C extension is active:** The file path will end in a compiled extension extension like `.so` (on Linux/macOS) or `.pyd` (on Windows).
*   **Pure Python fallback is active:** The file path is `pyEngine.py`.

