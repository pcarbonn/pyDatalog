# pyDatalog

[![Build Status](https://github.com/pcarbonn/pyDatalog/actions/workflows/build_wheels.yml/badge.svg)](https://github.com/pcarbonn/pyDatalog/actions)
[![Documentation Status](https://img.shields.io/readthedocs/pydatalog)](https://pydatalog.readthedocs.io/en/latest/?badge=latest)
[![PyPI version](https://img.shields.io/pypi/v/pyDatalog.svg)](https://pypi.org/project/pyDatalog/)
[![Python Versions](https://img.shields.io/pypi/pyversions/pyDatalog.svg)](https://pypi.org/project/pyDatalog/)
[![License](https://img.shields.io/pypi/l/pyDatalog.svg)](https://github.com/pcarbonn/pyDatalog/blob/master/LICENCE)

> [!NOTE]
> In view of the [300 stars on GitHub](https://seladb.github.io/StarTrack-js/#/) (Thanks !!), I have restarted support of this package in June 2026.

## Installation

You can install `pyDatalog` from PyPI using `pip`:

```bash
pip install pyDatalog
```

Or using `uv`:

```bash
uv add pyDatalog
```

> [!NOTE]
> Pre-compiled binary wheels are provided for most common platforms and Python versions. A **pure Python wheel** is also published on PyPI for universal compatibility. If a platform-specific wheel is not available, your package manager (like `pip` or `micropip`) will automatically install the pure Python wheel, which runs natively on WebAssembly/Pyodide, mobile environments (like Pydroid 3), and all other architectures without requiring a C compiler.
> If installation is requested from source (instead of from wheels) and fails, the library will gracefully fall back to its pure-Python implementation.

## Description

**pyDatalog** adds the logic programming paradigm to Python's toolbox, in a pythonic way.
You can now run logic queries on databases or Python objects, and use logic clauses to define python classes.
In particular, **pyDatalog** can be used as a query language:

* it can perform multi-database queries (from memory datastore, 11 relational databases, and noSQL database with
  appropriate connectors)
* it is more expressive than SQL, with a cleaner syntax;
* it facilitates re-use of SQL code snippet (e.g. for frequent joins or formula);


#### Datalog = SQL + recursivity

Datalog is a truly declarative language derived from Prolog, with strong academic foundations.  It complements Python
very well for:

* managing complex sets of related information (e.g. in data integration or the semantic web).
* simulating intelligent behavior (e.g. in games),
* performing recursive algorithms (e.g. in network protocol, code and graph analysis, parsing)
* solving discrete constraint problems.


#### As simple as Excel

Datalog excels at accelerated development: Datalog programs are often shorter than their Python equivalent,
and Datalog statements can be specified in any order, as simply as formula in a spreadsheet.

## Testimonials

pyDatalog is cited in [several scientific papers](https://scholar.google.com/scholar?hl=fr&as_sdt=0%2C5&q=pydatalog&btnG=) !

"Very neat.  Datalog rocks" - Marck Carter.  He has written a nice blog entry on unit conversion with Datalog.

"Congratulations for your wonderful work done in pyDatalog project" - Karamajit Kaur, researcher in India

You are also using pyDatalog ? [Let us know](mailto:pierre.carbonnelle@gmail.com).
