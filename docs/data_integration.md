# 3 - Datalog and data integration

### Relational databases

pyDatalog can be mixed with SQLAlchemy to provide powerful query facilities on relational databases, as shown in [this example](https://www.google.com/url?q=https%3A%2F%2Fbitbucket.org%2Fpcarbonn%2Fpydatalog%2Fsrc%2F92e47bb999da08cf789551e6e25d227495aa7b9f%2FpyDatalog%2Fexamples%2FSQLAlchemy.py%3Fat%3Ddefault&sa=D&sntz=1&usg=AOvVaw18FWHsRMtmIw94lyRiIc79).

The first step is to create a `Base` class that combines pyDatalog and SQLAlchemy capability, as follows :

```python
>>> from pyDatalog import pyDatalog, Logic
>>> Logic()  # initialize the pyDatalog engine
>>> from sqlalchemy.ext.declarative import declarative_base

>>> # define a base class with SQLAlchemy and pyDatalog capabilities
>>> Base = declarative_base(cls=pyDatalog.Mixin, metaclass=pyDatalog.sqlMetaMixin)

```

The second step is to associate a [SQLAlchemy session](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Form%2Ftutorial.html%23creating-a-session&sa=D&sntz=1&usg=AOvVaw37Q2ZQrGLNugZCTIUiHLoJ) to this `Base `class, with the appropriate [configuration](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Fcore%2Fengines.html&sa=D&sntz=1&usg=AOvVaw3FQ110J7YDtUcm7_NK9xja). pyDatalog will use this session to fetch data from the databases.

```python
>>> from sqlalchemy import create_engine
>>> from sqlalchemy.orm import sessionmaker

>>> # create database in memory
>>> engine = create_engine('sqlite:///:memory:', echo=False)

>>> # open a session on a database, then associate it to the Base class
>>> Session = sessionmaker(bind=engine)
>>> session = Session()

>>> Base.session = session

```

Classes that inherit from `Base` will now have both pyDatalog and SQLAlchemy capability. There are 2 ways to define the attributes of those classes : explicitly, or by inspecting the existing database tables. The first approach enables the creation of the tables in the relational database.


```python
>>> from sqlalchemy import Column, Integer, String, ForeignKey

>>> class Employee(Base): # Employee inherits from the Base class
...    __tablename__ = 'employee' # data are stored in the Employee table
...    name = Column(String, primary_key=True)
...    manager_name = Column(String, ForeignKey('employee.name'))
...    salary = Column(Integer)
...    def __repr__(self):
...        return "Employee: %s" % self.name

>>> # now create the table
>>> Base.metadata.create_all(engine)

```

The second approach can be used to reverse engineer an existing database, using [_table_args_](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Form%2Fextensions%2Fdeclarative.html%23table-configuration&sa=D&sntz=1&usg=AOvVaw29vgvLYHT0tZNwzVLEqoFS) :


```python
>>> class Employee(Base):
...    __tablename__ = 'employee'
...    __table_args__ = {'autoload':True} # autoload the model

```

In both cases, relations can be defined between tables.

The Employee class can now be defined with logic clauses and used in in-line queries, as explained in the [previous tutorial](oo_datalog.md). For example :


```python
>>> mary = Employee(name='Mary', salary=6300)
>>> session.add(mary)
>>> session.commit()

>>> # who has a salary of 6300 ?
>>> pyDatalog.create_symbols('X')

>>> (Employee.salary[X] == 6300)
>>> print(X)
[Employee: Mary]

```

pyDatalog uses the latest in-session data :


```python
>>> mary.salary = 3000 # modify in-session data

>>> (Employee.salary[X] == 3000)
>>> print(X)
[Employee: Mary]

```

Different classes in a python program can inherit from different Base classes, each having a session on different databases : pyDatalog queries will join data from the relevant databases, performing multi-database queries effortlessly. SQLAlchemy will issue the SQL statements in the proper [dialect ](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Fdialects%2Findex.html&sa=D&sntz=1&usg=AOvVaw1R11TbTcvUqt_09ICbvfqo)for each database. Data changes to instances of the classes can be persisted using [SQLAlchemy's transaction](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Fcore%2Fconnections.html&sa=D&sntz=1&usg=AOvVaw2hf7RHeTtfxyzbgKclToec) facilities. See [SQLAlchemy's documentation](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Form%2Findex.html&sa=D&sntz=1&usg=AOvVaw3VEKd--9SnK-uDJRNIdTc7) and [tutorial](http://www.google.com/url?q=http%3A%2F%2Fdocs.sqlalchemy.org%2Fen%2Frel_0_7%2Form%2Ftutorial.html&sa=D&sntz=1&usg=AOvVaw3tYN29glVCw513yQz5TOG7)  for more details.

### Non-relational databases

Queries on non-relational databases can be implemented using predicate resolvers written in python. They can be mixed with queries on relational databases using conjunctive queries or logic clauses.

[FoundationDB](http://www.google.com/url?q=http%3A%2F%2Ffoundationdb.com%2F&sa=D&sntz=1&usg=AOvVaw2sxLeMxHvYidK6i9-b5mEm) has a [nice tutorial](http://www.google.com/url?q=http%3A%2F%2Ffoundationdb.com%2Fdocumentation%2Flatest%2Fdatalog.html&sa=D&sntz=1&usg=AOvVaw3HIK-MlfykYlOuwcXrf6O3) for their pyDatalog binding. The example below illustrates a binding to [MongoDB](http://www.google.com/url?q=http%3A%2F%2Fwww.mongodb.org%2F&sa=D&sntz=1&usg=AOvVaw0uZvnt2a7YgtnkCeSamI8O). (see the [source](https://www.google.com/url?q=https%3A%2F%2Fbitbucket.org%2Fpcarbonn%2Fpydatalog%2Fsrc%2Fe885b1ae7dffc346d3bdc1e7d7a0d722a765ae30%2FpyDatalog%2Fexamples%2FMongo.py%3Fat%3Ddefault&sa=D&sntz=1&usg=AOvVaw2tHA8QLoBKu_uqiy7nglsW)).

The first step in the example is to create records in a MongoDB database.


```python
>>> from pymongo import MongoClient

>>> connection = MongoClient()
>>> db = connection.Employees
>>> profiles = db.profiles

>>> profiles_to_insert = [{"name": "John", "diploma": "MSc."},
...                       {"name": "Mary", "diploma": "EE"},
...                       {"name": "Sam", "diploma": "MBA"}]

>>> profiles.insert(profiles_to_insert)

```

For this to work, the mongoDB database must be started on the machine. See [pymongo tutorial](http://www.google.com/url?q=http%3A%2F%2Fapi.mongodb.org%2Fpython%2F2.0%2Ftutorial.html&sa=D&sntz=1&usg=AOvVaw2bqeGE4M7Qzs050isRVOuc).

The second step is to add a predicate resolver for `diploma `to the Employee class used in the section above, as explained in [the following tutorial](advanced.md).


```python
>>> from SQLAlchemy import Employee # import the SQLAlchemy example

>>> def _pyD_diploma2(cls, employee, diploma):
...     global profiles
...     if employee.is_const():
...         r = profiles.find_one({"name": employee.id.name})
...         if r:
...             yield (employee, r["diploma"])
...         return
...     raise AttributeError

>>> # attach the resolver to the Employee class
>>> Employee._pyD_diploma2 = classmethod(_pyD_diploma2)

```

This resolver search profiles by name. A search by diploma would be resolved in a similar way, using `find()`.

It then becomes easy to query both databases transparently:


```python
>>> from pyDatalog import pyDatalog

>>> X, N, Diploma = pyDatalog.create_symbols('X, N, Diploma')

>>> # Who has a salary of 6800 and a MSc. diploma
>>> (Employee.salary[X]==6800) & (Employee.diploma[X]=="MSc.")
>>> print(X)
[Employee: John]

```

Should `diploma` be later stored in the relational database, only the python resolver would need to be changed.