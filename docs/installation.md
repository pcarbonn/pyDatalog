# Installation

pyDatalog can be easily installed from the [python package library](http://www.google.com/url?q=http%3A%2F%2Fpypi.python.org%2Fpypi&sa=D&sntz=1&usg=AOvVaw3YwUGVPBvCfNRgl2Kta9bS) (pypi).

Installation on Windows

  * [install python](http://www.google.com/url?q=http%3A%2F%2Fwww.python.org%2Fgetit%2F&sa=D&sntz=1&usg=AOvVaw3qfZGjZ7DEqViLH-jx4p5H), or better, [pypy](http://www.google.com/url?q=http%3A%2F%2Fpypy.org%2Fdownload.html%23default-with-a-jit-compiler&sa=D&sntz=1&usg=AOvVaw0ec4mWRgFFcJRPboV0BjFn), in a directory whose path does not have spaces
  * download and run [pip-Win](https://sites.google.com/site/pydatalog/python/pip-for-windows), our tiny package manager for Windows
  * enter `pip install pyDatalog` in the command box of pip-Win, and click run
  * enter `pip install `[sqlalchemy](http://www.google.com/url?q=http%3A%2F%2Fpypi.python.org%2Fpypi%2FSQLAlchemy%2F&sa=D&sntz=1&usg=AOvVaw2k-qxqHNHhY6_K7QBsAs8J) in the command box, and click run

Although you can start programming in Python's IDLE, it is recommended to use a development environment, such as [PyCharm](http://www.google.com/url?q=http%3A%2F%2Fwww.jetbrains.com%2Fpycharm%2F&sa=D&sntz=1&usg=AOvVaw2FHwto7DNk_ggr4S-QoO0T).

To later upgrade pyDatalog, restart pip-Win, and enter` pip install --upgrade pyDatalog` in the command box (or click Upgrade all packages).

Installation on other platforms, using pip

  * [install python](http://www.google.com/url?q=http%3A%2F%2Fwww.python.org%2Fgetit%2F&sa=D&sntz=1&usg=AOvVaw3qfZGjZ7DEqViLH-jx4p5H)
  * install [pip](http://www.google.com/url?q=http%3A%2F%2Fwww.pip-installer.org%2Fen%2Flatest%2F&sa=D&sntz=1&usg=AOvVaw1n7CIJ8BoMd5B0YAYxaFXF) (including [prerequisites](http://www.google.com/url?q=http%3A%2F%2Fwww.pip-installer.org%2Fen%2Flatest%2Finstalling.html%23prerequisites&sa=D&sntz=1&usg=AOvVaw1bkl1-V49GyMVBxPDPzB5B))
  * type `pip install pyDatalog`
  * type `pip install `[sqlalchemy](http://www.google.com/url?q=http%3A%2F%2Fpypi.python.org%2Fpypi%2FSQLAlchemy%2F&sa=D&sntz=1&usg=AOvVaw2k-qxqHNHhY6_K7QBsAs8J)

To verify installation, start python IDLE, then type `from pyDatalog import pyDatalog` (or try `import pyDatalog`).

Although you can start programming in Python's IDLE, it is recommended to use a development environment, such as [PyCharm](http://www.google.com/url?q=http%3A%2F%2Fwww.jetbrains.com%2Fpycharm%2F&sa=D&sntz=1&usg=AOvVaw2FHwto7DNk_ggr4S-QoO0T).

To install an update of pyDatalog, use `pip install --upgrade pyDatalog` (or [upgrade all your packages](http://www.google.com/url?q=http%3A%2F%2Fstackoverflow.com%2Fquestions%2F2720014%2Fupgrading-all-packages-with-pip&sa=D&sntz=1&usg=AOvVaw1APOZLIyHnZfI3dP4r5Ek4))

Installation on pypy

  * install [pypy](http://www.google.com/url?q=http%3A%2F%2Fpypy.org%2Fdownload.html%23default-with-a-jit-compiler&sa=D&sntz=1&usg=AOvVaw0ec4mWRgFFcJRPboV0BjFn)
  * [download the zip file from pypi](http://www.google.com/url?q=http%3A%2F%2Fpypi.python.org%2Fpypi%2FpyDatalog%2F&sa=D&sntz=1&usg=AOvVaw2qmGPRfZoIlLziTYDLYaTO), and unzip it to a directory (it will include a setup.py file)
  * go to that unzipped directory in the shell
  * run "`pypy setup.py install`" from the shell (or "`sudo pypy setup.py install`" to have admin privilege). Add the full path to pypy if needed. If setup.py is not found, try with S etup.py.
  * install [SQLAlchemy](http://www.google.com/url?q=http%3A%2F%2Fpypi.python.org%2Fpypi%2FSQLAlchemy%2F&sa=D&sntz=1&usg=AOvVaw2k-qxqHNHhY6_K7QBsAs8J) in a similar way

To verify installation, run a program using pypy < full path to a pyDatalog example file> . Add the full path to pypy if needed.