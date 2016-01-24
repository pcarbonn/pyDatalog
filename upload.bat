rem please check version number before uploading on Pypi
pause
cd "C:\Python\GitHub\pyDatalog\build\lib"
del *.*
cd "C:\Python\GitHub\pyDatalog"
c:\python27\python.exe setup.py register sdist upload
rem c:\python27\python.exe setup.py bdist_wheel upload
pause