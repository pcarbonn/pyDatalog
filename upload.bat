rem please check version number before uploading on Pypi
pause
cd "C:\python\github\pyDatalog\build\lib"
del *.*
cd "C:\python\github\pyDatalog"
c:\python27\python.exe setup.py register sdist upload
rem c:\python27\python.exe setup.py bdist_wheel upload
pause