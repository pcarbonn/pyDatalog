rem please check version number before uploading on Pypi
pause
cd "C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog\build\lib"
del *.*
cd "C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog"
c:\python27\python.exe setup.py register sdist upload
rem c:\python27\python.exe setup.py bdist_wheel upload
pause