cd "C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog\build\lib"
del *.*
cd "C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog"
c:\python27\python.exe Setup.py sdist
c:\python27\python.exe setup.py bdist_wheel upload
rem Setup.py bdist_wininst
pause