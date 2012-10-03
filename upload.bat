rem please check version number before uploading on Pypi
pause
cd "C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog\build\lib"
del *.*
cd "C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog"
setup.py register sdist bdist_wininst upload
pause