rem verify version in version.py too !

set version=0.16.0rc1
set PKG_REPO="C:\Users\pcarbonn\Documents\98 Eclipse\pyDatalog"
set ENV27=c:\python27
set ENV33=c:\python33
set ENV34=c:\python34

set ENV2764=c:\python27-64
set ENV3364=c:\python33-64
set ENV3464=c:\python34-64

:: delete old files
set BASEPATH=%PATH%
cd %PKG_REPO%\wheelhouse
del /Q *.* 

cd %PKG_REPO%\build\lib
del /Q *.*
cd %PKG_REPO%

::
:: create regular package
::

c:\python27\python.exe setup.py sdist


::
:: create wheels
:: source : http://cowboyprogrammer.org/building-python-wheels-for-windows/
::
set DISTUTILS_USE_SDK=1
set MSSdk=1
:: cythonize pyEngine.py, to create pyEngine.c
%ENV27%\scripts\cython.exe pyDatalog\pyEngine.py

::
:: Python3 32 bit
:: Visual Studio 10

call "C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\vcvarsall.bat"
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /Release /x86

:: Python3.3 32bit
%ENV33%\scripts\pip.exe  install wheel 
%ENV33%\scripts\pip.exe  wheel --no-deps %PKG_REPO%
%ENV33%\scripts\pip.exe  uninstall -y pyDatalog
%ENV33%\scripts\pip.exe  install %PKG_REPO%\wheelhouse\pyDatalog-%version%-cp33-none-win32.whl

:: Python3.4 32bit
%ENV34%\scripts\pip.exe  install wheel 
%ENV34%\scripts\pip.exe  wheel --no-deps %PKG_REPO%
%ENV34%\scripts\pip.exe  uninstall -y pyDatalog
%ENV34%\scripts\pip.exe  install %PKG_REPO%\wheelhouse\pyDatalog-%version%-cp34-none-win32.whl

::
:: Python 3 64 bit
::
set PATH=C:\Program Files (x86)\Microsoft Visual Studio 10.0\VC\bin\amd64;%PATH%
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /Release /x64
 
:: Python3.3 64bit
%ENV3364%\scripts\pip.exe  install wheel 
%ENV3364%\scripts\pip.exe  wheel --no-deps %PKG_REPO%
%ENV3364%\scripts\pip.exe  uninstall -y pyDatalog
%ENV3364%\scripts\pip.exe  install %PKG_REPO%\wheelhouse\pyDatalog-%version%-cp33-none-win_amd64.whl

:: Python3.4 64bit
%ENV3464%\scripts\pip.exe  install wheel 
%ENV3464%\scripts\pip.exe  wheel --no-deps %PKG_REPO%
%ENV3464%\scripts\pip.exe  uninstall -y pyDatalog
%ENV3464%\scripts\pip.exe  install %PKG_REPO%\wheelhouse\pyDatalog-%version%-cp34-none-win_amd64.whl
 
:: Restore path and environment
set PATH=%BASEPATH%

::
:: Python 27
:: Visual Studio 9

:: Python 27 64 bits
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /Release /x64
set PATH=C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\amd64;%PATH%
%ENV2764%\scripts\pip.exe install wheel
%ENV2764%\scripts\pip.exe wheel --no-deps %PKG_REPO%
%ENV2764%\scripts\pip.exe  uninstall -y pyDatalog
%ENV2764%\scripts\pip.exe  install %PKG_REPO%\wheelhouse\pyDatalog-%version%-cp27-none-win_amd64.whl

:: Python 27 32 bits
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.Cmd" /Release /x86
call "C:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\vcvarsall.bat"
%ENV27%\scripts\pip.exe install wheel
%ENV27%\scripts\pip.exe wheel --no-deps %PKG_REPO%
%ENV27%\scripts\pip.exe  uninstall -y pyDatalog
%ENV27%\scripts\pip.exe  install %PKG_REPO%\wheelhouse\pyDatalog-%version%-cp27-none-win32.whl

pause