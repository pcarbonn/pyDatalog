@echo off
echo Please check the version number in pyproject.toml before uploading to PyPI.
pause
uv build
if %errorlevel% neq 0 (
    echo Build failed!
    exit /b %errorlevel%
)
echo Dist built successfully. Publishing to PyPI...
uv publish
pause