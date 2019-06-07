@echo off
rem Compare current "reports" with reference "reports(best)" 
rem Usage: inspect [Test]
rem   Test   - folder to inspect reports 

set INSPECT_TARGET=XDS-x86

if not "%~1" == ""  call :lbl_inspect_one %1 & goto :EOF

if not exist "%~dp0\.testing.bsc"  copy "%~dp0\.alltests.bsc"  "%~dp0\.testing.bsc"

set WAS_INSPECT_ERROR=no
for /F "eol=# tokens=1*" %%i IN (%~dp0\.testing.bsc) do  call :lbl_inspect_one %%i 

echo =================================================== 
if "%WAS_INSPECT_ERROR%" == "no"      echo --- %INSPECT_TARGET% --- Ok 
if not "%WAS_INSPECT_ERROR%" == "no"  echo *** %INSPECT_TARGET% *** Fail

goto :EOF


:lbl_inspect_one
pushd "%~1"
call inspect.bat || set WAS_INSPECT_ERROR=yes
echo.
popd
goto :EOF
