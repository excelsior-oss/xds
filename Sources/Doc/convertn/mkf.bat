@echo off
rem Usage: mkf [Release]
rem   Release - make in enduser mode

set MAKE_SCRIPT_DIR=%~dp0\..\..\.scripts

rem ============================================= Parse Arguments
set MAKE_MODE=Debug
if "%~1" == "Release" set MAKE_MODE=Release

rem ============================================= Configure
for %%i in (workplace) do if not exist %%i mkdir %%i 

call "%MAKE_SCRIPT_DIR%\setenv.bat" "MSVC_HOME" || exit /B 1

rem ============================================= Make component
set MAKE_TASK=convernt
set MAKE_OPTIONS=CFG="convertn - Win32 %MAKE_MODE%" 

echo %~dp0
echo nmake /f convertn.mak %MAKE_OPTIONS%
call nmake /f convertn.mak %MAKE_OPTIONS%  || goto lbl_Error
goto lbl_Success


rem ============================================= Script Messages
:lbl_Success
echo ============ %MAKE_TASK% ============ OK!
goto :EOF

:lbl_Error
echo *********** %MAKE_TASK% *********** Failed!
exit /B 1
