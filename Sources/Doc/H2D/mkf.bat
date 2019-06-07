@echo off
rem Usage: mkf Component Format
rem   Cmponent - component to make: isom2
rem   Format   - output format: html, pdf, chm

set MAKE_SCRIPT_DIR=%~dp0\..\..\.scripts

rem ============================================= Parse Arguments
set MAKE_COMPONENT=%~1

set TARGET_FMT=
if "%~2" == "pdf"   set TARGET_FMT=pdf
if "%~2" == "html"  set TARGET_FMT=html
if "%~2" == "chm"   set TARGET_FMT=chm

if "%TARGET_FMT%" == ""  (echo "Unknow format" & exit /B 1)

rem ============================================= Configure
for %%i in (..\workplace) do if not exist %%i mkdir %%i 

call "%MAKE_SCRIPT_DIR%\setenv.bat" "MSVC_HOME,MIKTEX,MS_HELP_WORKSHOP" || exit /B 1

if not exist "%~dp0\..\convertn\workplace\convertn.exe" call :lbl_build_convern

rem ============================================= Make component
set MAKE_TASK=%MAKE_COMPONENT%  %TARGET_FMT%
echo.  >..\Common\backend.tex
echo %~dp0
echo nmake /f %MAKE_COMPONENT%.mkf  TARGET_FMT=%TARGET_FMT%
call nmake /f %MAKE_COMPONENT%.mkf  TARGET_FMT=%TARGET_FMT% || goto lbl_Error
goto lbl_Success


rem ============================================= Make component
:lbl_build_convern
pushd ..\convertn
call build.bat || exit /B 1 
popd

rem ============================================= Script Messages
:lbl_Success
echo ============ %MAKE_TASK% ============ OK!
goto :EOF

:lbl_Error
echo *********** %MAKE_TASK% *********** Failed!
exit /B 1
