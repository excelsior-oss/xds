@echo off
rem Setup workplace folder
rem Usage: setup [Release] [Workplace]
rem   Release   - setup in enduser mode by ".setup-release.bsc"
rem   Workplace - target directory

set XDS_PRODUCT=XDS-x86
set SOURCE_DIR=%~dp0\..\Sources

set BUILD_SETUP_ARGS=
if "%~1" == "Release" set BUILD_SETUP_ARGS=Release & shift /1

if "%XDS_GLOBAL_LOG_DIR%" == "" set XDS_GLOBAL_LOG_DIR=%~dp0\log

if not "%1" == ""          set WORKPLACE_DIR=%1
if "%WORKPLACE_DIR%" == "" set WORKPLACE_DIR=%~dp0\%XDS_PRODUCT%

echo Setup %XDS_PRODUCT% workplace in "%WORKPLACE_DIR%" 
echo.

if not exist "%WORKPLACE_DIR%"  mkdir "%WORKPLACE_DIR%"

set XDS_SETUP_errorlevel=0
for /F "eol=# tokens=1*" %%i IN (%~dp0\.components.bsc) do  call :lbl_setup_workplace %%i 
if not "%XDS_SETUP_errorlevel%" == "0" goto lbl_Error
goto lbl_Success


rem ============================================= Setup worplace of components
:lbl_setup_workplace 
if not exist "%SOURCE_DIR%\%1\setup.bat"  goto :EOF
pushd "%SOURCE_DIR%\%1"
call setup.bat %BUILD_SETUP_ARGS% || set XDS_SETUP_errorlevel=1
popd
echo.
goto :EOF 


rem ============================================= Script Messages
:lbl_Success
echo %XDS_PRODUCT% workplace =^> "%WORKPLACE_DIR%"
echo ======= Setup %XDS_PRODUCT% workplace ======= OK!
goto :EOF

:lbl_Error
echo ******* Setup %XDS_PRODUCT% workplace ******* Failed!
exit /B 1
