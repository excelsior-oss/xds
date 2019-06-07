@echo off
rem Setups external workplace folder by ".setup.bsc" 
rem Usage: setup [Release] [Workplace]
rem   Release   - setup in enduser mode 
rem   Workplace - target directory

rem ============================================= Configure
rem Set ###_SOURCE_DIR, ###_CONFIG_FILE, ###_LOG_DIR, ###_ACTION_FILE and read '.config.bsc'
call "%~dp0\setvars.bat"  SETUP  .setup.bsc  || exit /B 1

if "%~1" == "Release" call :lbl_set_release_action & shift /1

set SETUP_TASK=Setup %SETUP_UNIT_NAME% workspace
set SETUP_LOG_NAME=%SETUP_UNIT_NAME%_setup.out

if not exist "%SETUP_LOG_DIR%" mkdir "%SETUP_LOG_DIR%"

set SETUP_LOG_FILE=%SETUP_LOG_DIR%\%SETUP_LOG_NAME%
if exist "%SETUP_LOG_FILE%" del /Q "%SETUP_LOG_FILE%"

if not "%1" == ""          set  WORKPLACE_DIR=%1
if "%WORKPLACE_DIR%" == "" call "%~dp0\setenv_workplace.bat"


rem ============================================= Run Action
echo %SETUP_TASK% in "%WORKPLACE_DIR%"

set SETUP_errorlevel=0
for /F "eol=# tokens=1*" %%i IN (%SETUP_ACTION_FILE%) do  call :lbl_setup_workplace %%i %%j
if not "%SETUP_errorlevel%" == "0" goto lbl_Error
goto lbl_Success


rem ============================================= Setup workspace
:lbl_setup_workplace
set SETUPT_ITEM_MODE=%3
if "%SETUPT_ITEM_MODE%" == "" set SETUPT_ITEM_MODE=%SETUP_MODE%

if not "%SETUPT_ITEM_MODE%" == "%SETUP_MODE%"  goto :EOF

set SETUP_TARGET=%WORKPLACE_DIR%\%2

call :lbl_make_dir "%SETUP_TARGET%" || exit /B 1

copy /Y %1 "%SETUP_TARGET%" 1>> "%SETUP_LOG_FILE%" 2>>&1
if errorlevel 1 set SETUP_errorlevel=1

goto :EOF 


rem ============================================= Make Directory
:lbl_make_dir
set TARGET_DIR=%~dp1
if not exist "%TARGET_DIR%" mkdir "%TARGET_DIR%"
if errorlevel 1 set SETUP_errorlevel=1
goto :EOF 


rem ============================================= Set Release Action
:lbl_set_release_action
set SETUP_MODE=release
goto :EOF 


rem ============================================= Script Messages
:lbl_Success
echo --- OK ---
goto :EOF

:lbl_Error
echo ******* %SETUP_TASK% ******* Failed! =^> "%SETUP_LOG_FILE%"
exit /B 1

