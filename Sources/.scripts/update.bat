@echo off
rem Updates external workplace folder with built components by ".update.bsc"
rem Usage: update [Release] [Workplace]
rem   Release   - update in enduser mode
rem   Workplace - target directory

rem ============================================= Configure
rem Set ###_SOURCE_DIR, ###_CONFIG_FILE, ###_LOG_DIR, ###_ACTION_FILE and read '.config.bsc'
call "%~dp0\setvars.bat"  UPDATE  .update.bsc  || exit /B 1

if "%~1" == "Release" call :lbl_set_release_action & shift /1

set UPDATE_TASK=Update %UPDATE_UNIT_NAME%

if not "%1" == ""          set  WORKPLACE_DIR=%1
if "%WORKPLACE_DIR%" == "" call "%~dp0\setenv_workplace.bat"


rem ============================================= Run Action
echo %UPDATE_TASK% into "%WORKPLACE_DIR%"

set UPDATE_errorlevel=0
for /F "eol=# tokens=1*" %%i IN (%UPDATE_ACTION_FILE%) do  call :lbl_update %%i %%j
if not "%UPDATE_errorlevel%" == "0" goto lbl_Error
goto lbl_Success


rem ============================================= Update component
:lbl_update
set UPDATE_ITEM_MODE=%3
if "%UPDATE_ITEM_MODE%" == "" set UPDATE_ITEM_MODE=%UPDATE_MODE%

if not "%SETUPT_ITEM_MODE%" == "%SETUP_MODE%"  goto :EOF

set UPDATE_TARGET=%WORKPLACE_DIR%\%2

call :lbl_make_dir "%UPDATE_TARGET%" || exit /B 1

set UPDATE_SOURCE=%~1
set UPDATE_SOURCE_LAST_SYMBOL=%UPDATE_SOURCE:~-1%

if     "%UPDATE_SOURCE_LAST_SYMBOL%" == "\"   xcopy "%UPDATE_SOURCE:~0,-1%" "%UPDATE_TARGET%" /Y /S || set UPDATE_errorlevel=1 
if not "%UPDATE_SOURCE_LAST_SYMBOL%" == "\"   copy /Y "%UPDATE_SOURCE%"     "%UPDATE_TARGET%"       || set UPDATE_errorlevel=1

goto :EOF 


rem ============================================= Make Directory
:lbl_make_dir
set TARGET_DIR=%~dp1
if not exist "%TARGET_DIR%" mkdir "%TARGET_DIR%"
if errorlevel 1 set UPDATE_errorlevel=1
goto :EOF 


rem ============================================= Set Release Action
:lbl_set_release_action
set UPDATE_MODE=release
goto :EOF 


rem ============================================= Script Messages
:lbl_Success
echo ======= %UPDATE_TASK% ======= OK!
goto :EOF

:lbl_Error
echo ******* %UPDATE_TASK% ******* Failed! =^> "%UPDATE_LOG_FILE%"
exit /B 1
