@echo off
call %~dp0\..\.config\config.bat RATS_REMOTE_DIR || goto :lbl_Error 

set SETUP_TIME_STAMP=%TIME:~0,-3%
set SETUP_TIME_STAMP=%SETUP_TIME_STAMP: =0%
set RATS_SETUP_DIR=%RATS_REMOTE_DIR%\%DATE:.=-%_%SETUP_TIME_STAMP::=.%

if not "%1" == "" set %1=%RATS_SETUP_DIR%

goto :EOF


:lbl_Error
echo ******* Setup RATS%* ******* Failed!
exit \B 1
