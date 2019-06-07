@echo off
rem Compare last reports with reference one 
rem
rem Usage: inspect [eval_dir [base_dir]]
rem   eval_dir - report folder to be verified, by default "reports" 
rem   base_dir - reference report folder, by default "reports(best)" 

set EVAL_DIR=reports
if not "%1" == "" set EVAL_DIR=%~1

set BASE_DIR=reports(best)
if not "%2" == "" set BASE_DIR=%~2

for %%i in (%CD%) do set PARENT_DIR=%%~ni
if not "%INSPECT_SILENTLY%" == "yes"  echo %PARENT_DIR%

if not exist %EVAL_DIR% echo --- Nothing to inspect ---             & goto :EOF
if not exist %BASE_DIR% echo *** Error: Reference dir not foung *** & exit /B 1

set EVAL_FILE=%EVAL_DIR%\failed_tests_list.txt
set BASE_FILE=%BASE_DIR%\failed_tests_list.txt
fc "%EVAL_FILE%" "%BASE_FILE%" 1>nul 2>nul || goto :lbl_diff_error

set EVAL_FILE=%EVAL_DIR%\rep_summary.txt
set BASE_FILE=%BASE_DIR%\rep_summary.txt
if exist "%BASE_FILE%"  fc "%EVAL_FILE%" "%BASE_FILE%" 1>nul 2>nul || goto :lbl_diff_warning

echo --- Ok ---
goto :EOF


:lbl_diff_error
echo *** Error: "%EVAL_FILE%"  ***
exit /B 1

:lbl_diff_warning
echo --- Warning: "%EVAL_FILE%" ---
exit /B 0
