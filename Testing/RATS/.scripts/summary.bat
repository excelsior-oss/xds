@echo off
rem ------------------------------------------------------------------------------
rem                          Excelsior Rats System Utility
rem                            (c) 2014, Excelsior Ltd.
rem
rem Module:       summary
rem Author:       Lvov Konstantin
rem Modified By:  
rem Created:      10-Apr-2014
rem Purpose:      Analysis summary reports of test's groups and 
rem               creates final report 
rem Main:         
rem Required:     Pakages  - cygwin-b20: grep, cat
rem ------------------------------------------------------------------------------

set REP_DIR=%~1
set SUMMARY_DESC=%~2

rem --------------- Create output file name
set SUMMARY_FILE_NAME=rep_summary.txt
set TMP_REP_FILE=%REP_DIR%\rep_summary.tmp
set SUMMARY_FILE=%REP_DIR%\%SUMMARY_FILE_NAME%
set TMP_SUMMARY_FILE=%REP_DIR%\rep_summary.out

set SETUP_TIME_STAMP=%TIME:~0,-3%
set SETUP_TIME_STAMP=%SETUP_TIME_STAMP: =0%
set TIMESTAPM_FILE=%REP_DIR%\%DATE:.=-%_%SETUP_TIME_STAMP::=.%.lbl

rem --------------- Store previous report file name
set SUMMARY_FILE_BACKUP=%TMP_SUMMARY_FILE%.old
if exist "%TMP_SUMMARY_FILE%"  copy /Y "%TMP_SUMMARY_FILE%"  "%SUMMARY_FILE_BACKUP%"  1>nul 2>&1
if exist "%TMP_SUMMARY_FILE%"  del  "%TMP_SUMMARY_FILE%"
if exist "%SUMMARY_FILE%"  del  "%SUMMARY_FILE%"
if exist "%TMP_REP_FILE%"  del "%TMP_REP_FILE%"

echo.%SUMMARY_DESC%> "%TIMESTAPM_FILE%"

rem --------------- Process rep-files
pushd "%REP_DIR%"
for /D %%i in (*) do call :lbl_handle_test_group "%%i"

FOR %%i in (rep_*.txt) DO call :lbl_handle_rep_file "%%i"
popd

echo ======================================== >> "%TMP_SUMMARY_FILE%"
call "%~dp0\..\bin\ctr_utils\stats\GetStat.exe" -t "%TMP_REP_FILE%" >> "%TMP_SUMMARY_FILE%"

del /q  "%TMP_REP_FILE%"  
ren %TMP_SUMMARY_FILE% %SUMMARY_FILE_NAME%
goto :EOF


:lbl_handle_test_group
for %%i in (%~1\*.rep) DO call "%~dp0\..\bin\ctr_utils\stats\GetStat.exe" "%%i" >>"%TMP_REP_FILE%"
goto :EOF


:lbl_handle_rep_file
call head --lines=1 %1 >> "%TMP_SUMMARY_FILE%"
call tail --lines=8 %1 >> "%TMP_SUMMARY_FILE%"
echo. >> "%TMP_SUMMARY_FILE%"
goto :EOF 