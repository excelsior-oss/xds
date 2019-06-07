@echo off
rem ------------------------------------------------------------------------------
rem                          Excelsior Rats System Utility
rem                            (c) 2000, Excelsior Ltd.
rem
rem Module:       report
rem Author:       Lvov Konstantin
rem Modified By:  
rem Created:      15-Apr-2003
rem Purpose:      Analysis rats rep-files for given tests' group and  
rem               create summary report.
rem Main:         
rem Required:     Componet - GetStat
rem               Pakages  - cygwin-b20: grep, cat
rem ------------------------------------------------------------------------------

set IN_REP_DIR=%1
set OUT_REP_DIR=%~d1%~p1
set TEST_SET=%~n1

rem --------------- Create output file name
set REP_FILE=%IN_REP_DIR%_rep.out
set TOTAL_REP_FILE=%IN_REP_DIR%_total_rep.out
set FINAL_REP_FILE=%OUT_REP_DIR%\rep_%TEST_SET%.txt

rem --------------- Store previous report file name
if exist "%REP_FILE%"         del "%REP_FILE%"
if exist "%TOTAL_REP_FILE%"   del "%TOTAL_REP_FILE%"

set FINAL_REP_FILE_BACKUP=%FINAL_REP_FILE%.old
if exist "%FINAL_REP_FILE%"  copy /Y "%FINAL_REP_FILE%"  "%FINAL_REP_FILE_BACKUP%"  1>nul 2>&1
if exist "%FINAL_REP_FILE%"  del  "%FINAL_REP_FILE%"

rem --------------- Process rep-files
echo Make Report for %IN_REP_DIR%
echo Errors in '%TEST_SET%' tests > "%REP_FILE%"
FOR %%i in (%IN_REP_DIR%\*.rep) DO call "%~dp0\..\bin\ctr_utils\stats\GetStat.exe"  %%i >> "%REP_FILE%"

rem --------------- Make total reports
call "%~dp0\..\bin\ctr_utils\stats\GetStat.exe" -t %REP_FILE% > "%TOTAL_REP_FILE%"

rem --------------- Make final report
echo Whole Report for '%TEST_SET%' tests > "%FINAL_REP_FILE%"
echo %DATE%  %TIME:~0,-3% >> "%FINAL_REP_FILE%"
FOR %%i in (%IN_REP_DIR%\*.rep) DO call "%~dp0\..\bin\ctr_utils\stats\GetStat.exe" -e %%i >> "%FINAL_REP_FILE%"
echo ---------------------------------------- >> "%FINAL_REP_FILE%"
call cat %TOTAL_REP_FILE% >> "%FINAL_REP_FILE%"

del /q  "%REP_FILE%"  "%TOTAL_REP_FILE%"
 