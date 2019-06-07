@echo off
set REP_COUNT_FILE=%~dp0\..\bin\ctr_utils\rep_count.txt

if not exist %REP_COUNT_FILE%  echo 0 > %REP_COUNT_FILE%
for /F %%i in (%REP_COUNT_FILE%) do  set /A NEW_COUNT=%%i+1

set REP_COUNT=%NEW_COUNT%
if %NEW_COUNT% LSS 10    set REP_COUNT=0%REP_COUNT%
if %NEW_COUNT% LSS 100   set REP_COUNT=0%REP_COUNT%

echo %NEW_COUNT% > %REP_COUNT_FILE%
